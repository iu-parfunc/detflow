/**
 * libdet: Lightweight determinism runtime enforcement library.
 * This file should be compiled into a shared object libdet.so and LD_PRELOADED
 * to intercept clib calls to various functions that may cause nondeterminism if
 * not intercepted, e.g. date, time, open.
 */
#define _GNU_SOURCE

#include <stdio.h>
#include <stdlib.h>
#include <dlfcn.h>
#include <string.h>
#include <stdbool.h>
#include <unistd.h>
#include <sys/time.h>
#include <sys/wait.h>
#include <stdarg.h>
#include <sys/timex.h>
#include <sys/times.h>
#include <sys/stat.h>
#include <errno.h>
#include <math.h> // ceil.
#include <dirent.h>

// #ifdef __APPLE__
#include <pthread.h> // Mac OS.
// #endif


/* ===================================================================================== */

extern const char *__progname; // Saved by glibc.

/* We expect these two env variables to hold the permissions for our process. */
#define rPermsVariable "DETIO_R_PERMS_LIST"
#define wPermsVariable "DETIO_W_PERMS_LIST"
#define uRandomFile "DETIO_URAND_PATH"
// TODO
/* #define passwdFile "DETIO_PASSWD_PATH" */
#define debugVariable "DEBUG"

#define interEnvVarsSize 4
const char* interEnvVars[interEnvVarsSize] =
  { rPermsVariable, wPermsVariable, uRandomFile, debugVariable };

#define quitit() abort()
// #define quitit() exit(1)


/* Apparently the true max size is more complicated... Good enough? */
#define pathMax 4096

/* ===================================================================================== */
/* Options to pass to libDetLog() for importance of message. */
typedef enum {
  errorI,     /* This is a fatal error. */
  interI,     /* We intercepted this system call */
  infoI,      /* Less important information */
  extraI      /* Extra information not useful most of the time. */
} Importance;

/* ===================================================================================== */
/* Protoptypes for non wrapper functions in this file. */
static inline char* getEnvVar(const char* var, bool dieIfNotSet);
int comparePaths(const char* fullPath, char* perm);
void checkPermissions(const char* path, bool isWrite);
bool hasPermission(const char* path, bool isWrite);
void libDetLog(Importance imp, char* format, ...);
pid_t doForks(void);
bool isAbsolutePath(const char* path);
void cleanStat(struct stat* buf);
bool checkAgainstInternalVars(const char* name, int len, const char* envvarArr[len]);

/* Pid table functions. */
pid_t vPidToRPid(pid_t vPid);
pid_t rPidToVPid(pid_t rPid);
void addEntryAndUpdatePid(pid_t rPid);

/* Constructor, destructor. */
void initializeLib() __attribute__((constructor));
void takedownLib() __attribute__((destructor));

/* ==================================================================================== */
/**
 * Global function pointers pointing to the original versions of functions we
 * have overwritten keep cached here for efficieny! Variables initialized in
 * initializeLib().
 */
FILE* (*orgFopen)(const char*, const char*) = NULL;
int (*orgOpen)(char* filename, int access, int permission) = NULL;
int (*orgPthreadCreate)
  (pthread_t*, const pthread_attr_t*, void* (*)(void*), void*) = NULL;
pid_t (*orgFork)(void) = NULL;
pid_t (*orgGetpid)(void) = NULL;
pid_t (*orgWait)(int* status) = NULL;
pid_t (*orgWaitpid)(pid_t pid, int* status, int options) = NULL;
pid_t (*orgWaitid)(idtype_t idtype, id_t id, siginfo_t *infop, int options) = NULL;
int (*orgStat)(int ver, const char *path, struct stat *buf) = NULL;
int (*orgFstat)(int ver, int fd, struct stat *buf) = NULL;
int (*orgLstat)(int ver, const char *path, struct stat *buf) = NULL;
int (*orgSetenv)(const char*, const char*, int);

/**
 * Global variables holding enviornment variables used for permissions checking,
 * debug verbosity informations, library paths, etc. Variables initialzed in
 * initializeLib().
 */
/* Path to our /dev/urandom replacment file. */
char* urand = NULL;
/* char* passwd = NULL; */
/* Permissions for reading and writing to directory given to us by the environment.
   We expect a string of paths separated by colons, e.g. "/dev/:/etc/:/home/user/foo" */
char* rPerms = NULL;
char* wPerms = NULL;
/* Init to 1 in case it is called before we fetch DEBUG from env. */
char* debugLevel = NULL;

/**
 * In some OS versions we are having issues where initializeLib() is not getting called
 * for robustness every function checks whether the library has been initialized.
 */
bool libInit = false;

/* ===================================================================================== */
/**
 * Generic table used for mapping vitual values to real values. These functions may
 * be wrapped to specialize an instace, e.g. see rPidToVPid() and vPidToRPid() with,
 * addVPidAndUpate().

 * We use int64_t since ino_t uses unsigned longs.
 */

/* We statically allocate our tables 1000 entries. */
#define mapTableSize 1000
/* First given out vitual value. */
#define startingVirVal 2000
/* Hmmm, this is in danger of variable capture during the macro expansion... */
#define maxVirVal (startingVirVal + mapTableSize) - 1

/**
 * Given a virtual value, a table, it will convert to a real value doing bounds checks.
 * The mapping will fail if:
 * 1) vVal is less than our starting vitual value.
 * 2) vVal is bigger than the space in our table - starting vVal.
 * 3) No mapping exist for this value.
 */
int vValueToRValue(int vVal, int valTable[mapTableSize]){
  /* Too low value, would have accessed negative value of array. */
  if(vVal - startingVirVal < 0){
    libDetLog(errorI, "Error: vVal %d map attempted. vVals must be >= 2000.\n", vVal);
    abort();
  }
  /* Too high value, would have gone out of bounds... */
  if(vVal - startingVirVal - mapTableSize >= 0){
    libDetLog(errorI, "Error: vVal %d map attempted. VVals must be < 3000.\n", vVal);
    abort();
  }
  /* Ensure mapping existed. Our vValTable was initialized to 0. */
  pid_t rVal = valTable[vVal - startingVirVal];
  if(rVal == 0){
    libDetLog(errorI, "Error: vVal %d map attempted. No mapping exists for this vVal.\n",
              vVal);
    abort();
  }

  return rVal;
}

/**
 * Given a real value find the virtual value that maps to it.
 * This operation takes O(n), where n is the number of allocated entries in the table.
 * Rval is expected to be greater than zero.
 */
int rValueToVValue(int rVal, int freshVal, int valTable[mapTableSize]){
  pid_t vVal = 0;

  if(rVal <= 0){
    libDetLog(errorI, "Error: rVal %d map attempted. RVal must be > 0.\n", rVal);
    abort();
  }

  /* Iterate though existing table entries looking for existing mapping. */
  for(int i = 0; i < freshVal - startingVirVal; i++){
    int currentRVal = valTable[i];
    if(currentRVal == rVal){
      vVal = i + startingVirVal;
      break;
    }
  }
  /* No rVal found! */
  if(vVal == 0){
    libDetLog(errorI, "Error: rVal %d map attempted. No mapping.\n", vVal);
    abort();
  }

  return vVal;
}

/**
 * Given a new real value, the current fresh virtual value passed a pointer,
 * and the current table to update:
 * 1) Create a new mapping from freshVal -> rVal in valTable.
 * 2) Update the value of freshVal.
 */
void addEntryAndUpdate(int rVal, int* freshVal, int valTable[mapTableSize]){
  if(rVal <= 0){
    libDetLog(errorI, "Error: rVal %d map attempted. RVals should be > 0.\n", rVal);
    abort();
  }

  valTable[(*freshVal) - startingVirVal] = rVal;
  (*freshVal)++;

  return;
}


/* ===================================================================================== */
/**
 * Table of pid's mapping virtual pids (vPids) to real pids (rPid). To maintain
 * determinism return vPids to the user that are always deterministic. When the user
 * make calls to wait, getpid, and fork function we use this mapping to mantain
 * conistency.

 * This is not a true hash table but a simple array. Therefore we can access
 * pidTable[vPid - startingPid] to access the entry for vPid (if one exists).

 * You should not access these variables yourself. Instead use vPidToRPid() and
 * rPidToVPid() to access elements and addVPidAndUpate() to update table.
 */

/**
 * Table holding mappings from vitualPids to real Pids.
 * Init to all zeros to catch calls to vPidtoRPid() with valid vPids where no mapping
 * exits yet. First entry for current running process allocated in init();
*/
pid_t pidTable[mapTableSize] = { 0 };

/* Mutable variable used to keep track of next fresh vPid, you should not modify this
   variable. */
int freshVPid = startingVirVal;


/**
 * Convert given vPid to a rPid. Runs in constant time. This mapping may fail:
 * 1) The vPid may be less than our starting fresh pid.
 * 2) The vPid may be bigger than the space in our table - starting vPid.
 * 3) No mapping may exist for this pid.
 */
pid_t vPidToRPid(pid_t vPid){
  libDetLog(extraI, "vPidToRPid call for vPid=%d\n", vPid);
  int result = vValueToRValue((int)vPid, pidTable);
  libDetLog(extraI, "Successful mapped vPid: %d -> rPid: %d\n", vPid, result);

  return (pid_t)result;
}

/**
 * Map a given rPid to vPid. This operation takes O(n) where n is the number of
 * mappings in our table.
 */
pid_t rPidToVPid(pid_t rPid){
  libDetLog(extraI, "rPidToVPid call for rPid=%d\n", rPid);
  int result = rValueToVValue(rPid, freshVPid, pidTable);
  libDetLog(extraI, "Successful mapped rPid: %d -> vPid: %d\n", rPid, result);
  return (pid_t)result;
}


/**
 * Given a rPid create a new vPid -> rPid mapping and update freshVPid global.
 * Fails when rPid <= 0.
 */
void addEntryAndUpdatePid(pid_t rPid){
  libDetLog(extraI, "Adding new mapping vPid: %d -> rPid: %d to pidTable.\n",
            freshVPid, rPid);
  addEntryAndUpdate((int)rPid, &freshVPid, pidTable);
  return;
}
/* ===================================================================================== */
/**
 * Map inode values from stat functions to vitual inodes. Inodes are defined as
 * u_long types. Our table has elements of 4 bytes only. So we convert. This might
 * might cause some problems, but I don't believe inodes will ever get big enough to
 * be a problem.

 * Weirdly gcc does not complain about a conversion from ino_t to int without a cast.
 * sizeof(int) = 4, sizeof(ino_t) = 8. I can lose some data?! Gcc does not care about
 * my data...
 */
int inodeTable[mapTableSize] = { 0 };
/* Mutable variable keeping track of next fresh inode, don't modify this variable yourself! */
int freshVInode = startingVirVal;


/**
 * Convert given vInode to a vInode. Runs in constant time. This mapping may fail:
 * 1) The vInode may be less than our starting fresh inode.
 * 2) The vInode may be bigger than the space in our table - starting vInode.
 * 3) No mapping may exist for this inode.
 */
ino_t vInodeToRInode(int vInode){
  libDetLog(extraI, "vInodeToRInode call for vInode=%d\n", vInode);
  int result = vValueToRValue((int)vInode, inodeTable);
  libDetLog(extraI, "Successful mapped vInode: %d -> rInode: %d\n", vInode, result);

  return (ino_t)result;
}

/**
 * Map a given rInode to vInode. This operation takes O(n) where n is the number of
 * mappings in our table.
 */
ino_t rInodeToVInode(ino_t rInode){
  libDetLog(extraI, "rInodeToVInode call for rInode=%lu\n", rInode);
  int result = rValueToVValue((int)rInode, freshVInode, inodeTable);
  libDetLog(extraI, "Successful mapped rInode: %d -> vInode: %d\n", rInode, result);
  return (ino_t)result;
}


/**
 * Given a rInode create a new vInode -> rInode mapping and update freshVInode global.
 * Fails when rInode <= 0.
 */
void addEntryAndUpdateInode(ino_t rInode){
  libDetLog(extraI, "Adding new mapping vInode: %d -> rInode: %d to pidTable.\n",
            freshVInode, rInode);
  addEntryAndUpdate((int)rInode, &freshVInode, inodeTable);
  return;
}


/* ===================================================================================== */

/**
 * ASSUMPTION: these environment variables aren't mutated.  This
 * function is idempotent.
 */
void fetchEnvVars(){
  /* Fetch our environment variables. */
  urand  = getEnvVar(uRandomFile, true);
  /* passwd  = getEnvVar(passwdFile, true); */
  wPerms = getEnvVar(wPermsVariable, true);
  rPerms = getEnvVar(rPermsVariable, true);
  debugLevel = getEnvVar(debugVariable, false);

  /* DEBUG env variable not set, assume 1 (errors only). */
  if(debugLevel == NULL){
    debugLevel = (char*)malloc(sizeof(char) * 100);
    strcpy(debugLevel, "0");
    // RRN: I disagree, not setting DEBUG is just "normal operation":
    // fprintf(stderr, "Warning: DEBUG not set, assuming = 0\n");
  }
  return;
}

void fetchSymbols(){
  /* Get original clib functions. */
  orgFopen = dlsym(RTLD_NEXT, "fopen");
  orgOpen = dlsym(RTLD_NEXT, "open");
  orgPthreadCreate = dlsym(RTLD_NEXT, "pthread_create");
  orgGetpid = dlsym(RTLD_NEXT, "getpid");
  orgFork = dlsym(RTLD_NEXT, "fork");
  orgWait = dlsym(RTLD_NEXT, "wait");
  orgWaitpid = dlsym(RTLD_NEXT, "waitpid");
  orgWaitid = dlsym(RTLD_NEXT, "waitid");
  /* Actual functions exported by libdet have prefix __x. */
  orgStat = dlsym(RTLD_NEXT, "__xstat");
  orgFstat = dlsym(RTLD_NEXT, "__fxstat");
  orgLstat = dlsym(RTLD_NEXT, "__lxstat");
  orgSetenv = dlsym(RTLD_NEXT, "setenv");
  return;
}

/**
 * Initialization function. Called once when this library is linked at runtime.
 * 1) Prefetch pointers to all original libc functions for efficiency and keep them
 * as global variables.
 * 2) Fetch environment variables.
 * 3) Initialize deterministic PID table.
 */
void initializeLib(){
  /* Ignore call if library is already initialzed */
  if(libInit){
    libDetLog(infoI, "Warning: library is already initialized, attempt to initalize again ignored.\n");
    return;
  }

  libInit = true;
  fetchEnvVars();
  fetchSymbols();

  /* Add initial entry to pidTable for current process. */
  pid_t myRPid = (*orgGetpid)();
  addEntryAndUpdatePid(myRPid);

  /* No more timezone information. */
  tzname[0] = "EST";
  putenv("TZ=America/New_York");

  libDetLog(infoI, "libdet.so has been loaded. Library initialized.\n");
  return;
}


/**
 * We are done. Free memory.
 */
void takedownLib(){
  libDetLog(infoI, "libdet.so done. Freeing resources.!\n");
  free(urand);
  /* free(passwd); */
  free(wPerms);
  free(rPerms);
  free(debugLevel);
  return;
}

/* ===================================================================================== */
/**
 * Glibc functions wrapped by our library:
 * These functions may be called in any order with no gurantee that the global state
 * has been initialized. Therefore every wrapper functions should call:
*  if(! libInit){ initializeLib(); }
 * to ensure the global state is initialized.
 */


/**
 * Permission-filtered wrapper around fopen.
 */
FILE* fopen(const char *path, const char *mode) {
  if(! libInit){ initializeLib(); }
  libDetLog(interI, "Intercepted call to \"fopen\", opening %s, mode %s\n", path, mode);

  checkPermissions(path, mode[0] == 'r' ? false : true); // glibc allows modes like "rme"

  /* Capture call to /dev/urandom */
  if(0 == strcmp(path, "/dev/urandom")){
    libDetLog(infoI, "Intercepted call to /dev/urandom. Opening our file instead.\n");

    FILE* openFd = (*orgFopen)(urand, mode);
    if(openFd == NULL){
      libDetLog(errorI, "Error: Failed to open our version of urandom: %s\n", urand);
      quitit();
    }
    return openFd;
  }
  else if(0 == strcmp(path, "/etc/localtime")){
    libDetLog(infoI, "Intercepted call to /etc/localtime. Opening our file instead.\n");
    FILE* openFd = (*orgFopen)(urand, mode);
    if(openFd == NULL){
      libDetLog(errorI, "Error: Failed to open our version of localtime: %s\n", urand);
      quitit();
    }

    return openFd;
  }else if(0 == strcmp(path, "/etc/passwd")){
    libDetLog(infoI, "Intercepted call to /etc/passwd. Opening our file instead.\n");
    FILE* openFd = (*orgFopen)(urand, mode);
    if(openFd == NULL){
      libDetLog(errorI, "Error: Failed to open our version of /etc/passwd: %s\n", urand);
      quitit();
    }

    return openFd;
  }

  return (*orgFopen)(path, mode);
}

uid_t getuid(void){
  libDetLog(interI, "Intercepted call to \"getuid\"\n");
  return 1000;
}


uid_t geteuid(void){
  libDetLog(interI, "Intercepted call to \"geteuid\"\n");
  return 1000;
}


/**
 * Wrapper for open, same logic as fopen.
 */
int open(char* filename, int access, int permission){
  if(! libInit){ initializeLib(); }
  libDetLog(interI, "Captured call to \"open\". Opening %s\n", filename);
  /* Get original open function. */

  // FIXME FIXME FIXME:
  /* The permissions for open are quite complicated and don't map nicely to our
     permissions so we shall ignore them for now! */
  checkPermissions(filename, false); // Umm... just pretend this is read?

  /* Capture call to /dev/urandom */
  if(0 == strcmp(filename, "/dev/urandom")){
    libDetLog(infoI, "Intercepted call to /dev/urandom. Opening our file instead.\n");

    int openFd = (*orgOpen)(urand, access, permission);
    if(openFd < 0){
      libDetLog(errorI, "Error: Failed to open our version of urandom: %s\n", urand);
      quitit();
    }

    return openFd;
  }
  /* If call to localtime. */
  if(0 == strcmp(filename, "/etc/localtime")){
    libDetLog(infoI, "Intercepted call to /etc/localtime. Opening our file instead.\n");

    int openFd = (*orgOpen)(urand, access, permission);
    if(openFd < 0){
      libDetLog(errorI, "Error: Failed to open our version of localtime: %s\n", urand);
      quitit();
    }

    return openFd;
  }
  else{
    return (*orgOpen)(filename, access, permission);
  }
}

// What about getdents? Should have to open a directory before listing though...
// Get dents is a direct system call we can't directly intercept. Instead we
// wrap the libc wrapper: readdir and readdir_r.
// Return null as pretending we are at the end of the directory stream.
struct dirent* readdir(DIR* dirp){
  return NULL;

}

// Similar to readdir, instead uses result. Returns 0 for success when
// we hit the end of the function stream. Pretend we are at the end.
int readdir_r(DIR *dirp, struct dirent *entry, struct dirent **result){
  (*result) = NULL;
  return 0;
}


// RRN, NOTE: What about mmap64??
// OSNL: For now it seems like turning off ASLR suffices. We will come back if we
// need to.
// from x86_64-linux-gnu/sys/mman.h:
/* void *mmap (void *__addr, size_t __len, int __prot, */
/*               int __flags, int __fd, __off64_t __offset) */
/* // __THROW */
/* { */
/*   printf("FINSHME: mmap wrapper\n"); */
/*   abort(); */
/* } */

/* void *mmap64 (void *__addr, size_t __len, int __prot, */
/*               int __flags, int __fd, __off64_t __offset) */
/* // __THROW */
/* { */
/*   printf("FINSHME: mmap wrapper\n"); */
/*   abort(); */
/* } */
// int munmap(void *addr, size_t length)
/*
 * The following functions handle the environment. We share the memory space with the
 * program we are tracking. Therefore we must not allow the user to observe or change
 * the environment. We do this by wrapping setenv, unsetenv, and getenv.
 */

/**
 * Check if name is matches any of our internal variables used by libdet.
 * @return: true if they do, false otherwise.
 */
bool checkAgainstInternalVars(const char* name, int len, const char* envvarArr[len]){
  // Iterate through our envvar and make sure user doesn't change them.
  for(int i = 0; i < len; i++){
    // Trying to change our variable return.
    if(strcmp(name, envvarArr[i]) == 0){
      return true;
     }
  }

  //We are good return true.
  return false;
}

/**
 * Disallow user from overwriting our enviornment variables.
 */
int setenv(const char *name, const char *value, int overwrite){
  if(! libInit){ initializeLib(); }
  libDetLog(interI, "Intercepted call to \"setenv\".\n");

  if(checkAgainstInternalVars(name, interEnvVarsSize, interEnvVars)){
      libDetLog(infoI, "Tried to modify our environment variable %s\n", name);
      return 0;
  }

  return (*orgSetenv)(name, value, overwrite);
}

/**
 * Disallow user from unseting enviornment variables.
 */
int unsetenv(const char *name){
  if(! libInit){ initializeLib(); }
  libDetLog(interI, "Intercepted call to \"unsetenv\".\n");

  if(checkAgainstInternalVars(name, interEnvVarsSize, interEnvVars)){
      libDetLog(infoI, "Tried to unset our environment variable %s\n", name);
      return 0;
  }

  int (*unsetenv)(const char*) = dlsym(RTLD_NEXT, "unsetenv");
  return (*unsetenv)(name);
}

char* getenv(const char *name){
  if(! libInit){ initializeLib(); }
  libDetLog(interI, "Intercepted call to \"getenv\".\n");

  if(checkAgainstInternalVars(name,  interEnvVarsSize, interEnvVars)){
    libDetLog(infoI, "Tried to read our environment variable %s\n", name);
    return NULL;
  }

  return getEnvVar(name, false);
}

char *secure_getenv(const char *name){
  if(! libInit){ initializeLib(); }
  libDetLog(interI, "Intercepted call to \"secure_getenv\".\n");

  if(checkAgainstInternalVars(name,  interEnvVarsSize, interEnvVars)){
    libDetLog(infoI, "Tried to read our environment variable %s\n", name);
    return NULL;
  }

  return getEnvVar(name, false);
}


/**
 * Overwriting time functions to always return the same time. Is there a better
 * to do this?
 * Based on description of:
 * https://linux.die.net/man/3/clock
 */
clock_t clock (void){
  if(! libInit){ initializeLib(); }
  libDetLog(interI, "Intercepted call to \"clock\".\n");
  /* Apparently clock_t is represented as a long int. */
  return 0;
}


/**
 * Values set based on description of
 * http://man7.org/linux/man-pages/man3/ntp_gettime.3.html
*/
int ntp_gettime (struct ntptimeval *tptr){
  if(! libInit){ initializeLib(); }
  libDetLog(interI, "Intercepted call to \"ntp_gettime\".\n");
  if(tptr != NULL){
    tptr->esterror = 0;
    tptr->maxerror = 0;
    tptr->time.tv_sec = 0;
    tptr->time.tv_usec = 0;
    tptr->tai = 0;
  }

  return TIME_OK;
}

/**
 * Processor times.
 * https://linux.die.net/man/2/times
 */
clock_t times (struct tms* buffer){
  if(! libInit){ initializeLib(); }
  libDetLog(interI, "Intercepted call to \"times\".\n");
  if(buffer != NULL){
    buffer->tms_utime = 0;
    buffer->tms_cstime = 0;
    buffer->tms_cutime = 0;
    buffer->tms_stime = 0;
  }

  return 1;
}

/**
 * http://man7.org/linux/man-pages/man3/clock_getcpuclockid.3.html
 */
int clock_getcpuclockid (pid_t __pid, clockid_t* __clock_id){
  if(! libInit){ initializeLib(); }
  libDetLog(interI, "Intercepted call to \"clock_getcpuclockid\".\n");
  if(__clock_id != NULL){
    *__clock_id = 1;
  }

  return 0;
}


/**
 * Time returns the current time elapsed since January 1st 1970.
 * time_t is implemented as an arithmetic number. Needed for srand.
 * http://man7.org/linux/man-pages/man2/time.2.html
 */
time_t time(time_t *result){
  if(! libInit){ initializeLib(); }
  libDetLog(interI, "Intercepted call to \"time\".\n");
  return 0;
}

/**
 * Struct set beginning of Unix time :O
 * https://linux.die.net/man/3/clock_gettime
 */
int clock_gettime(clockid_t clk_id, struct timespec *tp){
  if(! libInit){ initializeLib(); }
  libDetLog(interI, "Intercepted call to \"clock_gettime\".\n");
  if(tp == NULL)
    return -1;
  else{
    tp->tv_nsec = 0;
    tp->tv_sec  = 0;
  }

  return 0;
}


/**
 * I can't find information on this one.
*/
void gettime(struct timespec* ts){
  if(! libInit){ initializeLib(); }
  libDetLog(interI, "Intercepted call to \"gettime\".\n");
  if(ts != NULL){
    ts->tv_sec = 0;
    ts->tv_nsec = 0;
  }
  return;
}

/**
 * Either of the fields may be NULL. If so we do not populate them.
 * https://linux.die.net/man/2/gettimeofday
 */
#ifdef __APPLE__
int gettimeofday(struct timeval * __restrict tv, void* __restrict tz)
#else
int gettimeofday(struct timeval *tv, struct timezone *tz)
#endif
{
  if(! libInit){ initializeLib(); }
  libDetLog(interI, "Intercepted call to \"gettimeofday\".\n");
  if (tv != NULL) {
    tv->tv_sec = 0;
    tv->tv_usec = 0;
  }
  struct timezone * tz2 = (struct timezone *)tz;
  if(tz2 != NULL){
    tz2->tz_minuteswest = 0;
    tz2->tz_dsttime = 0;
  }

  return 0;
}

/**
 * Return the virtual pid mapped to this process.
 */
pid_t getpid(void){
  if(! libInit){ initializeLib(); }
  libDetLog(interI, "Intercepted call to \"getpid\".\n");
  pid_t rPid = (*orgGetpid)();
  return rPidToVPid(rPid);
}


pid_t fork(void){
  if(! libInit){ initializeLib(); }
  libDetLog(interI, "Intercepting fork...\n");
  return doForks();
}


pid_t vfork(void){
  if(! libInit){ initializeLib(); }
  libDetLog(interI, "Intercepted vfork()\n");
  return doForks();
}

/*
 * In general is not really possible to wrap a variadic function which will then call
 * another variadic function. Therefore we leave these functions unimplemented for now...
 */
/* int execl(const char *path, const char *arg, ... /\* (char  *) NULL *\/){ */

/* } */
/* int execlp(const char *file, const char *arg, ... /\* (char  *) NULL *\/){ */

/* } */
/* int execle(const char *path, const char *arg, ... /\*, (char *) NULL, char * const envp[] *\/){ */

/* } */
int execv(const char *path, char *const argv[]){
  if(! libInit){ initializeLib(); }
  libDetLog(interI, "Intercepting execv: %s\n", path);

  checkPermissions(path, false);

  int (*orgExecv)(const char *, char *const []);
  orgExecv = dlsym(RTLD_NEXT, "execv");

  return (*orgExecv)(path, argv);
}

/*
 * Execvp and expevpe use the shell to search through PATH, to see if they might
 * find the file in one of the given directories.
 * This is back to the problem of finding the full path meant for a specific file.
 * I don't want to disallow working programs this late in game.
 * TODO: Properly code these two functions.
 */
/* int execvp(const char *file, char *const argv[]){ */

/* } */

/* int execvpe(const char *file, char *const argv[], char *const envp[]){ */

/* } */

/**
 * Programs automatically inherent the enviornment of their parents. Therefore when called
 * through exec we do not need to redo the all to LD_PRELOAD.
 * We assume a non-adversarial program. Otherwise, notice the user could easily
 * overwrite our enviornment variables to give themselves permissions for everything.
 */
int execve(const char *filename, char *const argv[],
           char *const envp[]) {
  if(! libInit){ initializeLib(); }
  libDetLog(interI, "Intercepting execve: %s\n", filename);

  checkPermissions(filename, false);

  int (*orgExec)(const char *, char *const [], char *const []);
  orgExec = dlsym(RTLD_NEXT, "execve");

  return (*orgExec)(filename,argv,envp);
}

/**
 * Wait for child to finish. Return a vPid instead and regular status.
 */
pid_t wait(int* status){
  if(! libInit){ initializeLib(); }
  libDetLog(interI, "Intercepted call to \"wait\".\n");

  pid_t rPid = (*orgWait)(status);

  if(rPid != -1){
    return rPidToVPid(rPid);
  }
  /* Error, propage error up! */
  return -1;
}

pid_t wait3(int *status, int options, struct rusage *rusage){
  libDetLog(errorI, "Intercepted call to \"wait3\".\n");
  return waitpid(-1, status, options);
}

pid_t wait4(pid_t pid, int *status, int options, struct rusage *rusage){
  libDetLog(errorI, "Intercepted call to \"wait4\".\n");
  return waitpid(pid, status, options);
}



/**
 * Wait for pid given by first argument. Our wrapper handles the logic for mapping rPids
 * to rPid and vice versa.
 */
pid_t waitpid(pid_t pid, int* status, int option){
  if(! libInit){ initializeLib(); }
  libDetLog(interI, "Intercepted call to \"waitpid\".\n");

  /* Map pid (a vPid) to a rPid based on the expected behavior of waitpid:
     https://linux.die.net/man/2/wait */
  int rPid = 0;

  /* Wait for any child process who: process group ID == abs(pid). */
  if(pid < -1){
    /* Negate for mapping to realPid and negate result for orgwaitpid. */
    rPid = -vPidToRPid(-pid);
  }
  /* Wait for any child process. No mapping needed. */
  else if(pid == -1){
    rPid = -1;
  }
  /* Wait for any child process whose process group ID == calling process groupId. */
  else if(pid == 0){
    rPid = 0;
  }
  /* Pid was > 0, wait for: child pid ==  pid. */
  else{
    rPid = vPidToRPid(pid);
  }

  pid_t result = (*orgWaitpid)(rPid, status, option);

  /* Success! Map back to virtual pid. */
  if(result > 0){
    return rPidToVPid(result);
  }else{
    /* Waitpid failed. Propagate return value. */
    return result;
  }
}

/**
 * Wrapper to waitid taking care of mapping vPids.
 */
int waitid(idtype_t idtype, id_t id, siginfo_t *infop, int options){
  if(! libInit){ initializeLib(); }
  libDetLog(interI, "Intercepted call to \"waitid\" for id %d.\n", id);
  int result = 0;
  pid_t vPid = (pid_t)id;
  id_t rPid = 0;

  /* Map vPid to rPid. */
  if(idtype == P_PID){
    /* The user may give us some garbage id here, if they keep it do not try
       to map it as it will error out. Otherwise map. */
    rPid = (vPid <= 0 || vPid - maxVirVal < 0) ? vPid : (id_t)vPidToRPid((pid_t)id);
    result = (*orgWaitid)(idtype, rPid, infop, options);
  }
  else{
    result = (*orgWaitid)(idtype, id, infop, options);
  }

  /* Success, replace field in struct with our vPid. */
  if(result == 0){
    pid_t childRPid = infop->si_pid;
    infop->si_pid = rPidToVPid(childRPid);
  }

  return result;
}

/* There are the actual functions exported by libc for these functions.
   stat is just an alias.
 */
int __xstat(int ver, const char *path, struct stat *buf){
  if(! libInit){ initializeLib(); }
  libDetLog(interI, "Intercepted call to \"stat\".\n");
  bool hasRead  = hasPermission(path, false);
  bool hasWrite = hasPermission(path, true);
  int result;

  if (hasRead){
    result = (*orgStat)(ver, path, buf);
    /* Only update struct when we suceed. Otherwise leave garbage on buf alone. */
    if(result != -1){
      cleanStat(buf);
    }
  }
  else {
    result = 0; // Should this count as success or failure?
    buf->st_size = 0;
  }

  buf->st_mode |= (hasRead  ? S_IRUSR : 0) |
                  (hasWrite ? S_IWUSR : 0);
  // TODO: determine whether this should be a global failure:
  // if (result) quitit();
  // TODO: CLEAN errno
  return result;
}


/**
 * fstat is easy. We have a file descriptor so we know we have permissions
 * to read this file.
 */
int __fxstat(int ver, int fd, struct stat *buf){
  if(! libInit){ initializeLib(); }
  libDetLog(interI, "Intercepted call to \"fstat\".\n");
  // Here we can already access it, because we have it open!
  int result = (*orgFstat)(ver, fd, buf);

  /* Only update struct when we suceed. Otherwise leave garbage on buf alone. */
  if(result != -1){
    cleanStat(buf);
    buf->st_mode |= S_IXUSR | S_IRUSR | S_IWUSR;
  }
  // TODO: determine whether this should be a global failure:
  // if (result) quitit();
  // TODO: CLEAN errno
  return result;
}


int __lxstat(int ver, const char *path, struct stat *buf){
  if(! libInit){ initializeLib(); }
  libDetLog(interI, "Intercepted call to \"lstat\".\n");
  bool hasRead  = hasPermission(path, false);
  bool hasWrite = hasPermission(path, true);
  int result;
  if (hasRead){
    result = (*orgLstat)(ver, path, buf);
    /* Only update struct when we suceed. Otherwise leave garbage on buf alone. */
    if(result != -1){
      cleanStat(buf);
    }
  }
  else {
    result = 0; // Should this count as success or failure?
    buf->st_size = 0;
  }

  buf->st_mode |= (hasRead  ? S_IRUSR : 0) |
                  (hasWrite ? S_IWUSR : 0);
  // TODO: determine whether this should be a global failure:
  // if (result) quitit();
  // TODO: CLEAN errno
  return result;
}

/**
 * Wrapper for creating threads. We force the child to run and finish and join with
 * the parent before continuing. Notice this behavior may lead to deadlocks for
 * some programs.
 */
int pthread_create (pthread_t *__restrict newthread,
                    const pthread_attr_t *__restrict attr,
                    void *(* start_routine) (void *),
                    void *__restrict arg)
// __THROWNL __nonnull ((1, 3));
{
  if(! libInit){ initializeLib(); }
  // libDetLog(interI, "ERROR: Caught call to pthread_create.  UNFINISHED!\n");  quitit();

  // ---------------- Inline into current thread --------------------  
  // (*start_routine)(arg);  // Won't work in general, of course.

  // ---------------- Sequentializing verison --------------------
  libDetLog(interI, "Caught call to pthread_create.  Sequentializing.\n");
  int f = (*orgPthreadCreate)(newthread, attr, start_routine, arg);
  if (f) return f;
  else {
    // while(1) if (! pthread_tryjoin_np(*newthread, NULL)) break; 
    pthread_join(*newthread, NULL);

    libDetLog(infoI, "pthread_create joined child %d before continuing.\n", *newthread);
    return 0;
  }

}

void tzset (void){
  if(! libInit){ initializeLib(); }
  libDetLog(interI, "Intercepted call to \"tzset\".\n");
  putenv("TZ=\"\"");
}

int getlogin_r(char *buf, size_t bufsize){
  if(! libInit){ initializeLib(); }
  libDetLog(interI, "Intercepted call to \"getlogin_r\".\n");

  memset((void*)buf, 0, bufsize);
  return 0;
}
// TODO: execveat

// TODO: fexecve
// TODO: system

/* ===================================================================================== */
/**
 * Start of auxillary funtions for our runtime.
 * Notive these functions do not check to see if initalizeTrue is true. We expect the clib
 * wrapper functions that calls this function to have done that.
 */


/**
 * Zere most fields in the stat struct, which consists almost entirely
 * of nondeterministic information from our perspective.
 */
void cleanStat(struct stat* buf){
  buf->st_dev = 0;
  /* Add new entry to our inodeTable for this value and set struct to virtual value. */
  ino_t ino = buf->st_ino;
  addEntryAndUpdateInode(ino);
  buf->st_ino = rInodeToVInode(ino);

  // buf->st_mode = 0;
  buf->st_mode |= S_IXUSR;  // Only retain one bit!
  buf->st_nlink = 0;
  buf->st_uid = 0;
  buf->st_gid = 0;
  buf->st_rdev = 1;
  // Don't zero size!  That's legit.
  //  buf->st_size = 1;
  buf->st_blksize = 4096;
  buf->st_blocks = (blkcnt_t)ceil((double)buf->st_size / (double)buf->st_blksize);
  buf->st_atim.tv_nsec = 0;
  buf->st_atim.tv_sec = 0;
  buf->st_mtim.tv_nsec = 0;
  buf->st_mtim.tv_sec = 0;
  buf->st_ctim.tv_nsec = 0;
  buf->st_ctim.tv_sec = 0;
  return;
}


// TODO: In the future we may replace me with something more general:
bool isAbsolutePath(const char* path) {
  return (path[0] == '/');
}


/**
 * Given a path and whether the acess is read only or read and write
 * check our environment variables to see if a process has acess to
 * those variables.

 * Realpath() may fail if the file does not yet exists or some garbage path
 * is given. Instead we will assume we are givem full paths.
 */
void checkPermissions(const char* path, bool isWrite) {
  /* Exit if we do not have permissions. */
  if(! hasPermission(path, isWrite) ) {
    libDetLog(errorI, "Error: No permission for \"%s\"\n", path);
    quitit();
  }
  return;
}

/**
 * Predicate function to check permission WITHOUT throwing an error.  
 * (Unless an internal error occurs.)
 */
bool hasPermission(const char* path, bool isWrite) {
  bool hasPerm = false;
  char absolutePath[pathMax];

  if (wPerms == NULL) fetchEnvVars();
  char* correctPerms = isWrite ? wPerms : rPerms;
  if (!correctPerms) correctPerms = "";

  /* Strtok modifies string, copy to separte variable. */
  char* permsToCheck = (char*)malloc(sizeof(char) * strlen(correctPerms) + 1);
  char* scratch = (char*)malloc(sizeof(char) * strlen(correctPerms) + 1000);
  /* Pointer to original location for freeing. Needed as strtok_r will mutate the scratch
     pointer to a different place. */
  char* p = scratch;
  strcpy(permsToCheck, correctPerms);

  libDetLog(infoI, "Current %s Permissions: %s\n", isWrite ? "W" : "R", correctPerms);

  /* Look for absolute path. If none found use regular path instead... */
  // RRN: Disabling realpath for now.  /proc/self/maps is a symlink. See #2
  // char* status = (realpath(path, absolutePath));
  // if(status == NULL)
  {
    if(isAbsolutePath(path)) {
      libDetLog(infoI, "Realpath returned NULL!  Nonexistent absolute path.\n");
      strcpy(absolutePath, path);
    } else {
      char cwd[pathMax];
      char* status2 = getcwd(cwd,pathMax);
      if (!status2) {
        libDetLog(errorI, "Error: getcwd failed\n");
        quitit();
      }
      sprintf(absolutePath, "%s/%s", cwd, path);
      // TODO: could create an absolute path
      libDetLog(infoI, "Hackishly assembling absolute path...\n");
    }
  }

  libDetLog(infoI, "Using path \"%s\" for permission lookup (originally %s).\n",
            absolutePath, path);
  /*Iterate through tokenizer checking if our path is under the
    current path. */
  char* token = strtok_r(permsToCheck, ":", &scratch);
  while(token){
    if(comparePaths(absolutePath, token) == 0){
      hasPerm = true;
      break;
    }
    token = strtok_r(NULL, ":", &scratch);
  }

  free(permsToCheck);
  free(p);
  return hasPerm;
}


/**
 * Given the full path and a permision, compare the two strings
 * to see if perm is a prefix of fullpath. This will let us know
 *  whether the user actually has permissions to read/write to this file.

 * Iterate throught the chars in perm to see if they match fullPath,
 * if we run out of chars we know the permissions work. Example:
 * fullPath = /home/dir2/cir2/omar.txt

 * if perm is:
 *   /home/dir2/      --accept!
 *   /home/dir2/cir2/ --accept!
 *   /home/           --accept!
 *   /home/dir2/cir3/ --reject!
 *   /home/dir1/      --reject!
 */
int comparePaths(const char* fullPath, char* perm){
  size_t l = strlen(perm);
  size_t fullL = strlen(fullPath);

  for(unsigned int i = 0; i < l; i++){
    /* We have reached the end of fullPath this cannot be the permission for it.
       or strings don't match. */
    if(fullL <= i || perm[i] != fullPath[i])
      return 1;
  }
  /* Strings match return 0 (true by C string convention). */
  return 0;
}

/**
 * Wrapper for printf. Decides wether to print based on debug level.
 * imp: The importance of the message.
 * format: message to print.
 * ... : arguments to format string.

 * Uses debugLevel global.
 * Currently:
 * Level 5: Print all.
 * Level 4: Print information, errors, and intercepted calls.
 * Level 2, 3: Print errors and intercepted calls.
 * Level 1   : Print only errors.

 * Note 4, 5 and 2, 3 work the same. This may change in the future.
 */
void libDetLog(Importance imp, char* format, ...) {
  va_list args;
  /* Assumme it won't fail... */
  if(! libInit){ initializeLib(); }

  int debugNum = strtol(debugLevel, NULL, 10);
  bool print = false;

  /* Unknown importance level. */
  if(imp != errorI && imp != interI && imp != infoI && imp != extraI){
    fprintf(stderr, "  [libdet] Warning: Unknown importance level.\n");
    print = true;
  }

  /* Print information based on debug level. */
  switch(debugNum){
    /* Most verbose, print all messages. */
  case 5:
    print = true;
  case 4:
    if(imp == errorI || imp == interI || imp == extraI){
      print = true;
    }
    break;
    /* Ignore informatory messages. */
  case 3:
  case 2:
    if(imp == errorI || imp == interI){
      print = true;
    }
    break;
  case 1:
    if(imp == errorI){
      print = true;
    }
    break;
  case 0:
    if(imp == errorI){
      print = true;
    }
    break;
  default:
    fprintf(stderr, "  [libdet] Warning Unknown DEBUG level %d.\n", debugNum);
    break;
  }

  if(print){
    fprintf(stderr, "  [libdet %d %s] ", orgGetpid ? (*orgGetpid)() : 0, __progname);
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);
  }

  return;
}



// RRN, NOTE: What about mmap64??

// OSNL: For now it seems like turning off ASLR suffices. We will come back if we
// need to.
// from x86_64-linux-gnu/sys/mman.h:
/* void *mmap (void *__addr, size_t __len, int __prot, */
/*               int __flags, int __fd, __off64_t __offset) */
/* // __THROW */
/* { */
/*   printf("FINSHME: mmap wrapper\n"); */
/*   quitit(); */
/* } */

/* void *mmap64 (void *__addr, size_t __len, int __prot, */
/*               int __flags, int __fd, __off64_t __offset) */
/* // __THROW */
/* { */
/*   printf("FINSHME: mmap wrapper\n"); */
/*   quitit(); */
/* } */



// int munmap(void *addr, size_t length)


/**
 * Get env variable copy to free space and return as a heap-allocated pointer.
 */
static inline char* getEnvVar(const char* var, bool dieIfNotSet){
  // To avoid a recursive loop, we get the value ourselves.
  char* (*orgGetenv)(const char*) = dlsym(RTLD_NEXT, "getenv");
  char* tempResult = (*orgGetenv)(var);

  if(tempResult == NULL && dieIfNotSet){
    /* Do not make this a call to libDetLog, we need to fetch the env for DEBUG before
       we make a call to it. */
    fprintf(stderr, "  [libdet] Error: Environment variable \"%s\" does not exist.\n", var);
    quitit();
  }
  if(tempResult == NULL){
    return NULL;
  }
  char* returnVar = (char*) malloc(sizeof(char) * strlen(tempResult) + 1);
  strcpy(returnVar, tempResult);
  return returnVar;
}


pid_t realGetpid(void){
  return (*orgGetpid)();
}


/**
 * This function is called by both fork() and vfork(). For our purposes
 * they do the same thing.
 * We return a vPid to the user. Note both the parent and the child allocate an entry in
 * the pidTable for the child's pid as their memory space is no longer shared. This means
 * that any pid mappings created by the child are never seen by the parent. Therefore it
 * seems like a parent might lose information necessary to wait for a grandchild. However
 * some googling shows this process is unable to find information about it's grandchild:
 * http://stackoverflow.com/questions/6228864/wait-for-and-or-kill-process-grandchildren-produced-by-fork
 * http://stackoverflow.com/questions/12822611/fork-and-wait-how-to-wait-for-all-grandchildren-to-finish
 *
 */
pid_t doForks(void){
  siginfo_t sigInfo;
  pid_t forkResult = (*orgFork)();

  if(forkResult == -1){
    libDetLog(errorI, "Error, call to fork failed.\n");
    quitit();
  }
  /* This is the child's process. */
  if(forkResult == 0){
    libDetLog(infoI, "Entered child process\n");

    /* Create a new entry for us in our pidTable (parent does not see this!). */
    addEntryAndUpdatePid((*orgGetpid)());
    return 0;
  }

  /* Parent starts here! Should hold PID of child! Save this mapping to our pidTable. */
  addEntryAndUpdatePid(forkResult);

  /* We want to wait for child to finish. waitpid() and wait() have side effects
     as they will clear the process we are waiting on from the process table. Instead
     we use a variant that leaves the child process. This is necessary as the user
     may call wait or waitpid themselves. */

  /* Create a new mapping for our virtual pids for new child. */

  /* Waitid failed... */
  if(0 != (*orgWaitid)(P_PID, forkResult, &sigInfo, WEXITED | WNOWAIT)){
    libDetLog(errorI, "Error: Unable to waitid() on child!\n");
    switch(errno){
    case ECHILD:
      libDetLog(errorI,
        "The calling process has no existing unwaited-for child processes!\n");
      break;
    case EINTR:
      libDetLog(errorI,
        "The waitid() function was interrupted by a signal!\n");
      break;
    case EINVAL:
      libDetLog(errorI,
        "An invalid value was specified for options,"
        " or idtype and id specify an invalid set of processes.\n");
      break;
    default:
      libDetLog(errorI,
        "I don't know why waitid() failed. This should be impossible!\n");
      break;
    }
    quitit();
  }else{
    /* Figure out if we exited succesfully! */
    if(sigInfo.si_code == CLD_EXITED){
      /* Sucessful exit! */
      if(0 != sigInfo.si_status){
        libDetLog(errorI, "Error: Child process %d exited with nonzero code!: %d\n",
                  forkResult, sigInfo.si_status);
        quitit();
      }
    }else{
      libDetLog(errorI,
          "Error: Child process %d did not return exit code (Probably killed by signal)!\n",
                forkResult);
      quitit();
    }
  }
  libDetLog(infoI, "Bottom of fork()\n");
  return rPidToVPid(forkResult);
}


/* ===================================================================================== */

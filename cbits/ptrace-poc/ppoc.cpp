#include <sys/ptrace.h>
#include <sys/reg.h>     /* For constants ORIG_EAX, etc */
#include <sys/syscall.h>    /* For SYS_write, etc */
#include <sys/types.h>
#include <sys/user.h>
#include <sys/vfs.h>
#include <sys/wait.h>
#include <unistd.h>
#include <signal.h>
#include <string.h>

#include <stdint.h>
#include <cstdlib>
#include <stdio.h>
#include <cstdio> // for perror
#include <cstring> // for strlen
#include <sys/stat.h>
#include <fcntl.h>

#include <iostream>

#include <stdarg.h> //variadic args.

#define STRINGIZE(x) STRINGIZE2(x)
#define STRINGIZE2(x) #x
#define __LINE_STRING__ STRINGIZE(__LINE__)

// error check macro c/o http://stackoverflow.com/questions/6932401/elegant-error-checking
#define CHECK(x) do { \
  int retval = (x); \
   if (retval != 0) { \
    perror(#x " " __FILE__ ":" __LINE_STRING__); \
    exit(EXIT_FAILURE); \
   } \
  } while (0)

#define Swap4Bytes(val) \
  ( (((val) >> 24) & 0x000000FF) | (((val) >>  8) & 0x0000FF00) | \
    (((val) <<  8) & 0x00FF0000) | (((val) << 24) & 0xFF000000) )

#define Swap8Bytes(val) \
 ( (((val) >> 56) & 0x00000000000000FF) | (((val) >> 40) & 0x000000000000FF00) | \
   (((val) >> 24) & 0x0000000000FF0000) | (((val) >>  8) & 0x00000000FF000000) | \
   (((val) <<  8) & 0x000000FF00000000) | (((val) << 24) & 0x0000FF0000000000) | \
   (((val) << 40) & 0x00FF000000000000) | (((val) << 56) & 0xFF00000000000000) )

// Stolen from libdet.c...
extern const char *__progname; // Saved by glibc.

/* Options to pass to PtraceDetLog() for importance of message. */
typedef enum {
  errorI,     /* This is a fatal error. */
  interI,     /* We intercepted this system call */
  infoI,      /* Less important information */
  extraI      /* Extra information not useful most of the time. */
} Importance;


/* We expect these two env variables to hold the permissions for our process. */
#define rPermsVariable "DETIO_R_PERMS_LIST"
#define wPermsVariable "DETIO_W_PERMS_LIST"
#define uRandomFile "DETIO_URAND_PATH"
#define debugVariable "DEBUG"

/* Apparently the true max size is more complicated... Good enough? */
#define pathMax 4096
#define quitit() abort()
/**
 * Global variables holding enviornment variables used for permissions checking,
 * debug verbosity informations, library paths, etc. Variables initialzed in
 * initializeLib().
 */
/* Path to our /dev/urandom replacment file. */
char* urand = NULL;
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

/* Protoptypes for non wrapper functions in this file. */
static inline char* getEnvVar(std::string var, bool dieIfNotSet);
int comparePaths(const char* fullPath, char* perm);
void checkPermissions(const char* path, bool isWrite);
bool hasPermission(const char* path, bool isWrite);
void PtraceDetLog(Importance imp, std::string format, ...);
bool isAbsolutePath(const char* path);
void fetchEnvVars();

/* Pid table functions. */
pid_t vPidToRPid(pid_t vPid);
pid_t rPidToVPid(pid_t rPid);
void addEntryAndUpdatePid(pid_t rPid);

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
    PtraceDetLog(errorI, "Error: vVal %d map attempted. vVals must be >= 2000.\n", vVal);
    abort();
  }
  /* Too high value, would have gone out of bounds... */
  if(vVal - startingVirVal - mapTableSize >= 0){
    PtraceDetLog(errorI, "Error: vVal %d map attempted. VVals must be < 3000.\n", vVal);
    abort();
  }
  /* Ensure mapping existed. Our vValTable was initialized to 0. */
  pid_t rVal = valTable[vVal - startingVirVal];
  if(rVal == 0){
    PtraceDetLog(errorI, "Error: vVal %d map attempted. No mapping exists for this vVal.\n",
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
    PtraceDetLog(errorI, "Error: rVal %d map attempted. RVal must be > 0.\n", rVal);
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
    PtraceDetLog(errorI, "Error: rVal %d map attempted. No mapping.\n", vVal);
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
    PtraceDetLog(errorI, "Error: rVal %d map attempted. RVals should be > 0.\n", rVal);
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
  PtraceDetLog(extraI, "vPidToRPid call for vPid=%d\n", vPid);
  int result = vValueToRValue((int)vPid, pidTable);
  PtraceDetLog(extraI, "Successful mapped vPid: %d -> rPid: %d\n", vPid, result);

  return (pid_t)result;
}

/**
 * Map a given rPid to vPid. This operation takes O(n) where n is the number of
 * mappings in our table.
 */
pid_t rPidToVPid(pid_t rPid){
  PtraceDetLog(extraI, "rPidToVPid call for rPid=%d\n", rPid);
  int result = rValueToVValue(rPid, freshVPid, pidTable);
  PtraceDetLog(extraI, "Successful mapped rPid: %d -> vPid: %d\n", rPid, result);
  return (pid_t)result;
}


/**
 * Given a rPid create a new vPid -> rPid mapping and update freshVPid global.
 * Fails when rPid <= 0.
 */
void addEntryAndUpdatePid(pid_t rPid){
  PtraceDetLog(extraI, "Adding new mapping vPid: %d -> rPid: %d to pidTable.\n",
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
  PtraceDetLog(extraI, "vInodeToRInode call for vInode=%d\n", vInode);
  int result = vValueToRValue((int)vInode, inodeTable);
  PtraceDetLog(extraI, "Successful mapped vInode: %d -> rInode: %d\n", vInode, result);

  return (ino_t)result;
}

/**
 * Map a given rInode to vInode. This operation takes O(n) where n is the number of
 * mappings in our table.
 */
ino_t rInodeToVInode(ino_t rInode){
  PtraceDetLog(extraI, "rInodeToVInode call for rInode=%lu\n", rInode);
  int result = rValueToVValue((int)rInode, freshVInode, inodeTable);
  PtraceDetLog(extraI, "Successful mapped rInode: %d -> vInode: %d\n", rInode, result);
  return (ino_t)result;
}


/**
 * Given a rInode create a new vInode -> rInode mapping and update freshVInode global.
 * Fails when rInode <= 0.
 */
void addEntryAndUpdateInode(ino_t rInode){
  PtraceDetLog(extraI, "Adding new mapping vInode: %d -> rInode: %d to pidTable.\n",
            freshVInode, rInode);
  addEntryAndUpdate((int)rInode, &freshVInode, inodeTable);
  return;
}


std::string handleSyscall(pid_t tracee, long syscallNumber, struct user_regs_struct* regs);


int main(int argc, char** argv) {
  pid_t tracee;

  tracee = fork();
  bool setOptionsOnTracee = false;

  if (tracee == 0) {
    CHECK(ptrace(PTRACE_TRACEME, 0, NULL, NULL));

    char* traceeArgs [argc];
    memcpy(traceeArgs, argv+1, (argc-1) * sizeof(char*));
    traceeArgs[argc-1] = NULL;

    execvp(traceeArgs[0], traceeArgs);

  } else { // tracer
    while (1) {
      int status;
      tracee = wait(&status);

      // check if tracee has exited
      if (WIFEXITED(status)) { break; }

      // set options
      if (!setOptionsOnTracee) {
        CHECK(ptrace(PTRACE_SETOPTIONS, tracee, NULL, PTRACE_O_TRACECLONE | PTRACE_O_TRACEFORK | PTRACE_O_TRACEVFORK));
        setOptionsOnTracee = true;
      }

      struct user_regs_struct regs;
      CHECK(ptrace(PTRACE_GETREGS, tracee, NULL, &regs));

      std::string syscallName = handleSyscall(tracee, regs.orig_rax, &regs);

      // let tracee run until its next syscall
      CHECK(ptrace(PTRACE_SYSCALL, tracee, NULL, NULL));
    }
  }
  return 0;
}

/** Reads a NUL-delimited string from the given tracee. The contents of the
    string are returned as a C++ string. */
std::string readTraceeCString(pid_t tracee, uint64_t cstringPtr) {
  std::string r;
  bool done = false;
  while (!done) {
    // NB: a long is 8B on a 64-bit platform
    long result = ptrace(PTRACE_PEEKDATA, tracee, cstringPtr, NULL);
    const char* p = (const char*) &result;
    const size_t len = strnlen(p, sizeof(long));
    if (sizeof(long) != len) {
      done = true;
    }
    for (unsigned i = 0; i < len; i++) {
      r += p[i];
    }
    cstringPtr += len;
  }

  return r;
}

void copyFromTracee(long* dst, long* src, const pid_t tracee, const uint32_t bytesToCopy) {
  uint32_t bytesTransferred = 0;
  long *myDst = dst, *traceeSrc = src;
  while (bytesTransferred < bytesToCopy) {
    // NB: this will potentially read >bytesToCopy bytes from tracee, but writes only bytesToCopy bytes to dst
    long result = ptrace(PTRACE_PEEKDATA, tracee, traceeSrc, NULL);
    std::memcpy(myDst, &result,
                std::min(bytesToCopy-bytesTransferred,(uint32_t)sizeof(long)));
    bytesTransferred += sizeof(long);
    myDst++;
    traceeSrc++;
  }
}

void copyToTracee(long* dst, long* src, const pid_t tracee, const uint32_t bytesToCopy) {
  uint32_t bytesTransferred = 0;
  long *traceeDst = dst, *mySrc = src;
  while (bytesTransferred < bytesToCopy) {

    if (bytesToCopy - bytesTransferred >= sizeof(long)) {
      CHECK(ptrace(PTRACE_POKEDATA, tracee, traceeDst, *mySrc));
      bytesTransferred += sizeof(long);

    } else { // handle final transfer of <sizeof(long) bytes
      uint32_t transferSize = bytesToCopy - bytesTransferred;
      // read existing memory from tracee
      long origTraceeMem = ptrace(PTRACE_PEEKDATA, tracee, traceeDst, NULL);
      // overwrite the bytes we need to change
      std::memcpy(&origTraceeMem, src, transferSize);
      // copy merged result back to tracee
      CHECK(ptrace(PTRACE_POKEDATA, tracee, traceeDst, origTraceeMem));
      bytesTransferred += transferSize;
    }

    traceeDst++;
    mySrc++;
  }
}

/**
 * Check permissions for open based on globally set variables.
 */
void pre_open(std::string pathname, int flags, mode_t mode) {
  PtraceDetLog(infoI, "open() called with %s\n", pathname.c_str());

  //If this fails program will crash here!
  checkPermissions(pathname.c_str(), false); // Umm... just pretend this is read?

  // Udev random is handled in post processing...
}

int post_open(std::string pathname, int flags, mode_t mode){
  if(0 == strcmp(pathname.c_str(), "/dev/urandom")){
    PtraceDetLog(infoI, "Intercepted call to /dev/urandom.\n");
    PtraceDetLog(infoI, "Calling our file instead: %s\n", urand);
    int openFd = open(urand, O_RDONLY);
    if(openFd < 0){
      PtraceDetLog(errorI, "Error: Failed to open our version of urandom: %s\n", urand);
      quitit();
    }

    return openFd;
  }
  return -1;
}

void pre_creat(std::string pathname, mode_t mode) {
  PtraceDetLog(infoI, "creat() called with %s\n", pathname.c_str());
}

void pre_openat(int dirfd, std::string pathname, int flags, mode_t mode) {
  PtraceDetLog(infoI, "creat() called with %s\n", pathname.c_str());
}



void clear_statfs(struct statfs* sfs) {
  sfs->f_type = 0xEF53; // ext[234]
  sfs->f_bsize = 1024;
  sfs->f_blocks = 1024;
  sfs->f_bfree = 512;
  sfs->f_bavail = 512;
  sfs->f_files = 128;
  sfs->f_ffree = 64;
  //sfs->f_fsid = 0;
  sfs->f_namelen = 1024;
  sfs->f_frsize = 512;
  sfs->f_flags = 0;
}

void killTracee(std::string syscallName, pid_t tracee) {
  std::cerr << "killing tracee " << tracee << " that made unsupported syscall: " << syscallName << std::endl;
  CHECK(kill(tracee, SIGKILL));
}

std::string handleSyscall(pid_t tracee, long syscallNumber,
                          struct user_regs_struct* regs) {

  // NB: syscall arguments are passed in %rdi, %rsi, %rdx, %r10, %r8 and %r9 (in that order)

  static bool preSyscall = true;
  bool syscallReturns = true;
  std::string syscallName;

  switch(syscallNumber) {

#ifdef SYS__sysctl
  case SYS__sysctl : syscallName = "_sysctl"; break;
#endif

#ifdef SYS_access
  case SYS_access : syscallName = "access"; break;
#endif

#ifdef SYS_acct
  case SYS_acct : syscallName = "acct"; break;
#endif

#ifdef SYS_add_key
  case SYS_add_key : syscallName = "add_key"; break;
#endif

#ifdef SYS_adjtimex
  case SYS_adjtimex : syscallName = "adjtimex"; break;
#endif

#ifdef SYS_afs_syscall
  case SYS_afs_syscall : syscallName = "afs_syscall"; break;
#endif

#ifdef SYS_alarm
  case SYS_alarm : syscallName = "alarm"; break;
#endif

#ifdef SYS_brk
  case SYS_brk : syscallName = "brk"; break;
#endif

#ifdef SYS_capget
  case SYS_capget : syscallName = "capget"; break;
#endif

#ifdef SYS_capset
  case SYS_capset : syscallName = "capset"; break;
#endif

#ifdef SYS_chdir
  case SYS_chdir : syscallName = "chdir"; break;
#endif

#ifdef SYS_chmod
  case SYS_chmod : syscallName = "chmod"; break;
#endif

#ifdef SYS_chown
  case SYS_chown : syscallName = "chown"; break;
#endif

#ifdef SYS_chroot
  case SYS_chroot : syscallName = "chroot"; break;
#endif

#ifdef SYS_clock_getres
  case SYS_clock_getres : syscallName = "clock_getres"; break;
#endif

#ifdef SYS_clock_gettime
  case SYS_clock_gettime : syscallName = "clock_gettime"; break;
#endif

#ifdef SYS_clock_nanosleep
  case SYS_clock_nanosleep : syscallName = "clock_nanosleep"; break;
#endif

#ifdef SYS_clock_settime
  case SYS_clock_settime : syscallName = "clock_settime"; break;
#endif

#ifdef SYS_clone
  case SYS_clone : syscallName = "clone"; break;
#endif

#ifdef SYS_close
  case SYS_close : syscallName = "close"; break;
#endif

#ifdef SYS_creat
  case SYS_creat:
    if (preSyscall) {
      pre_creat(readTraceeCString(tracee, regs->rdi), regs->rsi);
    } else {
      // TODO: post_creat?
    }
    syscallName = "creat";
    break;
#endif

#ifdef SYS_create_module
  case SYS_create_module : syscallName = "create_module"; break;
#endif

#ifdef SYS_delete_module
  case SYS_delete_module : syscallName = "delete_module"; break;
#endif

#ifdef SYS_dup
  case SYS_dup : syscallName = "dup"; break;
#endif

#ifdef SYS_dup2
  case SYS_dup2 : syscallName = "dup2"; break;
#endif

#ifdef SYS_epoll_create
  case SYS_epoll_create : syscallName = "epoll_create"; break;
#endif

#ifdef SYS_epoll_ctl
  case SYS_epoll_ctl : syscallName = "epoll_ctl"; break;
#endif

#ifdef SYS_epoll_pwait
  case SYS_epoll_pwait : syscallName = "epoll_pwait"; break;
#endif

#ifdef SYS_epoll_wait
  case SYS_epoll_wait : syscallName = "epoll_wait"; break;
#endif

#ifdef SYS_eventfd
  case SYS_eventfd : syscallName = "eventfd"; break;
#endif

#ifdef SYS_execve
  case SYS_execve :
    syscallReturns = false;
    syscallName = "execve";
    break;
#endif

#ifdef SYS_exit
  case SYS_exit :
    syscallReturns = false;
    syscallName = "exit";
    break;
#endif

#ifdef SYS_exit_group
  case SYS_exit_group : syscallName = "exit_group"; break;
#endif

#ifdef SYS_faccessat
  case SYS_faccessat : syscallName = "faccessat"; break;
#endif

#ifdef SYS_fadvise64
  case SYS_fadvise64 : syscallName = "fadvise64"; break;
#endif

#ifdef SYS_fallocate
  case SYS_fallocate : syscallName = "fallocate"; break;
#endif

#ifdef SYS_fchdir
  case SYS_fchdir : syscallName = "fchdir"; break;
#endif

#ifdef SYS_fchmod
  case SYS_fchmod : syscallName = "fchmod"; break;
#endif

#ifdef SYS_fchmodat
  case SYS_fchmodat : syscallName = "fchmodat"; break;
#endif

#ifdef SYS_fchown
  case SYS_fchown : syscallName = "fchown"; break;
#endif

#ifdef SYS_fchownat
  case SYS_fchownat : syscallName = "fchownat"; break;
#endif

#ifdef SYS_fcntl
  case SYS_fcntl : syscallName = "fcntl"; break;
#endif

#ifdef SYS_fdatasync
  case SYS_fdatasync : syscallName = "fdatasync"; break;
#endif

#ifdef SYS_fgetxattr
  case SYS_fgetxattr : syscallName = "fgetxattr"; break;
#endif

#ifdef SYS_flistxattr
  case SYS_flistxattr : syscallName = "flistxattr"; break;
#endif

#ifdef SYS_flock
  case SYS_flock : syscallName = "flock"; break;
#endif

#ifdef SYS_fork
  case SYS_fork : syscallName = "fork"; break;
#endif

#ifdef SYS_fremovexattr
  case SYS_fremovexattr : syscallName = "fremovexattr"; break;
#endif

#ifdef SYS_fsetxattr
  case SYS_fsetxattr : syscallName = "fsetxattr"; break;
#endif

#ifdef SYS_fstat
  case SYS_fstat : syscallName = "fstat"; break;
#endif

#ifdef SYS_fstatfs
  case SYS_fstatfs :
    syscallName = "fstatfs";
    if (preSyscall) {
      PtraceDetLog(infoI, "Intercepted call to fstatfs\n");
    } else {
      struct statfs tmp;
      copyFromTracee((long*)&tmp, (long*)regs->rsi, tracee, sizeof(struct statfs));
      clear_statfs(&tmp);
      copyToTracee((long*)regs->rsi, (long*)&tmp, tracee, sizeof(struct statfs));
    }
    break;
#endif

#ifdef SYS_fsync
  case SYS_fsync : syscallName = "fsync"; break;
#endif

#ifdef SYS_ftruncate
  case SYS_ftruncate : syscallName = "ftruncate"; break;
#endif

#ifdef SYS_futex
  case SYS_futex : syscallName = "futex"; break;
#endif

#ifdef SYS_futimesat
  case SYS_futimesat : syscallName = "futimesat"; break;
#endif

#ifdef SYS_get_kernel_syms
  case SYS_get_kernel_syms : syscallName = "get_kernel_syms"; break;
#endif

#ifdef SYS_get_mempolicy
  case SYS_get_mempolicy : syscallName = "get_mempolicy"; break;
#endif

#ifdef SYS_get_robust_list
  case SYS_get_robust_list : syscallName = "get_robust_list"; break;
#endif

#ifdef SYS_get_thread_area
  case SYS_get_thread_area : syscallName = "get_thread_area"; break;
#endif

#ifdef SYS_getcwd
  case SYS_getcwd : syscallName = "getcwd"; break;
#endif

#ifdef SYS_getdents
  case SYS_getdents : syscallName = "getdents"; break;
#endif

#ifdef SYS_getdents64
  case SYS_getdents64 : syscallName = "getdents64"; break;
#endif

#ifdef SYS_getegid
  case SYS_getegid : syscallName = "getegid"; break;
#endif

#ifdef SYS_geteuid
  case SYS_geteuid : syscallName = "geteuid"; break;
#endif

#ifdef SYS_getgid
  case SYS_getgid : syscallName = "getgid"; break;
#endif

#ifdef SYS_getgroups
  case SYS_getgroups : syscallName = "getgroups"; break;
#endif

#ifdef SYS_getitimer
  case SYS_getitimer : syscallName = "getitimer"; break;
#endif

#ifdef SYS_getpgid
  case SYS_getpgid : syscallName = "getpgid"; break;
#endif

#ifdef SYS_getpgrp
  case SYS_getpgrp : syscallName = "getpgrp"; break;
#endif

#ifdef SYS_getpid
  case SYS_getpid :
    syscallName = "getpid";
    if (preSyscall){
      PtraceDetLog(infoI, "Intercepted call to getpid, pre.\n");
    } else {
      PtraceDetLog(infoI, "Intercepted call to getpid, post.\n");

      pid_t returnPid = 0;
      // Write to register. Notice we use pokeuser and 8 * RAX for this.
      // I don't know why but it works!
      // Vitualize me.
      ptrace(PTRACE_POKEUSER, tracee, 8 * RAX, returnPid);
    }
    break;
#endif

#ifdef SYS_getpmsg
  case SYS_getpmsg : syscallName = "getpmsg"; break;
#endif

#ifdef SYS_getppid
  case SYS_getppid : syscallName = "getppid"; break;
#endif

#ifdef SYS_getpriority
  case SYS_getpriority : syscallName = "getpriority"; break;
#endif

#ifdef SYS_getresgid
  case SYS_getresgid : syscallName = "getresgid"; break;
#endif

#ifdef SYS_getresuid
  case SYS_getresuid : syscallName = "getresuid"; break;
#endif

#ifdef SYS_getrlimit
  case SYS_getrlimit : syscallName = "getrlimit"; break;
#endif

#ifdef SYS_getrusage
  case SYS_getrusage : syscallName = "getrusage"; break;
#endif

#ifdef SYS_getsid
  case SYS_getsid : syscallName = "getsid"; break;
#endif

#ifdef SYS_gettid
  case SYS_gettid : syscallName = "gettid"; break;
#endif

#ifdef SYS_gettimeofday
  case SYS_gettimeofday : syscallName = "gettimeofday"; break;
#endif

#ifdef SYS_getuid
  case SYS_getuid : syscallName = "getuid"; break;
#endif

#ifdef SYS_getxattr
  case SYS_getxattr : syscallName = "getxattr"; break;
#endif

#ifdef SYS_init_module
  case SYS_init_module : syscallName = "init_module"; break;
#endif

#ifdef SYS_inotify_add_watch
  case SYS_inotify_add_watch : syscallName = "inotify_add_watch"; break;
#endif

#ifdef SYS_inotify_init
  case SYS_inotify_init : syscallName = "inotify_init"; break;
#endif

#ifdef SYS_inotify_rm_watch
  case SYS_inotify_rm_watch : syscallName = "inotify_rm_watch"; break;
#endif

#ifdef SYS_io_cancel
  case SYS_io_cancel : syscallName = "io_cancel"; break;
#endif

#ifdef SYS_io_destroy
  case SYS_io_destroy : syscallName = "io_destroy"; break;
#endif

#ifdef SYS_io_getevents
  case SYS_io_getevents : syscallName = "io_getevents"; break;
#endif

#ifdef SYS_io_setup
  case SYS_io_setup : syscallName = "io_setup"; break;
#endif

#ifdef SYS_io_submit
  case SYS_io_submit : syscallName = "io_submit"; break;
#endif

#ifdef SYS_ioctl
  case SYS_ioctl : syscallName = "ioctl"; break;
#endif

#ifdef SYS_ioperm
  case SYS_ioperm : syscallName = "ioperm"; break;
#endif

#ifdef SYS_iopl
  case SYS_iopl : syscallName = "iopl"; break;
#endif

#ifdef SYS_ioprio_get
  case SYS_ioprio_get : syscallName = "ioprio_get"; break;
#endif

#ifdef SYS_ioprio_set
  case SYS_ioprio_set : syscallName = "ioprio_set"; break;
#endif

#ifdef SYS_kexec_load
  case SYS_kexec_load : syscallName = "kexec_load"; break;
#endif

#ifdef SYS_keyctl
  case SYS_keyctl : syscallName = "keyctl"; break;
#endif

#ifdef SYS_kill
  case SYS_kill : syscallName = "kill"; break;
#endif

#ifdef SYS_lchown
  case SYS_lchown : syscallName = "lchown"; break;
#endif

#ifdef SYS_lgetxattr
  case SYS_lgetxattr : syscallName = "lgetxattr"; break;
#endif

#ifdef SYS_link
  case SYS_link : syscallName = "link"; break;
#endif

#ifdef SYS_linkat
  case SYS_linkat : syscallName = "linkat"; break;
#endif

#ifdef SYS_listxattr
  case SYS_listxattr : syscallName = "listxattr"; break;
#endif

#ifdef SYS_llistxattr
  case SYS_llistxattr : syscallName = "llistxattr"; break;
#endif

#ifdef SYS_lookup_dcookie
  case SYS_lookup_dcookie : syscallName = "lookup_dcookie"; break;
#endif

#ifdef SYS_lremovexattr
  case SYS_lremovexattr : syscallName = "lremovexattr"; break;
#endif

#ifdef SYS_lseek
  case SYS_lseek : syscallName = "lseek"; break;
#endif

#ifdef SYS_lsetxattr
  case SYS_lsetxattr : syscallName = "lsetxattr"; break;
#endif

#ifdef SYS_lstat
  case SYS_lstat : syscallName = "lstat"; break;
#endif

#ifdef SYS_madvise
  case SYS_madvise : syscallName = "madvise"; break;
#endif

#ifdef SYS_mbind
  case SYS_mbind : syscallName = "mbind"; break;
#endif

#ifdef SYS_migrate_pages
  case SYS_migrate_pages : syscallName = "migrate_pages"; break;
#endif

#ifdef SYS_mincore
  case SYS_mincore : syscallName = "mincore"; break;
#endif

#ifdef SYS_mkdir
  case SYS_mkdir : syscallName = "mkdir"; break;
#endif

#ifdef SYS_mkdirat
  case SYS_mkdirat : syscallName = "mkdirat"; break;
#endif

#ifdef SYS_mknod
  case SYS_mknod : syscallName = "mknod"; break;
#endif

#ifdef SYS_mknodat
  case SYS_mknodat : syscallName = "mknodat"; break;
#endif

#ifdef SYS_mlock
  case SYS_mlock : syscallName = "mlock"; break;
#endif

#ifdef SYS_mlockall
  case SYS_mlockall : syscallName = "mlockall"; break;
#endif

#ifdef SYS_mmap
  case SYS_mmap : syscallName = "mmap"; break;
#endif

#ifdef SYS_modify_ldt
  case SYS_modify_ldt : syscallName = "modify_ldt"; break;
#endif

#ifdef SYS_mount
  case SYS_mount : syscallName = "mount"; break;
#endif

#ifdef SYS_move_pages
  case SYS_move_pages : syscallName = "move_pages"; break;
#endif

#ifdef SYS_mprotect
  case SYS_mprotect : syscallName = "mprotect"; break;
#endif

#ifdef SYS_mq_getsetattr
  case SYS_mq_getsetattr : syscallName = "mq_getsetattr"; break;
#endif

#ifdef SYS_mq_notify
  case SYS_mq_notify : syscallName = "mq_notify"; break;
#endif

#ifdef SYS_mq_open
  case SYS_mq_open : syscallName = "mq_open"; break;
#endif

#ifdef SYS_mq_timedreceive
  case SYS_mq_timedreceive : syscallName = "mq_timedreceive"; break;
#endif

#ifdef SYS_mq_timedsend
  case SYS_mq_timedsend : syscallName = "mq_timedsend"; break;
#endif

#ifdef SYS_mq_unlink
  case SYS_mq_unlink : syscallName = "mq_unlink"; break;
#endif

#ifdef SYS_mremap
  case SYS_mremap : syscallName = "mremap"; break;
#endif

#ifdef SYS_msync
  case SYS_msync : syscallName = "msync"; break;
#endif

#ifdef SYS_munlock
  case SYS_munlock : syscallName = "munlock"; break;
#endif

#ifdef SYS_munlockall
  case SYS_munlockall : syscallName = "munlockall"; break;
#endif

#ifdef SYS_munmap
  case SYS_munmap : syscallName = "munmap"; break;
#endif

#ifdef SYS_nanosleep
  case SYS_nanosleep : syscallName = "nanosleep"; break;
#endif

#ifdef SYS_nfsservctl
  case SYS_nfsservctl : syscallName = "nfsservctl"; break;
#endif

#ifdef SYS_open
  case SYS_open :
    if (preSyscall) {
      pre_open(readTraceeCString(tracee, regs->rdi), regs->rsi, regs->rdx);

    } else {
      // Handle opening /dev/urandom here!
      // int result = post_open(readTraceeCString(tracee, regs->rdi), regs->rsi, regs->rdx);

      // caught call dot /dev/urandom change the return descriptor to ours.
      // if(result != -1){

        // ptrace(PTRACE_POKEUSER, tracee, 8 * RAX, result);
      // }
      //Does not work! Even if I change the file descriptor the return register points
      // to I'm not sure how to let the OS know that, the file descriptor points to a
      // different file now...:
    }
    syscallName = "open";
    break;
#endif

#ifdef SYS_openat
  case SYS_openat:
    if (preSyscall) {
      pre_openat(regs->rdi, readTraceeCString(tracee, regs->rsi), regs->rdx, regs->r10);
    } else {
      // TODO: post_openat?
    }
    syscallName = "openat";
    break;
#endif

#ifdef SYS_pause
  case SYS_pause : syscallName = "pause"; break;
#endif

#ifdef SYS_personality
  case SYS_personality : syscallName = "personality"; break;
#endif

#ifdef SYS_pipe
  case SYS_pipe : syscallName = "pipe"; break;
#endif

#ifdef SYS_pivot_root
  case SYS_pivot_root : syscallName = "pivot_root"; break;
#endif

#ifdef SYS_poll
  case SYS_poll : syscallName = "poll"; break;
#endif

#ifdef SYS_ppoll
  case SYS_ppoll : syscallName = "ppoll"; break;
#endif

#ifdef SYS_prctl
  case SYS_prctl : syscallName = "prctl"; break;
#endif

#ifdef SYS_pread64
  case SYS_pread64 : syscallName = "pread64"; break;
#endif

#ifdef SYS_pselect6
  case SYS_pselect6 : syscallName = "pselect6"; break;
#endif

#ifdef SYS_ptrace
  case SYS_ptrace : syscallName = "ptrace"; break;
#endif

#ifdef SYS_putpmsg
  case SYS_putpmsg : syscallName = "putpmsg"; break;
#endif

#ifdef SYS_pwrite64
  case SYS_pwrite64 : syscallName = "pwrite64"; break;
#endif

#ifdef SYS_query_module
  case SYS_query_module : syscallName = "query_module"; break;
#endif

#ifdef SYS_quotactl
  case SYS_quotactl : syscallName = "quotactl"; break;
#endif

#ifdef SYS_read
  case SYS_read : syscallName = "read"; break;
#endif

#ifdef SYS_readahead
  case SYS_readahead : syscallName = "readahead"; break;
#endif

#ifdef SYS_readlink
  case SYS_readlink : syscallName = "readlink"; break;
#endif

#ifdef SYS_readlinkat
  case SYS_readlinkat : syscallName = "readlinkat"; break;
#endif

#ifdef SYS_readv
  case SYS_readv : syscallName = "readv"; break;
#endif

#ifdef SYS_reboot
  case SYS_reboot : syscallName = "reboot"; break;
#endif

#ifdef SYS_remap_file_pages
  case SYS_remap_file_pages : syscallName = "remap_file_pages"; break;
#endif

#ifdef SYS_removexattr
  case SYS_removexattr : syscallName = "removexattr"; break;
#endif

#ifdef SYS_rename
  case SYS_rename : syscallName = "rename"; break;
#endif

#ifdef SYS_renameat
  case SYS_renameat : syscallName = "renameat"; break;
#endif

#ifdef SYS_request_key
  case SYS_request_key : syscallName = "request_key"; break;
#endif

#ifdef SYS_restart_syscall
  case SYS_restart_syscall : syscallName = "restart_syscall"; break;
#endif

#ifdef SYS_rmdir
  case SYS_rmdir : syscallName = "rmdir"; break;
#endif

#ifdef SYS_rt_sigaction
  case SYS_rt_sigaction : syscallName = "rt_sigaction"; break;
#endif

#ifdef SYS_rt_sigpending
  case SYS_rt_sigpending : syscallName = "rt_sigpending"; break;
#endif

#ifdef SYS_rt_sigprocmask
  case SYS_rt_sigprocmask : syscallName = "rt_sigprocmask"; break;
#endif

#ifdef SYS_rt_sigqueueinfo
  case SYS_rt_sigqueueinfo : syscallName = "rt_sigqueueinfo"; break;
#endif

#ifdef SYS_rt_sigreturn
  case SYS_rt_sigreturn : syscallName = "rt_sigreturn"; break;
#endif

#ifdef SYS_rt_sigsuspend
  case SYS_rt_sigsuspend : syscallName = "rt_sigsuspend"; break;
#endif

#ifdef SYS_rt_sigtimedwait
  case SYS_rt_sigtimedwait : syscallName = "rt_sigtimedwait"; break;
#endif

#ifdef SYS_sched_get_priority_max
  case SYS_sched_get_priority_max : syscallName = "sched_get_priority_max"; break;
#endif

#ifdef SYS_sched_get_priority_min
  case SYS_sched_get_priority_min : syscallName = "sched_get_priority_min"; break;
#endif

#ifdef SYS_sched_getaffinity
  case SYS_sched_getaffinity : syscallName = "sched_getaffinity"; break;
#endif

#ifdef SYS_sched_getparam
  case SYS_sched_getparam : syscallName = "sched_getparam"; break;
#endif

#ifdef SYS_sched_getscheduler
  case SYS_sched_getscheduler : syscallName = "sched_getscheduler"; break;
#endif

#ifdef SYS_sched_rr_get_interval
  case SYS_sched_rr_get_interval : syscallName = "sched_rr_get_interval"; break;
#endif

#ifdef SYS_sched_setaffinity
  case SYS_sched_setaffinity : syscallName = "sched_setaffinity"; break;
#endif

#ifdef SYS_sched_setparam
  case SYS_sched_setparam : syscallName = "sched_setparam"; break;
#endif

#ifdef SYS_sched_setscheduler
  case SYS_sched_setscheduler : syscallName = "sched_setscheduler"; break;
#endif

#ifdef SYS_sched_yield
  case SYS_sched_yield : syscallName = "sched_yield"; break;
#endif

#ifdef SYS_select
  case SYS_select : syscallName = "select"; break;
#endif

#ifdef SYS_sendfile
  case SYS_sendfile : syscallName = "sendfile"; break;
#endif

#ifdef SYS_set_mempolicy
  case SYS_set_mempolicy : syscallName = "set_mempolicy"; break;
#endif

#ifdef SYS_set_robust_list
  case SYS_set_robust_list : syscallName = "set_robust_list"; break;
#endif

#ifdef SYS_set_thread_area
  case SYS_set_thread_area : syscallName = "set_thread_area"; break;
#endif

#ifdef SYS_set_tid_address
  case SYS_set_tid_address : syscallName = "set_tid_address"; break;
#endif

#ifdef SYS_setdomainname
  case SYS_setdomainname : syscallName = "setdomainname"; break;
#endif

#ifdef SYS_setfsgid
  case SYS_setfsgid : syscallName = "setfsgid"; break;
#endif

#ifdef SYS_setfsuid
  case SYS_setfsuid : syscallName = "setfsuid"; break;
#endif

#ifdef SYS_setgid
  case SYS_setgid : syscallName = "setgid"; break;
#endif

#ifdef SYS_setgroups
  case SYS_setgroups : syscallName = "setgroups"; break;
#endif

#ifdef SYS_sethostname
  case SYS_sethostname : syscallName = "sethostname"; break;
#endif

#ifdef SYS_setitimer
  case SYS_setitimer : syscallName = "setitimer"; break;
#endif

#ifdef SYS_setpgid
  case SYS_setpgid : syscallName = "setpgid"; break;
#endif

#ifdef SYS_setpriority
  case SYS_setpriority : syscallName = "setpriority"; break;
#endif

#ifdef SYS_setregid
  case SYS_setregid : syscallName = "setregid"; break;
#endif

#ifdef SYS_setresgid
  case SYS_setresgid : syscallName = "setresgid"; break;
#endif

#ifdef SYS_setresuid
  case SYS_setresuid : syscallName = "setresuid"; break;
#endif

#ifdef SYS_setreuid
  case SYS_setreuid : syscallName = "setreuid"; break;
#endif

#ifdef SYS_setrlimit
  case SYS_setrlimit : syscallName = "setrlimit"; break;
#endif

#ifdef SYS_setsid
  case SYS_setsid : syscallName = "setsid"; break;
#endif

#ifdef SYS_settimeofday
  case SYS_settimeofday : syscallName = "settimeofday"; break;
#endif


#ifdef SYS_setuid
  case SYS_setuid : syscallName = "setuid"; break;
#endif

#ifdef SYS_setxattr
  case SYS_setxattr : syscallName = "setxattr"; break;
#endif

#ifdef SYS_sigaltstack
  case SYS_sigaltstack : syscallName = "sigaltstack"; break;
#endif

#ifdef SYS_signalfd
  case SYS_signalfd : syscallName = "signalfd"; break;
#endif

#ifdef SYS_splice
  case SYS_splice : syscallName = "splice"; break;
#endif

#ifdef SYS_stat
  case SYS_stat : syscallName = "stat"; break;
#endif

#ifdef SYS_statfs
  case SYS_statfs :
    syscallName = "statfs";
    if (preSyscall) {
      PtraceDetLog(infoI, "Intercepted call to statfs: %s.\n",
                readTraceeCString(tracee, regs->rdi).c_str());
    } else {
      PtraceDetLog(infoI, "Intercepted call to statfs, post.\n");
      struct statfs tmp;
      copyFromTracee((long*)&tmp, (long*)regs->rsi, tracee, sizeof(struct statfs));
      clear_statfs(&tmp);
      copyToTracee((long*)regs->rsi, (long*)&tmp, tracee, sizeof(struct statfs));
    }
    break;
#endif

#ifdef SYS_swapoff
  case SYS_swapoff : syscallName = "swapoff"; break;
#endif

#ifdef SYS_swapon
  case SYS_swapon : syscallName = "swapon"; break;
#endif

#ifdef SYS_symlink
  case SYS_symlink : syscallName = "symlink"; break;
#endif

#ifdef SYS_symlinkat
  case SYS_symlinkat : syscallName = "symlinkat"; break;
#endif

#ifdef SYS_sync
  case SYS_sync : syscallName = "sync"; break;
#endif

#ifdef SYS_sync_file_range
  case SYS_sync_file_range : syscallName = "sync_file_range"; break;
#endif

#ifdef SYS_sysfs
  case SYS_sysfs : syscallName = "sysfs"; break;
#endif

#ifdef SYS_sysinfo
  case SYS_sysinfo : syscallName = "sysinfo"; break;
#endif

#ifdef SYS_syslog
  case SYS_syslog : syscallName = "syslog"; break;
#endif

#ifdef SYS_tee
  case SYS_tee : syscallName = "tee"; break;
#endif

#ifdef SYS_tgkill
  case SYS_tgkill : syscallName = "tgkill"; break;
#endif

#ifdef SYS_time
  case SYS_time : syscallName = "time"; break;
#endif

#ifdef SYS_timer_create
  case SYS_timer_create : syscallName = "timer_create"; break;
#endif

#ifdef SYS_timer_delete
  case SYS_timer_delete : syscallName = "timer_delete"; break;
#endif

#ifdef SYS_timer_getoverrun
  case SYS_timer_getoverrun : syscallName = "timer_getoverrun"; break;
#endif

#ifdef SYS_timer_gettime
  case SYS_timer_gettime : syscallName = "timer_gettime"; break;
#endif

#ifdef SYS_timer_settime
  case SYS_timer_settime : syscallName = "timer_settime"; break;
#endif

#ifdef SYS_timerfd_create
  case SYS_timerfd_create : syscallName = "timerfd_create"; break;
#endif

#ifdef SYS_timerfd_gettime
  case SYS_timerfd_gettime : syscallName = "timerfd_gettime"; break;
#endif

#ifdef SYS_timerfd_settime
  case SYS_timerfd_settime : syscallName = "timerfd_settime"; break;
#endif

#ifdef SYS_times
  case SYS_times : syscallName = "times"; break;
#endif

#ifdef SYS_tkill
  case SYS_tkill : syscallName = "tkill"; break;
#endif

#ifdef SYS_truncate
  case SYS_truncate : syscallName = "truncate"; break;
#endif

#ifdef SYS_umask
  case SYS_umask : syscallName = "umask"; break;
#endif

#ifdef SYS_umount2
  case SYS_umount2 : syscallName = "umount2"; break;
#endif

#ifdef SYS_uname
  case SYS_uname : syscallName = "uname"; break;
#endif

#ifdef SYS_unlink
  case SYS_unlink : syscallName = "unlink"; break;
#endif

#ifdef SYS_unlinkat
  case SYS_unlinkat : syscallName = "unlinkat"; break;
#endif

#ifdef SYS_unshare
  case SYS_unshare : syscallName = "unshare"; break;
#endif

#ifdef SYS_uselib
  case SYS_uselib : syscallName = "uselib"; break;
#endif

#ifdef SYS_ustat
  case SYS_ustat :
    syscallName = "ustat";
    killTracee(syscallName, tracee);
    break;
#endif

#ifdef SYS_utime
  case SYS_utime : syscallName = "utime"; break;
#endif

#ifdef SYS_utimensat
  case SYS_utimensat : syscallName = "utimensat"; break;
#endif

#ifdef SYS_utimes
  case SYS_utimes : syscallName = "utimes"; break;
#endif

#ifdef SYS_vfork
  case SYS_vfork : syscallName = "vfork"; break;
#endif

#ifdef SYS_vhangup
  case SYS_vhangup : syscallName = "vhangup"; break;
#endif

#ifdef SYS_vmsplice
  case SYS_vmsplice : syscallName = "vmsplice"; break;
#endif

#ifdef SYS_vserver
  case SYS_vserver : syscallName = "vserver"; break;
#endif

#ifdef SYS_wait4
  case SYS_wait4 : syscallName = "wait4"; break;
#endif

#ifdef SYS_waitid
  case SYS_waitid : syscallName = "waitid"; break;
#endif

#ifdef SYS_write
  case SYS_write : syscallName = "write"; break;
#endif

#ifdef SYS_writev
  case SYS_writev : syscallName = "writev"; break;
#endif

#ifdef SYS_accept
  case SYS_accept : syscallName = "accept"; break;
#endif

#ifdef SYS_arch_prctl
  case SYS_arch_prctl : syscallName = "arch_prctl"; break;
#endif

#ifdef SYS_bind
  case SYS_bind : syscallName = "bind"; break;
#endif

#ifdef SYS_connect
  case SYS_connect : syscallName = "connect"; break;
#endif

#ifdef SYS_epoll_ctl_old
  case SYS_epoll_ctl_old : syscallName = "epoll_ctl_old"; break;
#endif

#ifdef SYS_epoll_wait_old
  case SYS_epoll_wait_old : syscallName = "epoll_wait_old"; break;
#endif

#ifdef SYS_getpeername
  case SYS_getpeername : syscallName = "getpeername"; break;
#endif

#ifdef SYS_getsockname
  case SYS_getsockname : syscallName = "getsockname"; break;
#endif

#ifdef SYS_getsockopt
  case SYS_getsockopt : syscallName = "getsockopt"; break;
#endif

#ifdef SYS_listen
  case SYS_listen : syscallName = "listen"; break;
#endif

#ifdef SYS_msgctl
  case SYS_msgctl : syscallName = "msgctl"; break;
#endif

#ifdef SYS_msgget
  case SYS_msgget : syscallName = "msgget"; break;
#endif

#ifdef SYS_msgrcv
  case SYS_msgrcv : syscallName = "msgrcv"; break;
#endif

#ifdef SYS_msgsnd
  case SYS_msgsnd : syscallName = "msgsnd"; break;
#endif

#ifdef SYS_newfstatat
  case SYS_newfstatat : syscallName = "newfstatat"; break;
#endif

#ifdef SYS_recvfrom
  case SYS_recvfrom : syscallName = "recvfrom"; break;
#endif

#ifdef SYS_recvmsg
  case SYS_recvmsg : syscallName = "recvmsg"; break;
#endif

#ifdef SYS_security
  case SYS_security : syscallName = "security"; break;
#endif

#ifdef SYS_semctl
  case SYS_semctl : syscallName = "semctl"; break;
#endif

#ifdef SYS_semget
  case SYS_semget : syscallName = "semget"; break;
#endif

#ifdef SYS_semop
  case SYS_semop : syscallName = "semop"; break;
#endif

#ifdef SYS_semtimedop
  case SYS_semtimedop : syscallName = "semtimedop"; break;
#endif

#ifdef SYS_sendmsg
  case SYS_sendmsg : syscallName = "sendmsg"; break;
#endif

#ifdef SYS_sendto
  case SYS_sendto : syscallName = "sendto"; break;
#endif

#ifdef SYS_setsockopt
  case SYS_setsockopt : syscallName = "setsockopt"; break;
#endif

#ifdef SYS_shmat
  case SYS_shmat : syscallName = "shmat"; break;
#endif

#ifdef SYS_shmctl
  case SYS_shmctl : syscallName = "shmctl"; break;
#endif

#ifdef SYS_shmdt
  case SYS_shmdt : syscallName = "shmdt"; break;
#endif

#ifdef SYS_shmget
  case SYS_shmget : syscallName = "shmget"; break;
#endif

#ifdef SYS_shutdown
  case SYS_shutdown : syscallName = "shutdown"; break;
#endif

#ifdef SYS_socket
  case SYS_socket : syscallName = "socket"; break;
#endif

#ifdef SYS_socketpair
  case SYS_socketpair : syscallName = "socketpair"; break;
#endif

#ifdef SYS_tuxcall
  case SYS_tuxcall : syscallName = "tuxcall"; break;
#endif

#ifdef SYS__llseek
  case SYS__llseek : syscallName = "_llseek"; break;
#endif

#ifdef SYS__newselect
  case SYS__newselect : syscallName = "_newselect"; break;
#endif

#ifdef SYS_bdflush
  case SYS_bdflush : syscallName = "bdflush"; break;
#endif

#ifdef SYS_break
  case SYS_break : syscallName = "break"; break;
#endif

#ifdef SYS_chown32
  case SYS_chown32 : syscallName = "chown32"; break;
#endif

#ifdef SYS_fadvise64_64
  case SYS_fadvise64_64 : syscallName = "fadvise64_64"; break;
#endif

#ifdef SYS_fchown32
  case SYS_fchown32 : syscallName = "fchown32"; break;
#endif

#ifdef SYS_fcntl64
  case SYS_fcntl64 : syscallName = "fcntl64"; break;
#endif

#ifdef SYS_fstat64
  case SYS_fstat64 : syscallName = "fstat64"; break;
#endif

#ifdef SYS_fstatat64
  case SYS_fstatat64 : syscallName = "fstatat64"; break;
#endif

#ifdef SYS_fstatfs64
  case SYS_fstatfs64 :
    syscallName = "fstatfs64";
    killTracee(syscallName, tracee);
    break;
#endif

#ifdef SYS_ftime
  case SYS_ftime : syscallName = "ftime"; break;
#endif

#ifdef SYS_ftruncate64
  case SYS_ftruncate64 : syscallName = "ftruncate64"; break;
#endif

#ifdef SYS_getcpu
  case SYS_getcpu : syscallName = "getcpu"; break;
#endif

#ifdef SYS_getegid32
  case SYS_getegid32 : syscallName = "getegid32"; break;
#endif

#ifdef SYS_geteuid32
  case SYS_geteuid32 : syscallName = "geteuid32"; break;
#endif

#ifdef SYS_getgid32
  case SYS_getgid32 : syscallName = "getgid32"; break;
#endif

#ifdef SYS_getgroups32
  case SYS_getgroups32 : syscallName = "getgroups32"; break;
#endif

#ifdef SYS_getresgid32
  case SYS_getresgid32 : syscallName = "getresgid32"; break;
#endif

#ifdef SYS_getresuid32
  case SYS_getresuid32 : syscallName = "getresuid32"; break;
#endif

#ifdef SYS_getuid32
  case SYS_getuid32 : syscallName = "getuid32"; break;
#endif

#ifdef SYS_gtty
  case SYS_gtty : syscallName = "gtty"; break;
#endif

#ifdef SYS_idle
  case SYS_idle : syscallName = "idle"; break;
#endif

#ifdef SYS_ipc
  case SYS_ipc : syscallName = "ipc"; break;
#endif

#ifdef SYS_lchown32
  case SYS_lchown32 : syscallName = "lchown32"; break;
#endif

#ifdef SYS_lock
  case SYS_lock : syscallName = "lock"; break;
#endif

#ifdef SYS_lstat64
  case SYS_lstat64 : syscallName = "lstat64"; break;
#endif

#ifdef SYS_madvise1
  case SYS_madvise1 : syscallName = "madvise1"; break;
#endif

#ifdef SYS_mmap2
  case SYS_mmap2 : syscallName = "mmap2"; break;
#endif

#ifdef SYS_mpx
  case SYS_mpx : syscallName = "mpx"; break;
#endif

#ifdef SYS_nice
  case SYS_nice : syscallName = "nice"; break;
#endif

#ifdef SYS_oldfstat
  case SYS_oldfstat :
    syscallName = "oldfstat";
    killTracee(syscallName, tracee);
    break;
#endif

#ifdef SYS_oldlstat
  case SYS_oldlstat :
    syscallName = "oldlstat";
    killTracee(syscallName, tracee);
    break;
#endif

#ifdef SYS_oldolduname
  case SYS_oldolduname : syscallName = "oldolduname"; break;
#endif

#ifdef SYS_oldstat
  case SYS_oldstat :
    syscallName = "oldstat";
    killTracee(syscallName, tracee);
    break;
#endif

#ifdef SYS_olduname
  case SYS_olduname : syscallName = "olduname"; break;
#endif

#ifdef SYS_prof
  case SYS_prof : syscallName = "prof"; break;
#endif

#ifdef SYS_profil
  case SYS_profil : syscallName = "profil"; break;
#endif

#ifdef SYS_readdir
  case SYS_readdir : syscallName = "readdir"; break;
#endif

#ifdef SYS_sendfile64
  case SYS_sendfile64 : syscallName = "sendfile64"; break;
#endif

#ifdef SYS_setfsgid32
  case SYS_setfsgid32 : syscallName = "setfsgid32"; break;
#endif

#ifdef SYS_setfsuid32
  case SYS_setfsuid32 : syscallName = "setfsuid32"; break;
#endif

#ifdef SYS_setgid32
  case SYS_setgid32 : syscallName = "setgid32"; break;
#endif

#ifdef SYS_setgroups32
  case SYS_setgroups32 : syscallName = "setgroups32"; break;
#endif

#ifdef SYS_setregid32
  case SYS_setregid32 : syscallName = "setregid32"; break;
#endif

#ifdef SYS_setresgid32
  case SYS_setresgid32 : syscallName = "setresgid32"; break;
#endif

#ifdef SYS_setresuid32
  case SYS_setresuid32 : syscallName = "setresuid32"; break;
#endif

#ifdef SYS_setreuid32
  case SYS_setreuid32 : syscallName = "setreuid32"; break;
#endif

#ifdef SYS_setuid32
  case SYS_setuid32 : syscallName = "setuid32"; break;
#endif

#ifdef SYS_sgetmask
  case SYS_sgetmask : syscallName = "sgetmask"; break;
#endif

#ifdef SYS_sigaction
  case SYS_sigaction : syscallName = "sigaction"; break;
#endif

#ifdef SYS_signal
  case SYS_signal : syscallName = "signal"; break;
#endif

#ifdef SYS_sigpending
  case SYS_sigpending : syscallName = "sigpending"; break;
#endif

#ifdef SYS_sigprocmask
  case SYS_sigprocmask : syscallName = "sigprocmask"; break;
#endif

#ifdef SYS_sigreturn
  case SYS_sigreturn : syscallName = "sigreturn"; break;
#endif

#ifdef SYS_sigsuspend
  case SYS_sigsuspend : syscallName = "sigsuspend"; break;
#endif

#ifdef SYS_socketcall
  case SYS_socketcall : syscallName = "socketcall"; break;
#endif

#ifdef SYS_ssetmask
  case SYS_ssetmask : syscallName = "ssetmask"; break;
#endif

#ifdef SYS_stat64
  case SYS_stat64 :
    syscallName = "stat64";

    break;
#endif

#ifdef SYS_statfs64
  case SYS_statfs64 :
    syscallName = "statfs64";
    killTracee(syscallName, tracee);
    break;
#endif

#ifdef SYS_stime
  case SYS_stime : syscallName = "stime"; break;
#endif

#ifdef SYS_stty
  case SYS_stty : syscallName = "stty"; break;
#endif

#ifdef SYS_truncate64
  case SYS_truncate64 : syscallName = "truncate64"; break;
#endif

#ifdef SYS_ugetrlimit
  case SYS_ugetrlimit : syscallName = "ugetrlimit"; break;
#endif

#ifdef SYS_ulimit
  case SYS_ulimit : syscallName = "ulimit"; break;
#endif

#ifdef SYS_umount
  case SYS_umount : syscallName = "umount"; break;
#endif

#ifdef SYS_vm86
  case SYS_vm86 : syscallName = "vm86"; break;
#endif

#ifdef SYS_vm86old
  case SYS_vm86old : syscallName = "vm86old"; break;
#endif

#ifdef SYS_waitpid
  case SYS_waitpid : syscallName = "waitpid"; break;
#endif

  default:
    syscallName = "unknown";
  }

  if (syscallReturns) { preSyscall = !preSyscall; }

  return syscallName;
}
//========================================================================================

/**
 * Initialization function. Called once when this library is linked at runtime.
 * 1) Prefetch pointers to all original libc functions for efficiency and keep them
 * as global variables.
 * 2) Fetch environment variables.
 * 3) TODO Initialize deterministic PID table.
 */
void initializeLib(){
  /* Ignore call if library is already initialzed */
  if(libInit){
    PtraceDetLog(infoI, "Warning: library is already initialized, attempt to initalize again ignored.\n");
    return;
  }

  libInit = true;
  fetchEnvVars();
 
  /* Add initial entry to pidTable for current process. */
  pid_t myRPid = getpid();
  addEntryAndUpdatePid(myRPid);

  PtraceDetLog(infoI, "PtraceDet. Library initialized.\n");
  return;
}

// TODO: Replace me with something more general:
bool isAbsolutePath(const char* path) {
  return (path[0] == '/');
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
void PtraceDetLog(Importance imp, std::string format, ...) {
  va_list args;
  /* Assumme it won't fail... */
  if(! libInit){ initializeLib(); }

  int debugNum = strtol(debugLevel, NULL, 10);
  bool print = false;

  /* Unknown importance level. */
  if(imp != errorI && imp != interI && imp != infoI && imp != extraI){
    fprintf(stderr, "  [PtraceDet] Warning: Unknown importance level.\n");
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
    fprintf(stderr, "  [PtraceDet] Warning Unknown DEBUG level %d.\n", debugNum);
    break;
  }

  if(print){
    fprintf(stderr, "  [PtraceDet %d %s] ", getpid(), __progname);
    va_start(args, format);
    vfprintf(stderr, format.c_str(), args);
    va_end(args);
  }

  return;
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
    PtraceDetLog(errorI, "Error: No permission for \"%s\"\n", path);
    quitit();
  }
  return;
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
 * ASSUMPTION: these environment variables aren't mutated.  This
 * function is idempotent.
 */
void fetchEnvVars(){
  /* Fetch our environment variables. */
  urand  = getEnvVar(uRandomFile, true);
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

/**
 * Get env variable copy to free space and return as a heap-allocated pointer.
 */
static inline char* getEnvVar(std::string var, bool dieIfNotSet){
  char* tempResult = getenv(var.c_str());

  if(tempResult == NULL && dieIfNotSet){
    /* Do not make this a call to PtraceDetLog, we need to fetch the env for DEBUG before
       we make a call to it. */
    fprintf(stderr, "  [PtraceDet] Error: Environment variable \"%s\" does not exist.\n", var.c_str());
    quitit();
  }
  if(tempResult == NULL){
    return NULL;
  }
  char* returnVar = (char*) malloc(sizeof(char) * strlen(tempResult) + 1);
  strcpy(returnVar, tempResult);
  return returnVar;
}

/**
 * Predicate function to check permission WITHOUT throwing an error.
 * (Unless an internal error occurs.)
 */
bool hasPermission(const char* path, bool isWrite) {
  bool hasPerm = false;
  char absolutePath[pathMax];
  std::string emptyStr = "";

  if (wPerms == NULL) fetchEnvVars();
  char* correctPerms = isWrite ? wPerms : rPerms;
  if (!correctPerms) correctPerms = (char*)emptyStr.c_str();

  /* Strtok modifies string, copy to separte variable. */
  char* permsToCheck = (char*)malloc(sizeof(char) * strlen(correctPerms) + 1);
  char* scratch = (char*)malloc(sizeof(char) * strlen(correctPerms) + 1000);
  /* Pointer to original location for freeing. Needed as strtok_r will mutate the scratch
     pointer to a different place. */
  char* p = scratch;
  strcpy(permsToCheck, correctPerms);

  PtraceDetLog(infoI, "Current %s Permissions: %s\n", isWrite ? "W" : "R", correctPerms);

  /* Look for absolute path. If none found use regular path instead... */
  // RRN: Disabling realpath for now.  /proc/self/maps is a symlink. See #2
  // char* status = (realpath(path, absolutePath));
  // if(status == NULL)
  {
    if(isAbsolutePath(path)) {
      PtraceDetLog(infoI, "Realpath returned NULL!  Nonexistent absolute path.\n");
      strcpy(absolutePath, path);
    } else {
      char cwd[pathMax];
      char* status2 = getcwd(cwd,pathMax);
      if (!status2) {
        PtraceDetLog(errorI, "Error: getcwd failed\n");
        quitit();
      }
      sprintf(absolutePath, "%s/%s", cwd, path);
      // TODO: could create an absolute path
      PtraceDetLog(infoI, "Hackishly assembling absolute path...\n");
    }
  }

  PtraceDetLog(infoI, "Using path \"%s\" for permission lookup (originally %s).\n",
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

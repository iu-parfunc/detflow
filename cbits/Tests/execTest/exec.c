#include <unistd.h>
#include <sys/time.h>
#include <stdio.h>

/* int execve(const char *filename, char *const argv[], char *const envp[]); */
/* Test out execve, do we inherent LD_PRELOAD from our parent? */

/* This test shows processes automatically inherent the enviornment from their parents.
* So there is no need for us to intercept exec functions ourselves.
* We must stil check the filepath though, to make sure the user doesn't have access to
* files outside it's permissions. */


int main(){
  char* args[1] = {"./time"};
  execv("./time", args);
  return 0;
}

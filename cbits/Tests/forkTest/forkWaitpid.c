#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include <time.h>
#include <stdlib.h>
#include <sys/wait.h>

/**
 * Variant of fork where we wait ourselves to ensure waits are being handled
 * properly by our libdet interception.

 * See doForks() in libdet.so for description of problem.

 * This variant uses waitpid.
 */
int main(void){
  int status1;
  int status2;

  printf("Parent: My pid: %d\n", getpid());
  pid_t pid = fork();
  waitpid(pid, &status1, 0);

  /* Child is assigned PID = 0. */
  if(pid == 0){
    pid_t pid2 = fork();

    if(pid2 == 0){
      printf("Grandchild: My pid: %d\n", getpid());
    }
    else{
      waitpid(pid2, &status2, 0);
      printf("Child: My child's PID: %d\n", pid2);
      printf("Child: My PID: %d\n", getpid());
    }
  }
  else{
    printf("Parent: My child's PID %d\n", pid);
    printf("Parent: My pid: %d\n", getpid());
  }
  return 0;
}

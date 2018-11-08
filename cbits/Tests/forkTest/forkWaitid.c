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

 * This variant uses wait().
 */
int main(void){
  siginfo_t infop1;
  siginfo_t infop2;

  printf("Parent: My pid: %d\n", getpid());
  pid_t pid = fork();

  /* Child is assigned PID = 0. */
  if(pid == 0){
    pid_t pid2 = fork();

    if(pid2 == 0){
      printf("Grandchild: My pid: %d\n", getpid());
    }
    else{
      waitid(P_PID, (id_t)pid2, &infop2, WEXITED);
      printf("Child: My child's PID: %d\n", pid2);
      printf("Child: My PID: %d\n", getpid());
    }
  }
  else{
    waitid(P_PID, (id_t)pid, &infop1, WEXITED);
    printf("Parent: My child's PID %d\n", pid);
    printf("Parent: My pid: %d\n", getpid());
  }
  return 0;
}

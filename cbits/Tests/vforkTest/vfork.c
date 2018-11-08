#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include <time.h>
#include <stdlib.h>

int main(void){
  printf("Parent: My pid: %d\n", getpid());
  pid_t pid = fork();

  /* Child is assigned PID = 0. */
  if(pid == 0){
    pid_t pid2 = fork();
    if(pid2 == 0){
      printf("Grandchild: My pid: %d\n", getpid());
    }
    else{
      printf("Child: My child's PID: %d\n", pid2);
      printf("Child: My PID: %d\n", getpid());
    }
  }
  else{
    /* Randomly generate a wait between 0 and 2 seconds */
    printf("Parent: My child's PID %d\n", pid);
    printf("Parent: My pid: %d\n", getpid());
  }
  return 0;
}

#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <fcntl.h>
#include <sys/stat.h>

int main(void){
  open("omar.txt", O_WRONLY, S_IWRITE);
  printf("Calling the fopen() function...\n");

  FILE *fd = fopen("test.txt", "r");
  if (!fd) {
    printf("fopen() returned NULL\n");
    return 1;
  }
  printf("fopen() succeeded\n");

  return 0;
}

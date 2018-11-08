#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <fcntl.h>
#include <sys/stat.h>

int main(void) {
  void* po = malloc(1);
  printf("Deterministic malloc using ASLR\n");
  printf("Malloc should return the same address everytime: %p\n", po);
  return 0;
}

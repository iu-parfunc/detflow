#include <stdio.h>
#include <stdlib.h>

int main(void) {
  printf("Calling the fopen() function...\n");

  FILE *fd = fopen("test.txt","r");
  if (!fd) {
    printf("fopen() returned NULL\n");
    return EXIT_SUCCESS;
  }

  printf("fopen() succeeded\n");

  return EXIT_SUCCESS;
}

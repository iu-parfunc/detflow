#include <stdio.h>

FILE *fopen(const char *path, const char *mode) {
  printf("Our custom, always-failing fopen™\n");
  return NULL;
}

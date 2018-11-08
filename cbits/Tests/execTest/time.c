#include <unistd.h>
#include <sys/time.h>
#include <stdio.h>

int main(){
  struct timeval tv;
  gettimeofday(&tv, NULL);
  printf("My time %lu\n", tv.tv_sec);

}

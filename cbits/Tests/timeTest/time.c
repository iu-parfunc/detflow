#include<stdio.h>
#include<time.h>
#include <sys/time.h>

//int gettimeofday(struct timeval *tv, struct timezone *tz);

int main(){
  time_t myTime = time(NULL);
  struct tm tm = *localtime(&myTime);

  printf("now: %d-%d-%d %d:%d:%d\n",
         tm.tm_year + 1900, tm.tm_mon + 1, tm.tm_mday, tm.tm_hour, tm.tm_min, tm.tm_sec);

  struct timeval tv;
  gettimeofday(&tv, NULL);

  printf("%s\n", tzname[0]);
  printf("Current seconds: %lu\n", tv.tv_sec);

  printf("Current time(): %lu\n", time(NULL));
  return 0;
}

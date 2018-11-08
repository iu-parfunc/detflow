#include<stdio.h>
#include<unistd.h>


int main(){
  printf("Time and date program compiled: %s %s\n", __TIME__, __DATE__);

  return 0;
}

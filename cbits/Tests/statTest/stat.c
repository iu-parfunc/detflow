#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/stat.h>

int main(void){
  struct stat buf;
  int result = stat("./hello.txt", &buf);

  if(result == 0){
    printf("Local file found!\n");
    printf("Inode value: %lu\n", buf.st_ino);
    return 0;
  }
  else{
    printf("Local file not found...\n");
    return 0;
  }
}

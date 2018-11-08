#include <time.h>
#include <stdio.h>

int main(){
  char* timezone =tzname[0];
  printf("Timezone %s\n", timezone);
}

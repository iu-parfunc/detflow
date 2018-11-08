#include<stdio.h>
#include<stdlib.h>
#include<time.h>

void testRand();
void testDevURandom();

int main(){
  testRand();
  testDevURandom();
  return 0;
}

void testRand(){
  srand(time(NULL));
  int myRand = rand();
  printf("MyRand %d\n", myRand);
}

void testDevURandom(){
  FILE* fin = fopen("/dev/urandom", "r");
  char buffer[100];
  fread(buffer, 100, sizeof(char), fin);

  printf("URandom results:\n");
  for(int i = 0; i < 100; i++)
    printf("%d ",buffer[i]);
  printf("\n");

  return;
}

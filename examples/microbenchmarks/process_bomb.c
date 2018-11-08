
#include <stdio.h>
#include <stdlib.h>
#include <cilk/cilk.h>

int count = 2000;

int main()
{
  int nworkers =  __cilkrts_get_nworkers();
  int total = count * nworkers;
  printf("Bombing the processor with lots of process creation\n");
  printf("Cilk workers: %d\n", nworkers);
  printf("Running %d subprocesses per worker\n", count);
  
  cilk_for(int i=0; i< total; i++)
  {
    int code = system("./a.out");
    if (code != 0) {
      fprintf(stderr,"ERROR: system call failed, returned code: %d\n", code);
      abort();
    }
  }

  printf("Done with all %d subprocess calls\n", total);
  return 0;
}

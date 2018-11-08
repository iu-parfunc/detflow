#include <stdio.h>
#include "dep1.h"
#include "dep2.h"
#include "dep3.h"
#include "dep4.h"
#include "dep5.h"
#include "dep6.h"
#include "dep7.h"
#include "dep8.h"

int main() {
  printf("%zu %zu %zu %zu %zu %zu %zu %zu\n",
         dep1(), dep2(), dep3(), dep4(),
         dep5(), dep6(), dep7(), dep8());
  return 0;
}

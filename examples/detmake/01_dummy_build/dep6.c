#include <stdint.h>
#include <string.h>
#include "dep6.h"

// Let's make gcc have to think for a while
#define a "xxxxxxxxxxx"
#define b a a a a a a a
#define c b b b b b b b
#define d c c c c c c c
#define e d d d d d d d
#define f e e e e e e e
#define g f f f f f f f
#define h g g g g g g g

size_t dep6(void) {
  return strlen(h);
}

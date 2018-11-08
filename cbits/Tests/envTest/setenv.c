#include <stdlib.h>

/* int setenv(const char *name, const char *value, int overwrite); */
/* int unsetenv(const char *name); */

/* Test enviornment setting function. */
int main(){
  // Under DEBUG=5 we should be able to see interception messages for all these.
  setenv("DEBUG", "bad", 1);
  setenv("DETIO_R_PERMS_LIST", "bad", 1);
  setenv("DETIO_W_PERMS_LIST", "bad", 1);
  setenv("DETIO_URAND_PATH", "bad", 1);

  // This should work just fine :)
  setenv("omar", "good", 1);

  // Same for unsetenv
  unsetenv("DEBUG");
  unsetenv("DETIO_R_PERMS_LIST");
  unsetenv("DETIO_W_PERMS_LIST");
  unsetenv("DETIO_URAND_PATH");

  // Good
  unsetenv("omar");

  return 0;
}

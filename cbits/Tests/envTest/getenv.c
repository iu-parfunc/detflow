#include <stdlib.h>

char* getenv(const char *name);
char *secure_getenv(const char *name);

/* Test enviornment reading function. */
int main(){
  // Under DEBUG=5 we should be able to see interception messages for all these.
  getenv("DEBUG");
  getenv("DETIO_R_PERMS_LIST");
  getenv("DETIO_W_PERMS_LIST");
  getenv("DETIO_URAND_PATH");

  // This should work just fine :)
  getenv("PATH");

  // Again.
  secure_getenv("DEBUG");
  secure_getenv("DETIO_R_PERMS_LIST");
  secure_getenv("DETIO_W_PERMS_LIST");
  secure_getenv("DETIO_URAND_PATH");

  // This should work just fine :)
  secure_getenv("PATH");

  return 0;
}

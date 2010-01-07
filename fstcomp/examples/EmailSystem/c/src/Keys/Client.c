#include <string.h>
#include "Client.h"

int
findUserPublicKeyPair (void *listdata, void *searchdata)
{
  return strcmp
    (((struct userPublicKeyPair *) listdata)->user,
     (char *) searchdata) ? 0 : 1;
}

int
isKeyPairValid (char *publicKey, char *privateKey) {
  return strcmp(publicKey, privateKey);
}

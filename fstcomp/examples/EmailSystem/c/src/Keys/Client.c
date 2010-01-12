#include <string.h>
#include "Client.h"

int
findUserPublicKeyPair (void *listdata, void *searchdata)
{
  if (!listdata || !searchdata)
    return -1;
  return strcmp
    (((struct userPublicKeyPair *) listdata)->user,
     (char *) searchdata) ? 0 : 1;
}

int
isKeyPairValid (char *publicKey, char *privateKey)
{
  if (!publicKey || !privateKey)
    return -1;
  return strcmp (publicKey, privateKey);
}

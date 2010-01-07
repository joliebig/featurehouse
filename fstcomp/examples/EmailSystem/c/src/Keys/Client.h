#include "slist.h"

struct client
{
  NODE *userPublicKeyPairs;
  char *privateKey;
};

struct userPublicKeyPair
{
  char *user;
  char *publicKey;
};

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "Client.h"
#include "Email.h"

int
main (void)
{
  struct client *bob = (struct client *) malloc (sizeof (struct client));
  bob->name = "bob";
  bob->privateKey = "key-bob";

  struct client *rjh = (struct client *) malloc (sizeof (struct client));
  rjh->name = "rjh";
  rjh->forwardReceiver = "chucknorris";

  struct client *chucknorris =
    (struct client *) malloc (sizeof (struct client));
  chucknorris->name = "chucknorris";
  // rjh has no private key, thus he cannot sign messages
  struct userPublicKeyPair *userPublicKeyPairBob =
    (struct userPublicKeyPair *) malloc (sizeof (struct userPublicKeyPair));
  userPublicKeyPairBob->user = "bob";
  userPublicKeyPairBob->publicKey = "key-bob";
  NODE *userPublicKeyPairsChucknorris = list_create (userPublicKeyPairBob);
  chucknorris->userPublicKeyPairs = userPublicKeyPairsChucknorris;

  struct email *mail = (struct email *) malloc (sizeof (struct email));
  mail->to = "rjh";
  mail->subject = "<some subject>";
  mail->body = "<some body>";

  outgoing (bob, mail);
  incoming (rjh, mail);
  incoming (chucknorris, (struct email *)rjh->outgoingBuffer->data);

  return 0;
}

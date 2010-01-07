//fstcomp> generated include-guard 
#ifndef __CLIENT_H
#define __CLIENT_H
//fstcomp> /generated include-guard 

#include "Email.h"
#include "slist.h"
struct  client {
  char *name; 
  NODE *outgoingBuffer; 
  NODE *userPublicKeyPairs; 
  char *privateKey; 
  char *forwardReceiver; 
  int idCounter;};


void outgoing (struct client *client, struct email *msg);


void incoming (struct client *client, struct email *msg);
struct  userPublicKeyPair {
  char *user; 
  char *publicKey;};
// TODO remove
void sign (struct client *client, struct email *msg);


// TODO remove
void forward (struct client *client, struct email *msg);
//TODO remove
void verify (struct client *client, struct email *msg);


void addMessageID (struct client *client, struct email *msg);

//fstcomp> generated include-guard 
#endif
//fstcomp> /generated include-guard 

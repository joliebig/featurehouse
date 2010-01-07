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
  NODE *addressBook; 
  int idCounter;};


void outgoing (struct client *client, struct email *msg);


void incoming (struct client *client, struct email *msg);
struct  userPublicKeyPair {
  char *user; 
  char *publicKey;};
// TODO remove
void encrypt (struct client *client, struct email *msg);
struct  addressBookEntry {
  char *alias; 
  NODE *address;};


// TODO remove after fixing the composition-function-order-problem
void resolveAlias (struct client *client, struct email *msg);
// TODO remove
void decrypt (struct client *client, struct email *msg);


void addMessageID (struct client *client, struct email *msg);

//fstcomp> generated include-guard 
#endif
//fstcomp> /generated include-guard 

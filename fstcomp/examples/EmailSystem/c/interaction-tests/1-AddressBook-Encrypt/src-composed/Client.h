//fstcomp> generated include-guard 
#ifndef __CLIENT_H
#define __CLIENT_H
//fstcomp> /generated include-guard 

#include "Email.h"
#include "slist.h"
struct client
{
  char *name;
  NODE *addressBook;
};


void outgoing (struct client *client, struct email *msg);


void incoming (struct client *client, struct email *msg);
struct addressBookEntry
{
  char *alias;
  NODE *address;
};


// TODO remove after fixing the composition-function-order-problem
void resolveAlias (struct client *client, struct email *msg);

//fstcomp> generated include-guard 
#endif
//fstcomp> /generated include-guard 

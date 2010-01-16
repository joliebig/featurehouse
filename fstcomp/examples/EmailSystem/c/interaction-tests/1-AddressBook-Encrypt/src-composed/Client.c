#include <stdlib.h>

#include <stdio.h>

#include <string.h>

#include "Client.h"


// outgoing emails leave the client at this point. here they are put in an outgoing queue instead.
void
mail (struct client *client, struct email *msg)
{
  //TODO 
}


// emails to be sent are processed by this method before beeing mailed.
void
outgoing__wrappee__SingleLinkedList (struct client *client, struct email *msg)
{
  mail (client, msg);
}


void
outgoing (struct client *client, struct email *msg)
{
  resolveAlias (client, msg);
  outgoing__wrappee__SingleLinkedList (client, msg);
}


// incoming emails reach the user at this point. here they are put in a mailbox.
void
deliver (struct client *client, struct email *msg)
{
  //TODO
}


// incoming emails are processed by this method before delivery.
void
incoming (struct client *client, struct email *msg)
{
  deliver (client, msg);
}

int
findAddressBookEntry (void *listdata, void *searchdata)
{
  return strcmp
    ((struct addressBookEntry *) listdata->alias,
     (char *) searchdata) ? 0 : 1;
}


void
resolveAlias (struct client *client, struct email *msg)
{
  struct email *clone = cloneEmail (msg);
  NODE *found =
    list_find (client->addressBook, findAddressBookEntry, clone->to);
  if (!found)
    return;
  printf ("alias: %s\n", ((struct addressBookEntry *) found->data)->alias);
  NODE *address = ((struct addressBookEntry *) found->data)->address;
  if (address)
    {
      msg->to = strdup (address->data);
      printf ("address: %s\n", (char *) (address->data));
      address = address->next;
    }
  while (address)
    {
      struct email *newmsg = cloneEmail (clone);
      newmsg->to = strdup (address->data);
      printf ("address: %s\n", (char *) (address->data));
      address = address->next;
      outgoing (client, newmsg);
    }
}

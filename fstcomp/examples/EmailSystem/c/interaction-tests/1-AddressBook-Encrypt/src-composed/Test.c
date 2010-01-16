#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "Client.h"
#include "Email.h"

int
main (void)
{
  struct client *bob = (struct client *) malloc (sizeof (struct client));
  bob->name = "bob";
  struct client *rjh = (struct client *) malloc (sizeof (struct client));
  rjh->name = "rjh";

  //char keysBob[][MAX] = { "rjh" };
  //setKeys (bob, *keysBob, 1);

  struct addressBookEntry *addressBookEntry =
    (struct addressBookEntry *) malloc (sizeof (struct addressBookEntry));
  addressBookEntry->alias = "someAlias";
  addressBookEntry->address = list_create ("rjh");
  addressBookEntry->address =
    list_insert_beginning (addressBookEntry->address, "chucknorris");
  addressBookEntry->address =
    list_insert_beginning (addressBookEntry->address, "mcguyver");
  NODE *addressBook = list_create (addressBookEntry);

  bob->addressBook = addressBook;

  struct email *bobToAlias = (struct email *) malloc (sizeof (struct email));
  bobToAlias->from = "bob";
  bobToAlias->to = "someAlias";
  bobToAlias->subject = "<some subject>";
  bobToAlias->body = "<some body>";
  outgoing (bob, bobToAlias);

/*
  struct emailQueueElement *element = (*(*bob).sendBuffer).head;
  printf ("----\n");
  printf ("- SEND BUFFER BOB\n");
  while (element != NULL)
    {
      printf ("----\n");
      printMail ((*element).email);
      element = (*element).previous;
    }
*/
}

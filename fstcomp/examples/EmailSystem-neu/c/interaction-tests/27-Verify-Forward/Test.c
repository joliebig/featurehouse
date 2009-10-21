#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "Client.h"
#ifndef Email_h
#define Email_h
#include "Email.h"
#endif
#ifndef EmailQueue_h
#define EmailQueue_h
#include "EmailQueue.h"
#endif

int
main (void)
{
  struct client *bob = (struct client *) malloc (sizeof (struct client));
  init (bob);
  struct client *rjh = (struct client *) malloc (sizeof (struct client));
  init (rjh);

  char signatureReceiversBob[][MAX] = { "rjh" };
  setSignatureReceivers (bob, *signatureReceiversBob, 1);

  struct email bobToRjh =
    { "bob", "rjh", "<cc>", "<some subject>", "<some body>" };
  outgoing (bob, &bobToRjh);

  struct emailQueueElement *element = (*(*bob).sendBuffer).head;
  printf ("----\n");
  printf ("- SEND BUFFER BOB\n");
  while (element != NULL)

    {
      printf ("----\n");
      printMail ((*element).email);
      element = (*element).previous;
    }

  incoming (rjh, (*(*(*bob).sendBuffer).head).email);

  element = (*(*rjh).sendBuffer).head;
  printf ("----\n");
  printf ("- SEND BUFFER RJH\n");
  while (element != NULL)

    {
      printf ("----\n");
      printMail ((*element).email);
      element = (*element).previous;
    }

  strcat ((*(*(*(*bob).sendBuffer).head).email).from, " manipulated");
  printf ("----\n");
  printf ("- FORWARDED MESSAGE ALTERED ON WAY TO RECEIVER\n");
  printf ("----\n");
  printMail ((*(*(*bob).sendBuffer).head).email);

  return 0;
}

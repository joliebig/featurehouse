#include "Client.h"

void
printMail (struct email *msg)
{
  printf ("FROM:\n%s\n", (*msg).from);
  printf ("TO:\n%s\n", (*msg).to);
  printf ("CC:\n%s\n", (*msg).cc);
  printf ("SUBJECT:\n%s\n", (*msg).subject);
  printf ("BODY:\n%s\n", (*msg).body);
}

int
main (void)
{
  init();

  struct email outgoingMail =
    { "<sender>", "<receiver>", "<cc>", "<some subject>", "<some body>" };
  printf ("* NEW OUTGOING MAIL\n");
  outgoing (&outgoingMail);

  struct email incomingMail =
    { "<sender>", "<receiver>", "<cc>", "<some subject>",
    "[encrypted]\n[valid-signature]\n<some body>"
  };
  printf ("* NEW INCOMING MAIL\n");
  incoming (&incomingMail);

  struct emailQueueElement *element = (*inbox).head;
  printf("----\n");
  printf("- INBOX\n");
  while (element != NULL)
  {
    printf("----\n");
    printMail((*element).email);
    element = (*element).previous;
  }

  element = (*sendBuffer).head;
  printf("----\n");
  printf("- SEND BUFFER\n");
  while (element != NULL)
  {
    printf("----\n");
    printMail((*element).email);
    element = (*element).previous;
  }
  return 0;
}

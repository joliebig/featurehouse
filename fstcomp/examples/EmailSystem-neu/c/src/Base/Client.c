#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#ifndef Client_h
#define Client_h
#include "Client.h"
#endif

void
init (struct client *client)
{
  (*client).inbox = createEmailQueue ();
  (*client).sendBuffer = createEmailQueue ();
}

// outgoing emails leave the client at this point. here they are put in an outgoing queue instead.
void
mail (struct client *client, struct email *msg)
{
  //printf ("* NEW MAIL\n");

  //printMail (msg);
  offerEmailQueue ((*client).sendBuffer, msg);
}

// emails to be sent are processed by this method before beeing mailed.
void
outgoing (struct client *client, struct email *msg)
{

  //printf ("* NEW OUTGOING MAIL\n");
  //printMail (msg);
  //sign (msg);
  //encrypt (msg);
  mail (client, msg);
}

// incoming emails reach the user at this point. here they are put in a mailbox.
void
deliver (struct client *client, struct email *msg)
{
  //printf ("* NEW DELIVERED MAIL\n");

  //printMail (msg);
  offerEmailQueue ((*client).inbox, msg);
}

// incoming emails are processed by this method before delivery.
void
incoming (struct client *client, struct email *msg)
{

  //printf ("* NEW INCOMING MAIL\n");
  //printMail (msg);
  //decrypt (msg);
  //verify (msg);
  //forward (msg);
  deliver (client, msg);
}

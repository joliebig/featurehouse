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
  offerEmailQueue ((*client).sendBuffer, msg);
}

// emails to be sent are processed by this method before beeing mailed.
void
outgoing__wrappee__Base (struct client *client, struct email *msg)
{
  mail (client, msg);
}

void
outgoing (struct client *client, struct email *msg)
{
  encrypt (client, msg);
  outgoing__wrappee__Base (client, msg);
}

// incoming emails reach the user at this point. here they are put in a mailbox.
void
deliver (struct client *client, struct email *msg)
{
  offerEmailQueue ((*client).inbox, msg);
}

// incoming emails are processed by this method before delivery.
void
incoming__wrappee__Encrypt (struct client *client, struct email *msg)
{
  deliver (client, msg);
}

void
incoming__wrappee__Forward (struct client *client, struct email *msg)
{
  forward (client, msg);
  incoming__wrappee__Encrypt (client, msg);
}

void
incoming (struct client *client, struct email *msg)
{
  decrypt (client, msg);
  incoming__wrappee__Forward (client, msg);
}

// adds the encrypt flag to message body
void
encrypt (struct client *client, struct email *msg)
{
  int i;
  for (i = 0; i < (*client).keysSize; i++)
    {
      if (strncmp (((*client).keys + i * MAX), (*msg).to, strlen ((*msg).to))
	  == 0)
	{
	  printf ("> encrypted\n");
	  char newBody[MAX] = "[encrypted]\n";
	  strcat (newBody, (*msg).body);
	  strcpy ((*msg).body, newBody);
	  break;
	}
    }
}

void
setKeys (struct client *client, char *keys, int size)
{
  (*client).keys = keys;
  (*client).keysSize = size;
}

void
forward (struct client *client, struct email *msg)
{
  printf ("> forwarded\n");
  struct email *clone = cloneEmail (msg);
  strcpy ((*clone).from, "<forwarder>");
  strcpy ((*clone).to, "<forward-receiver>");
  outgoing (client, clone);
}

// removes the decryption flag if possible
void
decrypt (struct client *client, struct email *msg)
{
  char key[] = "[encrypted]\n";
  if (strncmp (key, (*msg).body, strlen (key)) == 0)
    {
      printf ("> decrypted\n");
      strcpy ((*msg).body, &(*msg).body[strlen (key)]);
    }
  else
    {
      printf ("> not decrypted\n");
    }
}

#include <stdio.h>

#include <stdlib.h>

#include <string.h>

#include "Email.h"


struct email *
cloneEmail__wrappee__Keys (struct email *msg)
{
  struct email *clone = (struct email *) malloc (sizeof (struct email));
  if (msg->id)
    clone->id = msg->id;
  if (msg->from)
    clone->from = strdup (msg->from);
  if (msg->to)
    clone->to = strdup (msg->to);
  if (msg->subject)
    clone->subject = strdup (msg->subject);
  if (msg->body)
    clone->body = strdup (msg->body);
  return clone;
}


struct email *
cloneEmail (struct email *msg)
{
  struct email *clone = cloneEmail__wrappee__Keys (msg);
  clone->isEncrypted = msg->isEncrypted;
  if (msg->encryptionKey)
    clone->encryptionKey = strdup (msg->encryptionKey);
  return clone;
}


void
printMail__wrappee__Keys (struct email *msg)
{
  printf ("ID:\n  %i\n", msg->id);
  printf ("FROM:\n  %s\n", msg->from);
  printf ("TO:\n  %s\n", msg->to);
  printf ("SUBJECT:\n  %s\n", msg->subject);
  printf ("IS_READABLE\n  %i\n", isReadable (msg));
  printf ("BODY:\n  %s\n", msg->body);
}

void
printMail (struct email *msg)
{
  printMail__wrappee__Keys (msg);
  printf ("ENCRYPTED\n  %i\n", msg->isEncrypted);
  printf ("ENCRYPTION KEY\n  %s\n", msg->encryptionKey);
}


int
isReadable__wrappee__Keys (struct email *msg)
{
  return 1;
}


int
isReadable (struct email *msg)
{
  if (0 == isEncrypted (msg))
    return isReadable__wrappee__Keys (msg);
  else
    return 0;
}


int
isEncrypted (struct email *msg)
{
  return msg->isEncrypted;
}

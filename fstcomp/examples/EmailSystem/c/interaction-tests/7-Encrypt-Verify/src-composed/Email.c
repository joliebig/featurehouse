#include <stdio.h>

#include <stdlib.h>

#include <string.h>

#include "Email.h"


struct email *
cloneEmail__wrappee__Keys (struct email *msg)
{
  struct email *clone = (struct email *) malloc (sizeof (struct email));
  if (msg->id)
    clone->id = strdup (msg->id);
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
cloneEmail__wrappee__Encrypt (struct email *msg)
{
  struct email *clone = cloneEmail__wrappee__Keys (msg);
  clone->isEncrypted = msg->isEncrypted;
  if (msg->encryptionKey)
    clone->encryptionKey = strdup (msg->encryptionKey);
  return clone;
}


struct email *
cloneEmail__wrappee__Sign (struct email *msg)
{
  struct email *clone = cloneEmail__wrappee__Encrypt (msg);
  clone->isSigned = msg->isSigned;
  if (msg->signKey)
    clone->signKey = strdup (msg->signKey);
  return clone;
}


struct email *
cloneEmail (struct email *msg)
{
  struct email *clone = cloneEmail__wrappee__Sign (msg);
  clone->isSignatureVerified = msg->isSignatureVerified;
  return clone;
}


void
printMail__wrappee__Keys (struct email *msg)
{
  printf ("ID:\n  %s\n", msg->id);
  printf ("FROM:\n  %s\n", msg->from);
  printf ("TO:\n  %s\n", msg->to);
  printf ("SUBJECT:\n  %s\n", msg->subject);
  printf ("IS_READABLE\n  %i\n", isReadable (msg));
  printf ("BODY:\n  %s\n", msg->body);
}

void
printMail__wrappee__Encrypt (struct email *msg)
{
  printMail__wrappee__Keys (msg);
  printf ("ENCRYPTED\n  %i\n", msg->isEncrypted);
  printf ("ENCRYPTION KEY\n  %s\n", msg->encryptionKey);
}

void
printMail__wrappee__Sign (struct email *msg)
{
  printMail__wrappee__Encrypt (msg);
  printf ("SIGNED\n  %i\n", msg->isSigned);
  printf ("SIGNATURE\n  %s\n", msg->signKey);
}

void
printMail (struct email *msg)
{
  printMail__wrappee__Sign (msg);
  printf ("SIGNATURE VERIFIED\n  %i\n", msg->isSignatureVerified);
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


int
isSigned (struct email *msg)
{
  return msg->isSigned;
}


int
isVerified (struct email *msg)
{
  return msg->isSignatureVerified;
}

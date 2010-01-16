#include <stdio.h>

#include <stdlib.h>

#include <string.h>

#include "Email.h"


void
printMail (struct email *msg)
{
  printf ("MSG_ID:\n  %i\n", msg->msgID);
  printf ("FROM:\n  %s\n", msg->from);
  printf ("TO:\n  %s\n", msg->to);
  printf ("SUBJECT:\n  %s\n", msg->subject);
  printf ("IS_READABLE\n  %i\n", isReadable (msg));
  printf ("BODY:\n  %s\n", msg->body);
}


int
isReadable (struct email *msg)
{
  return 1;
}


struct email *
cloneEmail (struct email *msg)
{
  struct email *clone = (struct email *) malloc (sizeof (struct email));
  clone->msgID = msg->msgID;
  clone->from = strdup (msg->from);
  clone->to = strdup (msg->to);
  clone->subject = strdup (msg->subject);
  clone->body = strdup (msg->body);
  return clone;
}

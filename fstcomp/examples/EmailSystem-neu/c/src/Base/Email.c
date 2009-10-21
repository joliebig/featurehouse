#include <stdio.h>

#ifndef Email_h
#define Email_h
#include "Email.h"
#endif

void
printMail (struct email *msg)
{
  printf ("FROM:\n%s\n", (*msg).from);
  printf ("TO:\n%s\n", (*msg).to);
  printf ("CC:\n%s\n", (*msg).cc);
  printf ("SUBJECT:\n%s\n", (*msg).subject);
  printf ("BODY:\n%s\n", (*msg).body);
}

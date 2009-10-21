#include <stdio.h>

#ifndef Email_h
#define Email_h
#include "Email.h"
#endif

void
printMail (struct email *msg)
{ printf ("FROM:\n%s\n", (*msg).from); printf ("TO:\n%s\n", (*msg).to); printf ("CC:\n%s\n", (*msg).cc); printf ("SUBJECT:\n%s\n", (*msg).subject); printf ("BODY:\n%s\n", (*msg).body);
}
#include <stdlib.h>

#include <string.h>

struct email *
cloneEmail (struct email *msg)
{ struct email *clone = (struct email *) malloc (sizeof (struct email)); strcpy ((*clone).from, (*msg).from); strcpy ((*clone).to, (*msg).to); strcpy ((*clone).cc, (*msg).cc); strcpy ((*clone).subject, (*msg).subject); strcpy ((*clone).body, (*msg).body); return clone;
}

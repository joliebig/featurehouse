#include <stdlib.h>
#include <string.h>

struct email *
cloneEmail (struct email *msg)
{
  struct email *clone = (struct email *) malloc (sizeof (struct email));
  strcpy ((*clone).from, (*msg).from);
  strcpy ((*clone).to, (*msg).to);
  strcpy ((*clone).cc, (*msg).cc);
  strcpy ((*clone).subject, (*msg).subject);
  strcpy ((*clone).body, (*msg).body);
  return clone;
}

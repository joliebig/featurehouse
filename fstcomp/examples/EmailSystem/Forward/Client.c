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

void
forward (struct email * msg)
{
  printf ("> forwarded\n");
  struct email *clone = cloneEmail (msg);
  strcpy ((*clone).from, "<forwarder>");
  strcpy ((*clone).to, "<forward-receiver>");
  outgoing (clone);
}

void
incoming (struct email *msg)
{
  forward (msg);
  original (msg);
}
void
forward (struct client *client, struct email *msg)
{
  printf ("> forwarded\n");
  struct email *clone = cloneEmail (msg);
  strcpy ((*clone).from, "<forwarder>");
  strcpy ((*clone).to, "<forward-receiver>");
  outgoing (client, clone);
}

void
incoming (struct client *client, struct email *msg)
{
  forward (client, msg);
  original (client, msg);
}

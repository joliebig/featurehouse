// adds the encrypt flag to message body
void
encrypt (struct email *msg)
{
  printf ("> encrypted\n");
  char newBody[MAX] = "[encrypted]\n";
  strcat (newBody, (*msg).body);
  strcpy ((*msg).body, newBody);
}

void
outgoing (struct email *msg)
{
  encrypt (msg);
  original (msg);
}
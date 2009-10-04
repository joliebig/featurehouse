// adds the sign flag to message body
void
sign (struct email *msg)
{
  printf ("> signed\n");
  char newBody[MAX] = "[valid-signature]\n";
  strcat (newBody, (*msg).body);
  strcpy ((*msg).body, newBody);
}

void
outgoing (struct email *msg)
{
  sign (msg);
  original (msg);
}
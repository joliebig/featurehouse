// checks for a valid signature and replaces it by a flag signaling a verified signature
void
verify (struct client *client, struct email *msg)
{
  char key[] = "[valid-signature]\n";
  if (strncmp (key, (*msg).body, strlen (key)) == 0)

    {
      printf ("> verified\n");
      char newBody[MAX] = "[signature verified]\n";
      strcat (newBody, &(*msg).body[strlen (key)]);
      strcpy ((*msg).body, newBody);
    }

  else

    {
      printf ("> not verified\n");
    }
}

void
incoming (struct client *client, struct email *msg)
{
  verify (client, msg);
  original (client, msg);
}

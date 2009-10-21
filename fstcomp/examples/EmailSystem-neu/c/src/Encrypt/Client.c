// adds the encrypt flag to message body
void
encrypt (struct client *client, struct email *msg)
{
  int i;
  for (i = 0; i < (*client).keysSize; i++)
    {
      //printf("%s\n", ((*client).keys + i * MAX));
      if (strncmp (((*client).keys + i * MAX), (*msg).to, strlen ((*msg).to))
	  == 0)
	{
	  printf ("> encrypted\n");
	  char newBody[MAX] = "[encrypted]\n";
	  strcat (newBody, (*msg).body);
	  strcpy ((*msg).body, newBody);
	  break;
	}
    }
}

void
outgoing (struct client *client, struct email *msg)
{
  encrypt (client, msg);
  original (client, msg);
}

void
setKeys (struct client *client, char *keys, int size)
{
  (*client).keys = keys;
  (*client).keysSize = size;
}

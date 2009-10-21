// adds the sign flag to message body
void
sign (struct client *client, struct email *msg)
{
  int i;
  for (i = 0; i < (*client).signatureReceiversSize; i++)
    {
      //printf("%s\n", ((*client).keys + i * MAX));
      if (strncmp
	  (((*client).signatureReceivers + i * MAX), (*msg).to,
	   strlen ((*msg).to)) == 0)
	{
	  printf ("> signed\n");
	  char newBody[MAX] = "[valid-signature]\n";
	  strcat (newBody, (*msg).body);
	  strcpy ((*msg).body, newBody);
	  break;
	}
    }
}

void
setSignatureReceivers (struct client *client, char *receivers, int size)
{
  (*client).signatureReceivers = receivers;
  (*client).signatureReceiversSize = size;
}

void
outgoing (struct client *client, struct email *msg)
{
  sign (client, msg);
  original (client, msg);
}

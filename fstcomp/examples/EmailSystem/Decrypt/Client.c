// removes the decryption flag if possible
void
decrypt (struct email *msg)
{
  char key[] = "[encrypted]\n";
  if (strncmp (key, (*msg).body, strlen (key)) == 0)
    {
      printf ("> decrypted\n");
      strcpy ((*msg).body, &(*msg).body[strlen (key)]);
    }
  else
    {
      printf ("> not decrypted\n");
    }
}

void
incoming (struct email *msg)
{
  decrypt (msg);
  original (msg);
}
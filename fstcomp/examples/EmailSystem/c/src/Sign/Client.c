// adds the sign flag to message body
void
sign (struct client *client, struct email *msg)
{
  if (!client->privateKey)
    return;
  msg->signKey = strdup (client->privateKey);
  msg->isSigned = 1;
}

void
outgoing (struct client *client, struct email *msg)
{
  sign (client, msg);
  original (client, msg);
}

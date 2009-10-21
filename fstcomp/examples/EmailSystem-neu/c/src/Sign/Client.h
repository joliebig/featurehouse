void sign (struct client *client, struct email *msg);

void setSignatureReceivers (struct client *client, char *receivers, int size);

struct client
{
  char *signatureReceivers;
  int signatureReceiversSize;
};

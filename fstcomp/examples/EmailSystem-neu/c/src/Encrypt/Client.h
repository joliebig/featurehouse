void encrypt (struct client *client, struct email *msg);

void setKeys (struct client *client, char *keys, int size);

struct client
{
  char *keys;
  int keysSize;
};

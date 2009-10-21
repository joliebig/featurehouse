#ifndef Email_h
#define Email_h
#include "Email.h"
#endif

#ifndef EmailQueue_h
#define EmailQueue_h
#include "EmailQueue.h"
#endif
struct  client { struct emailQueue *inbox;  struct emailQueue *sendBuffer;  char *signatureReceivers;  int signatureReceiversSize;};

void init (struct client *client);

void mail (struct client *client, struct email *msg);

void outgoing (struct client *client, struct email *msg);

void deliver (struct client *client, struct email *msg);

void incoming (struct client *client, struct email *msg);
void sign (struct client *client, struct email *msg);

void setSignatureReceivers (struct client *client, char *receivers, int size);
void forward (struct client *client, struct email *msg);
void verify (struct client *client, struct email *msg);

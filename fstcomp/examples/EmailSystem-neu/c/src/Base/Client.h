#ifndef Email_h
#define Email_h
#include "Email.h"
#endif
#ifndef EmailQueue_h
#define EmailQueue_h
#include "EmailQueue.h"
#endif

/*
 * two sorts of emails are processed by this client: incoming and outgoing ones.
 * control flow is
 * email from internet > incoming > deliver > receipient receives mail
 * sender writes email > outgoing > mail > email sent through internet
 */
struct client
{
  struct emailQueue *inbox;
  struct emailQueue *sendBuffer;
};

void init (struct client *client);

void mail (struct client *client, struct email *msg);

void outgoing (struct client *client, struct email *msg);

void deliver (struct client *client, struct email *msg);

void incoming (struct client *client, struct email *msg);

#ifndef Email_h
#define Email_h
#include "Email.h"
#endif

#include <stdlib.h>
struct emailQueue
{
  struct emailQueueElement *head;
  struct emailQueueElement *tail;
};
struct emailQueueElement
{
  struct emailQueueElement *previous;
  struct email *email;
};

struct emailQueue *createEmailQueue ();

void offerEmailQueue (struct emailQueue *queue, struct email *msg);

struct email *pollQueue (struct emailQueue *queue);

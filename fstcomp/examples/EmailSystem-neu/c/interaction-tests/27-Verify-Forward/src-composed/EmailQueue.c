#ifndef EmailQueue_h
#define EmailQueue_h
#include "EmailQueue.h"
#endif

struct emailQueueElement *
createEmailQueueElement (struct email *msg)
{ struct emailQueueElement *element = (struct emailQueueElement *) malloc (sizeof (struct emailQueueElement)); (*element).email = msg; (*element).previous = NULL; return element;
}

struct emailQueue *
createEmailQueue ()
{ struct emailQueue *queue = (struct emailQueue *) malloc (sizeof (struct emailQueue)); (*queue).head = NULL; (*queue).tail = NULL; return queue;
}

void
offerEmailQueue (struct emailQueue *queue, struct email *msg)
{ struct emailQueueElement *element = createEmailQueueElement (msg); if ((*queue).tail != NULL) (*(*queue).tail).previous = element; else (*queue).head = element; (*queue).tail = element;
}

struct email *
pollQueue (struct emailQueue *queue)
{ struct emailQueueElement *element = (*queue).head; (*queue).head = (*element).previous; struct email *msg = (*element).email; free (element); return msg;
}

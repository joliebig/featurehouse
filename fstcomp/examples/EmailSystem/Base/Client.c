#include "Client.h"

struct emailQueueElement *
createEmailQueueElement (struct email *msg)
{
  struct emailQueueElement *element =
    (struct emailQueueElement *) malloc (sizeof (struct emailQueueElement));
  (*element).email = msg;
  (*element).previous = NULL;
  return element;
}

struct emailQueue *
createEmailQueue ()
{
  struct emailQueue *queue = (struct emailQueue *) malloc (sizeof (struct emailQueue));
  (*queue).head = NULL;
  (*queue).tail = NULL;
  return queue;
}

void
offerEmailQueue (struct emailQueue *queue, struct email *msg)
{
  struct emailQueueElement *element = createEmailQueueElement (msg);
  if ((*queue).tail != NULL)
    (*(*queue).tail).previous = element;
  else
    (*queue).head = element;
  (*queue).tail = element;
}

void
init ()
{
  inbox = createEmailQueue();
  sendBuffer = createEmailQueue();
}

struct email *
pollQueue (struct emailQueue *queue)
{
  struct emailQueueElement *element = (*queue).head;
  (*queue).head = (*element).previous;
  struct email *msg = (*element).email;
  free (element);
  return msg;
}

// outgoing emails leave the client at this point. here they are put in an outgoing queue instead.
void 
mail (struct email *msg)
{
  printf ("* NEW MAIL\n");
  //printMail (msg);
  offerEmailQueue (sendBuffer, msg);
}

// emails to be sent are processed by this method before beeing mailed.
void 
outgoing (struct email *msg)
{
  //printf ("* NEW OUTGOING MAIL\n");
  //printMail (msg);
  //sign (msg);
  //encrypt (msg);
  mail (msg);
}

// incoming emails reach the user at this point. here they are put in a mailbox.
void
deliver (struct email *msg)
{
  printf ("* NEW DELIVERED MAIL\n");
  //printMail (msg);
  offerEmailQueue (inbox, msg);
}

// incoming emails are processed by this method before delivery.
void
incoming (struct email *msg)
{
  //printf ("* NEW INCOMING MAIL\n");
  //printMail (msg);
  //decrypt (msg);
  //verify (msg);
  //forward (msg);
  deliver (msg);
}

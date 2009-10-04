#include <stdlib.h>
#include <stdio.h>
#include <string.h>

/*
 * two sorts of emails are processed by this client: incoming and outgoing ones.
 * control flow is
 * email from internet > incoming > deliver > receipient receives mail
 * sender writes email > outgoing > mail > email sent through internet
 */
#define MAX 2000

struct email
{
  char from[MAX];
  char to[MAX];
  char cc[MAX];
  char subject[MAX];
  char body[MAX];
};

struct emailQueueElement
{
  struct emailQueueElement *previous;
  struct email *email;
};

struct emailQueue
{
  struct emailQueueElement *head;
  struct emailQueueElement *tail;
};

struct emailQueue *inbox;
struct emailQueue *sendBuffer;

void
init ();

void 
mail (struct email *msg);

void 
outgoing (struct email *msg);

void
deliver (struct email *msg);

void
incoming (struct email *msg);
#include <stdlib.h>

#include <stdio.h>

#include <string.h>

#define MAX 2000

struct email
{ char from[MAX]; char to[MAX]; char cc[MAX]; char subject[MAX]; char body[MAX];
};

struct emailQueueElement
{ struct emailQueueElement *previous; struct email *email;
};

struct emailQueue
{ struct emailQueueElement *head; struct emailQueueElement *tail;
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
void
encrypt (struct email *msg);
void
sign (struct email *msg);
void
forward (struct email * msg);
void
verify (struct email *msg);
void
decrypt (struct email *msg);

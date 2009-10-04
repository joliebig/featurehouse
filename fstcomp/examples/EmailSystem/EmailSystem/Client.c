#include "Client.h"

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

void
init ()
{ inbox = createEmailQueue(); sendBuffer = createEmailQueue();
}

struct email *
pollQueue (struct emailQueue *queue)
{ struct emailQueueElement *element = (*queue).head; (*queue).head = (*element).previous; struct email *msg = (*element).email; free (element); return msg;
}
// outgoing emails leave the client at this point. here they are put in an outgoing queue instead.
void
mail (struct email *msg)
{ printf ("* NEW MAIL\n"); offerEmailQueue (sendBuffer, msg);
}
// emails to be sent are processed by this method before beeing mailed.
void
outgoing__wrappee__Base (struct email *msg)
{ mail (msg);
}

void
outgoing__wrappee__Encrypt (struct email *msg)
{ encrypt (msg); outgoing__wrappee__Base(msg);
}

void
outgoing (struct email *msg)
{ sign (msg); outgoing__wrappee__Encrypt(msg);
}
// incoming emails reach the user at this point. here they are put in a mailbox.
void
deliver (struct email *msg)
{ printf ("* NEW DELIVERED MAIL\n"); offerEmailQueue (inbox, msg);
}
// incoming emails are processed by this method before delivery.
void
incoming__wrappee__Sign (struct email *msg)
{ deliver (msg);
}

void
incoming__wrappee__Forward (struct email *msg)
{ forward (msg); incoming__wrappee__Sign(msg);
}

void
incoming__wrappee__Verify (struct email *msg)
{ verify (msg); incoming__wrappee__Forward(msg);
}

void
incoming (struct email *msg)
{ decrypt (msg); incoming__wrappee__Verify(msg);
}
// adds the encrypt flag to message body
void
encrypt (struct email *msg)
{ printf ("> encrypted\n"); char newBody[MAX] = "[encrypted]\n"; strcat (newBody, (*msg).body); strcpy ((*msg).body, newBody);
}
// adds the sign flag to message body
void
sign (struct email *msg)
{ printf ("> signed\n"); char newBody[MAX] = "[valid-signature]\n"; strcat (newBody, (*msg).body); strcpy ((*msg).body, newBody);
}
struct email *
cloneEmail (struct email *msg)
{ struct email *clone = (struct email *) malloc (sizeof (struct email)); strcpy ((*clone).from, (*msg).from); strcpy ((*clone).to, (*msg).to); strcpy ((*clone).cc, (*msg).cc); strcpy ((*clone).subject, (*msg).subject); strcpy ((*clone).body, (*msg).body); return clone;
}

void
forward (struct email * msg)
{ printf ("> forwarded\n"); struct email *clone = cloneEmail (msg); strcpy ((*clone).from, "<forwarder>"); strcpy ((*clone).to, "<forward-receiver>"); outgoing (clone);
}
// checks for a valid signature and replaces it by a flag signaling a verified signature
void
verify (struct email *msg)
{ char key[] = "[valid-signature]\n"; if (strncmp (key, (*msg).body, strlen (key)) == 0) { printf ("> verified\n"); char newBody[MAX] = "[signature verified]\n"; strcat (newBody, &(*msg).body[strlen (key)]); strcpy ((*msg).body, newBody); } else { printf ("> not verified\n"); }
}
// removes the decryption flag if possible
void
decrypt (struct email *msg)
{ char key[] = "[encrypted]\n"; if (strncmp (key, (*msg).body, strlen (key)) == 0) { printf ("> decrypted\n"); strcpy ((*msg).body, &(*msg).body[strlen (key)]); } else { printf ("> not decrypted\n"); }
}

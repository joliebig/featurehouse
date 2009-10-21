#include <stdlib.h>

#include <stdio.h>

#include <string.h>

#ifndef Client_h
#define Client_h
#include "Client.h"
#endif

void
init (struct client *client)
{ (*client).inbox = createEmailQueue (); (*client).sendBuffer = createEmailQueue ();
}
// outgoing emails leave the client at this point. here they are put in an outgoing queue instead.
void
mail (struct client *client, struct email *msg)
{ offerEmailQueue ((*client).sendBuffer, msg);
}
// emails to be sent are processed by this method before beeing mailed.
void
outgoing__wrappee__Base (struct client *client, struct email *msg)
{ mail (client, msg);
}

void
outgoing (struct client *client, struct email *msg)
{ sign (client, msg); outgoing__wrappee__Base(client, msg);
}
// incoming emails reach the user at this point. here they are put in a mailbox.
void
deliver (struct client *client, struct email *msg)
{ offerEmailQueue ((*client).inbox, msg);
}
// incoming emails are processed by this method before delivery.
void
incoming__wrappee__Sign (struct client *client, struct email *msg)
{ deliver (client, msg);
}

void
incoming__wrappee__Forward (struct client *client, struct email *msg)
{ forward (client, msg); incoming__wrappee__Sign(client, msg);
}

void
incoming (struct client *client, struct email *msg)
{ verify (client, msg); incoming__wrappee__Forward(client, msg);
}
// adds the sign flag to message body
void
sign (struct client *client, struct email *msg)
{ int i; for (i = 0; i < (*client).signatureReceiversSize; i++) { if (strncmp (((*client).signatureReceivers + i * MAX), (*msg).to, strlen ((*msg).to)) == 0)	{ printf ("> signed\n"); char newBody[MAX] = "[valid-signature]\n"; strcat (newBody, (*msg).body); strcpy ((*msg).body, newBody); break;	} }
}

void
setSignatureReceivers (struct client *client, char *receivers, int size)
{ (*client).signatureReceivers = receivers; (*client).signatureReceiversSize = size;
}
void
forward (struct client *client, struct email *msg)
{ printf ("> forwarded\n"); struct email *clone = cloneEmail (msg); strcpy ((*clone).from, "<forwarder>"); strcpy ((*clone).to, "<forward-receiver>"); outgoing (client, clone);
}
// checks for a valid signature and replaces it by a flag signaling a verified signature
void
verify (struct client *client, struct email *msg)
{ char key[] = "[valid-signature]\n"; if (strncmp (key, (*msg).body, strlen (key)) == 0) { printf ("> verified\n"); char newBody[MAX] = "[signature verified]\n"; strcat (newBody, &(*msg).body[strlen (key)]); strcpy ((*msg).body, newBody); } else { printf ("> not verified\n"); }
}

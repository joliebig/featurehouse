#include <stdlib.h>

#include <stdio.h>

#include <string.h>
#include "Client.h"


// outgoing emails leave the client at this point. here they are put in an outgoing queue instead.
void 
mail__wrappee__Keys (struct client *client, struct email *msg)
{
  //TODO 
  printf ("=> %s MAIL\n", client->name);
  printMail (msg);
  if (!client->outgoingBuffer)
    client->outgoingBuffer = list_create (msg);
  else
    list_insert_after (client->outgoingBuffer, msg);
}


void 
mail__wrappee__Forward (struct client *client, struct email *msg)
{
// VERIFICATION HOOK
  int verificationHook_isSigned = isSigned (msg);
// VERIFICATION HOOK END
  mail__wrappee__Keys(client, msg);
}


void
mail (struct client *client, struct email *msg)
{
// VERIFICATION HOOK
  int verificationHook_isVerified = isVerified (msg);
// VERIFICATION HOOK END
  mail__wrappee__Forward(client, msg);
}


// emails to be sent are processed by this method before beeing mailed.
void 
outgoing__wrappee__Keys (struct client *client, struct email *msg)
{
  msg->from = strdup (client->name);
  mail (client, msg);
}


void 
outgoing__wrappee__Verify (struct client *client, struct email *msg)
{
  sign (client, msg);
  outgoing__wrappee__Keys(client, msg);
}



void
outgoing (struct client *client, struct email *msg)
{
  addMessageID (client, msg);
  outgoing__wrappee__Verify(client, msg);
}


// incoming emails reach the user at this point. here they are put in a mailbox.
void
deliver (struct client *client, struct email *msg)
{
  //TODO
  printf ("=> %s DELIVER\n", client->name);
  printMail (msg);
}


// incoming emails are processed by this method before delivery.
void 
incoming__wrappee__Sign (struct client *client, struct email *msg)
{
  deliver (client, msg);
}


void 
incoming__wrappee__Forward (struct client *client, struct email *msg)
{
  forward (client, msg);
  incoming__wrappee__Sign(client, msg);
}


void
incoming (struct client *client, struct email *msg)
{
  verify (client, msg);
  incoming__wrappee__Forward(client, msg);
}


int
findUserPublicKeyPair (void *listdata, void *searchdata)
{
  if (!listdata || !searchdata)
    return -1;
  return strcmp
    (((struct userPublicKeyPair *) listdata)->user,
     (char *) searchdata) ? 0 : 1;
}


int
isKeyPairValid (char *publicKey, char *privateKey)
{
  if (!publicKey || !privateKey)
    return 0;
  return strcmp (publicKey, privateKey) ? 0 : 1;
}
// adds the sign flag to message body
void
sign (struct client *client, struct email *msg)
{
  if (!client->privateKey)
    return;
  msg->signKey = strdup (client->privateKey);
  msg->isSigned = 1;
}


void
forward (struct client *client, struct email *msg)
{
  if (!client->forwardReceiver || !isReadable (msg))
    return;
  // VERIFICATION HOOK
  int verificationHook_isReadable = isReadable (msg);
  // VERIFICATION HOOK END
  struct email *clone = cloneEmail (msg);
  clone->to = strdup (client->forwardReceiver);
  outgoing (client, clone);
}
// checks for a valid signature and replaces it by a flag signaling a verified signature
void
verify (struct client *client, struct email *msg)
{
  // VERIFICATION HOOK
  int verificationHook_list_find =
    list_find (client->userPublicKeyPairs, findUserPublicKeyPair, msg->from) != NULL;
  printf ("\n> hook\n%i\n\n", verificationHook_list_find);
  // VERIFICATION HOOK END
  // VERIFICATION HOOK
  if (verificationHook_list_find)
    {
      int verificationHook_isKeyPairValid =
	isKeyPairValid (((struct userPublicKeyPair
			  *) (list_find (client->userPublicKeyPairs,
					 findUserPublicKeyPair,
					 msg->from)->data))->publicKey,
			msg->signKey);
      printf ("\n> hook\n%i\n\n", verificationHook_isKeyPairValid);
    }
  // VERIFICATION HOOK END
  // VERIFICATION HOOK
  int verificationHook_isReadable = isReadable (msg);
  // VERIFICATION HOOK END
  if (!isReadable (msg) || !msg->isSigned)
    return;
  NODE *foundPublicKeyPair =
    list_find (client->userPublicKeyPairs, findUserPublicKeyPair, msg->from);
  if (foundPublicKeyPair
      && 0 == strcmp (msg->signKey,
		      ((struct userPublicKeyPair *)
		       foundPublicKeyPair->data)->publicKey))
    {
      msg->isSignatureVerified = 1;
    }
}


void
addMessageID (struct client *client, struct email *msg)
{
  if (!msg->id)
    {
      /*
         msg->id = (char *) malloc (sizeof (int) + strlen (client->name));
         sprintf (msg->id, "%s%i", client->name, client->idCounter);
       */
      msg->id = client->idCounter;
      client->idCounter++;
    }
}

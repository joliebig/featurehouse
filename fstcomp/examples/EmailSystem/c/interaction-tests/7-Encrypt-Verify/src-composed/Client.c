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
}



void
mail__wrappee__Sign (struct client *client, struct email *msg)
{
// VERIFICATION HOOK
  int verificationHook_isEncrypted = isEncrypted (msg);
// VERIFICATION HOOK END
  mail__wrappee__Keys (client, msg);
}


void
mail (struct client *client, struct email *msg)
{
// VERIFICATION HOOK
  int verificationHook_isVerified = isVerified (msg);
// VERIFICATION HOOK END
  mail__wrappee__Sign (client, msg);
}


// emails to be sent are processed by this method before beeing mailed.
void
outgoing__wrappee__Keys (struct client *client, struct email *msg)
{
  mail (client, msg);
}


void
outgoing__wrappee__Encrypt (struct client *client, struct email *msg)
{
  encrypt (client, msg);
  outgoing__wrappee__Keys (client, msg);
}


void
outgoing__wrappee__Decrypt (struct client *client, struct email *msg)
{
  sign (client, msg);
  outgoing__wrappee__Encrypt (client, msg);
}



void
outgoing (struct client *client, struct email *msg)
{
  addMessageID (client, msg);
  outgoing__wrappee__Decrypt (client, msg);
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
incoming__wrappee__Keys (struct client *client, struct email *msg)
{
  deliver (client, msg);
}


void
incoming__wrappee__Sign (struct client *client, struct email *msg)
{
// VERIFICATION HOOK
  int verificationHook_isEncrypted = isEncrypted (msg);
// VERIFICATION HOOK END
  incoming__wrappee__Keys (client, msg);
}


void
incoming__wrappee__Verify (struct client *client, struct email *msg)
{
  verify (client, msg);
  incoming__wrappee__Sign (client, msg);
}


void
incoming (struct client *client, struct email *msg)
{
  decrypt (client, msg);
  incoming__wrappee__Verify (client, msg);
}


int
findUserPublicKeyPair (void *listdata, void *searchdata)
{
  return strcmp
    (((struct userPublicKeyPair *) listdata)->user,
     (char *) searchdata) ? 0 : 1;
}

void
encrypt (struct client *client, struct email *msg)
{
  NODE *foundPublicKeyPair =
    list_find (client->userPublicKeyPairs, findUserPublicKeyPair, msg->to);
  if (foundPublicKeyPair)
    {
      msg->encryptionKey =
	strdup (((struct userPublicKeyPair *) foundPublicKeyPair->data)->
		publicKey);
      msg->isEncrypted = 1;
    }
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

// checks for a valid signature and replaces it by a flag signaling a verified signature
void
verify (struct client *client, struct email *msg)
{
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

// removes the decryption flag if possible
void
decrypt (struct client *client, struct email *msg)
{
  if (!client->privateKey)
    return;
  if (msg->isEncrypted == 1
      && 0 == strcmp (msg->encryptionKey, client->privateKey))
    {
      msg->encryptionKey = NULL;
      msg->isEncrypted = 0;
    }
}


void
addMessageID (struct client *client, struct email *msg)
{
  if (!msg->id)
    {
      msg->id = (char *) malloc (sizeof (int) + sizeof (client->name));
      sprintf (msg->id, "%s%i", client->name, client->idCounter);
      client->idCounter++;
    }
  msg->from = strdup (client->name);
}

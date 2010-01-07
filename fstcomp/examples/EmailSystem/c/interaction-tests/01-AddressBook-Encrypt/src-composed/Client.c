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
mail (struct client *client, struct email *msg)
{
// VERIFICATION HOOK
  int verificationHook_isEncrypted = isEncrypted (msg);
// VERIFICATION HOOK END
  mail__wrappee__Keys(client, msg);
}


// emails to be sent are processed by this method before beeing mailed.
void 
outgoing__wrappee__Keys (struct client *client, struct email *msg)
{
  msg->from = strdup (client->name);
  mail (client, msg);
}


void 
outgoing__wrappee__Encrypt (struct client *client, struct email *msg)
{
  encrypt (client, msg);
  outgoing__wrappee__Keys(client, msg);
}


void 
outgoing__wrappee__Decrypt (struct client *client, struct email *msg)
{
  resolveAlias (client, msg);
  outgoing__wrappee__Encrypt(client, msg);
}



void
outgoing (struct client *client, struct email *msg)
{
  addMessageID (client, msg);
  outgoing__wrappee__Decrypt(client, msg);
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
incoming__wrappee__AddressBook (struct client *client, struct email *msg)
{
// VERIFICATION HOOK
  int verificationHook_isEncrypted = isEncrypted (msg);
// VERIFICATION HOOK END
  incoming__wrappee__Keys(client, msg);
}


void
incoming (struct client *client, struct email *msg)
{
  decrypt (client, msg);
  incoming__wrappee__AddressBook(client, msg);
}


int
findUserPublicKeyPair (void *listdata, void *searchdata)
{
  return strcmp
    (((struct userPublicKeyPair *) listdata)->user,
     (char *) searchdata) ? 0 : 1;
}


int
isKeyPairValid (char *publicKey, char *privateKey) {
  return strcmp(publicKey, privateKey);
}
void
encrypt (struct client *client, struct email *msg)
{
  NODE *foundPublicKeyPair =
    list_find (client->userPublicKeyPairs, findUserPublicKeyPair, msg->to);
  if (foundPublicKeyPair)
    {
      msg->encryptionKey =
	strdup (((struct userPublicKeyPair *) foundPublicKeyPair->
		 data)->publicKey);
      msg->isEncrypted = 1;
    }
}
int
findAddressBookEntry (void *listdata, void *searchdata)
{
  return strcmp
    (((struct addressBookEntry *) listdata)->alias,
     (char *) searchdata) ? 0 : 1;
}


void
resolveAlias (struct client *client, struct email *msg)
{
  if (!client->addressBook)
    return;
  struct email *clone = cloneEmail (msg);
  NODE *found =
    list_find (client->addressBook, findAddressBookEntry, clone->to);
  if (!found)
    return;
  NODE *address = ((struct addressBookEntry *) found->data)->address;
  if (address)
    {
      msg->to = strdup (address->data);
      address = address->next;
    }
  while (address)
    {
      struct email *newmsg = cloneEmail (clone);
      newmsg->to = strdup (address->data);
      address = address->next;
      outgoing (client, newmsg);
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
      /*
      msg->id = (char *) malloc (sizeof (int) + strlen (client->name));
      sprintf (msg->id, "%s%i", client->name, client->idCounter);
      */
      msg->id = client->idCounter;
      client->idCounter++;
    }
}

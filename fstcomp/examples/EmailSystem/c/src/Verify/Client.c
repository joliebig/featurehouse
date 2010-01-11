// checks for a valid signature and replaces it by a flag signaling a verified signature
void
verify (struct client *client, struct email *msg)
{
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
incoming (struct client *client, struct email *msg)
{
  verify (client, msg);
  original (client, msg);
}

void
mail (struct client *client, struct email *msg)
{
// VERIFICATION HOOK
  int verificationHook_isVerified = isVerified (msg);
// VERIFICATION HOOK END
  original (client, msg);
}

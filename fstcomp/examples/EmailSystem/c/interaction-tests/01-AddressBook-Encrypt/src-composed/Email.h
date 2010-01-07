//fstcomp> generated include-guard 
#ifndef __EMAIL_H
#define __EMAIL_H
//fstcomp> /generated include-guard 

struct  email {
  int id; 
  char *from; 
  char *to; 
  char *subject; 
  char *body; 
  int isEncrypted; 
  char *encryptionKey;};


struct email *cloneEmail (struct email *msg);


void printMail (struct email *msg);


int isReadable (struct email *msg);


int isEncrypted (struct email *msg);

//fstcomp> generated include-guard 
#endif
//fstcomp> /generated include-guard 

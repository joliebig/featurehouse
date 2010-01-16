//fstcomp> generated include-guard 
#ifndef __EMAIL_H
#define __EMAIL_H
//fstcomp> /generated include-guard 

struct email
{
  int msgID;
  char *from;
  char *to;
  char *subject;
  char *body;
};


struct email *cloneEmail (struct email *msg);


void printMail (struct email *msg);


int isReadable (struct email *msg);

//fstcomp> generated include-guard 
#endif
//fstcomp> /generated include-guard 

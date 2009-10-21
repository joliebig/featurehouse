#define MAX 2000

struct email
{
  char from[MAX];
  char to[MAX];
  char cc[MAX];
  char subject[MAX];
  char body[MAX];
};

void printMail (struct email *msg);

#include <stdio.h>

void spam() { original(); printf("in spam ext1\n"); }

void afterwards() {
	spam();
	printf("in afterwards ext1\n");
}

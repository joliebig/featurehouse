#include <stdio.h>

int main__wrappee__C1( void ) { printf( "Hallo\n" ); }

int main( void ) { main__wrappee__C1(); printf( " Welt!\n" ); }
#include <base.h>

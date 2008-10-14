#ifndef MY_RSA_H
#define MY_RSA_H

/* arch-tag: 9de65f09-19cd-46c6-9f2d-c9206f0268f7 */

#ifndef CONFIG_INCLUDED
#define CONFIG_INCLUDED
#undef PACKAGE_NAME
#undef PACKAGE_STRING
#undef PACKAGE_TARNAME
#undef PACKAGE_VERSION
#include "config.h"
#endif

#include <openssl/rsa.h>
#include <openssl/bn.h>
#include <openssl/evp.h>
#include <limits.h>

#endif

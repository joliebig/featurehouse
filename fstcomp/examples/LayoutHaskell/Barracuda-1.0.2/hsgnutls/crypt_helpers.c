#include <gcrypt.h>

#define LITTLE_ENDIAN 0
#define BIG_ENDIAN    1

int endian() {
	int i = 1;
	char* p = (char *)&i;
	if(p[0] == 1) return LITTLE_ENDIAN;
	else return BIG_ENDIAN;
}

void fix_endian(char *addr,size_t len) {
	if(endian() == LITTLE_ENDIAN) {
		size_t i;
		char tmp;
		for(i=0;i<len/2;i++) {
			tmp = addr[i];
			addr[i] = addr[len-i-1];
			addr[len-i-1] = tmp;
		}
	}
}

gcry_error_t gcry_cipher_setkey2 (gcry_cipher_hd_t H, void *K, size_t L) {
	fix_endian((char*)K,L);
	return gcry_cipher_setkey(H,K,L);
}

gcry_error_t gcry_cipher_setiv2 (gcry_cipher_hd_t H, void *K, size_t L) {
	fix_endian((char*)K,L);
	return gcry_cipher_setiv(H,K,L);
}

#include <gnutls/gnutls.h>
#include <gcrypt.h>
#include <errno.h>
#include <pthread.h>
#include <stdlib.h>

long gnutls_io_wrap_h(void*,void*,size_t,int*);
void freeStablePtr(void *ptr);

#ifdef COMPAT_GNUTLS_1_0
typedef gnutls_transport_ptr gnutls_transport_ptr_t;
typedef gnutls_connection_end gnutls_connection_end_t;
typedef gnutls_session gnutls_session_t;
#endif


static ssize_t gnutls_io_wrap_c(gnutls_transport_ptr_t tr, void* buf, size_t len) 
{
  int err, res;
  res = gnutls_io_wrap_h(tr,buf,len,&err);
  errno = err;
  return res;
}


void replace_transport_stable_ptrs(gnutls_session_t session, void *p1, void *p2) {
  void *o1, *o2;
  gnutls_transport_get_ptr2(session, &o1, &o2);
  gnutls_transport_set_ptr2(session, p1, p2);
  freeStablePtr(o1);
  freeStablePtr(o2);
}

gnutls_session_t init_session_wrap(gnutls_connection_end_t con_end, void *sptr1, void *sptr2, int *err) {
  gnutls_session_t ses;
  *err = gnutls_init(&ses, con_end);
  if(*err != 0) return 0;
  gnutls_transport_set_push_function(ses,(gnutls_push_func)&gnutls_io_wrap_c);
  gnutls_transport_set_pull_function(ses,&gnutls_io_wrap_c);
  gnutls_transport_set_ptr2(ses, sptr1, sptr2);
  gnutls_set_default_priority(ses);
  return ses;
}

GCRY_THREAD_OPTION_PTHREAD_IMPL;

void gcry_init_helper(void) {
  gcry_control(GCRYCTL_DISABLE_SECMEM_WARN);
  gcry_control(GCRYCTL_SET_THREAD_CBS, &gcry_threads_pthread);
  gcry_control(GCRYCTL_INIT_SECMEM, 16384, 0);
}

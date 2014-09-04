#ifndef LOCK_H
#define LOCK_H

/**
 * simple mutex/rwlock wrapper
 **/

#ifdef __cplusplus
extern "C"{
#endif

#ifndef USE_ETHREAD
#define USE_ETHREAD
#endif
#ifdef USE_PTHREAD

#include <pthread.h>
typedef pthread_mutex_t mutex_t ;
typedef pthread_rwlock_t rwlock_t ;
typedef pthread_cond_t cond_t ;

#elif defined USE_ETHREAD

#include "erl_driver.h"
typedef ErlDrvMutex* mutex_t ;
typedef ErlDrvRWLock* rwlock_t ;
typedef ErlDrvCond* cond_t ;

#endif

void * myalloc(size_t);
void   myfree(void *);

void mutex_init(mutex_t*);
void mutex_lock(mutex_t*);
void mutex_unlock(mutex_t*);
void mutex_fini(mutex_t*);

void rwlock_init(rwlock_t*);
void rwlock_rdlock(rwlock_t*);
void rwlock_rwlock(rwlock_t*);
void rwlock_rdunlock(rwlock_t*);
void rwlock_rwunlock(rwlock_t*);
void rwlock_fini(rwlock_t*);;

#ifdef __cplusplus
}
#endif

#endif


#include "lock.h"
#include <stdlib.h>

void * myalloc(size_t size){
  return 
#ifdef USE_PTHREAD
    malloc(size);
#elif defined  USE_ETHREAD
    driver_alloc(size);
#endif
}

void   myfree(void * ptr){
#ifdef USE_PTHREAD
  free(ptr);
#elif defined  USE_ETHREAD
  driver_free(ptr);
#endif
}

void mutex_init(mutex_t * mutex){
#ifdef USE_PTHREAD
  if( pthread_mutex_init(mutex,NULL) != 0 ){
    return ;
  };
#elif defined  USE_ETHREAD
  *mutex = erl_drv_mutex_create(NULL);
#endif
  //  return ret;
}
void mutex_lock(mutex_t* mutex){
#ifdef USE_PTHREAD
  if( pthread_mutex_lock(mutex) != 0 ){ return ;}
#elif defined  USE_ETHREAD
  erl_drv_mutex_lock(*mutex);
#endif
}
void mutex_unlock(mutex_t* mutex){
#ifdef USE_PTHREAD
  pthread_mutex_unlock(mutex);
#elif defined  USE_ETHREAD
  erl_drv_mutex_unlock(*mutex);
#endif
}
void mutex_fini(mutex_t* mutex){
#ifdef USE_PTHREAD
  pthread_mutex_destroy(mutex);
  //  free(mutex);
#elif defined  USE_ETHREAD
  erl_drv_mutex_destroy(*mutex);
#endif
}

void rwlock_init(rwlock_t * rwlock){
  //  rwlock_t * ret;
#ifdef USE_PTHREAD
  //  ret=(pthread_rwlock_t*)malloc(sizeof(pthread_rwlock_t));
  pthread_rwlock_init(rwlock,NULL);
  //  *ret = PTHREAD_RWLOCK_INITIALIZER;
#elif defined  USE_ETHREAD
  *rwlock = erl_drv_rwlock_create(NULL);
#endif
  //  return ret;
}
void rwlock_rdlock(rwlock_t* lock){
#ifdef USE_PTHREAD
  pthread_rwlock_rdlock(lock);
#elif defined  USE_ETHREAD
  erl_drv_rwlock_rlock(*lock);
#endif
}
void rwlock_rwlock(rwlock_t* lock){
#ifdef USE_PTHREAD
  pthread_rwlock_wrlock(lock);
#elif defined  USE_ETHREAD
  erl_drv_rwlock_rwlock(*lock);
#endif
}
void rwlock_rdunlock(rwlock_t* lock){
#ifdef USE_PTHREAD
  pthread_rwlock_unlock(lock);
#elif defined  USE_ETHREAD
  erl_drv_rwlock_runlock(*lock);
#endif
}
void rwlock_rwunlock(rwlock_t* lock){
#ifdef USE_PTHREAD
  pthread_rwlock_unlock(lock);
#elif defined  USE_ETHREAD
  erl_drv_rwlock_rwunlock(*lock);
#endif
}
void rwlock_fini(rwlock_t* lock){
#ifdef USE_PTHREAD
  pthread_rwlock_destroy(lock);
  //  free(lock);
#elif defined  USE_ETHREAD
  erl_drv_rwlock_destroy(*lock);
#endif
};

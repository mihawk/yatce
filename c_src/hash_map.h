#ifndef HASH_MAP_H
#define HASH_MAP_H

#include "linked_list.h"
#include <stdbool.h>

#ifdef __cplusplus
extern "C"{
#endif

  typedef struct hash_map_ hash_map_t;
  
  ///< allocates a new empty hash_map. NULL when failed.
  void  init_hash_map( hash_map_t *, unsigned int L );

  ///< deallocates a yatce map. all tables should be closed before this.
  void  fini_hash_map( hash_map_t *, void (*on_free)(void*) );
  
  ///< add an entry to a yatce map. adb must be initialized either opened. 
  bool  push_hash_map( hash_map_t *, const char * strkey,  void * value);
  
  ///< remove an entry from a yatce map. TCADB should be closed and del'ed by user. NULL if empty || full.
  void * pop_hash_map( hash_map_t *, const char * strkey,
		       void * (*on_remove)(void *, const char *, void* ), void * arg);
  
  ///< get an object from map.//returns entry with rd-locked status!
  void * get_hash_map( hash_map_t *, const char * strkey);
  void * operate_on_hash_entry(hash_map_t * , const char * strkey, 
			       void* (*operation)(void *, const char *, void *), void * arg);
  
  /// internals.
  // gets a hash integer from table name.
  unsigned int hash_table_hash__(const char *);

struct hash_map_{ /** something global data: is it thread safe? */
  
  unsigned int(*hash_func)(const char *);

  linked_list_t * hash_table;
  unsigned int hash_table_length;
  rwlock_t lock;

  unsigned int count;
  mutex_t mutex;

};


#ifdef __cplusplus
}
#endif

//void         test_hash_map(void);

#endif


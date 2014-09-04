#ifndef LINKED_LIST_H
#define LINKED_LIST_H

/**
 * a linked-list library with callback on critical section.
 **/

#include "lock.h"
#include <stdbool.h>

#ifdef __cplusplus
extern "C"{
#endif
  typedef struct linked_list_entry_ linked_list_entry;

  //single direction linked list.
  typedef struct linked_list_{
    rwlock_t lock;
    unsigned int count;
    linked_list_entry * head;
  } linked_list_t;
  
  ///< just allocate an empty list
  void init_linked_list(linked_list_t* list);
  
  ///< not only de-allocate the list, but also pops all entry from list with destructor function for values.
  ///< you can set 'free' function and other cleaning program in on_free function. 
  ///< this function will run before the program frees list_entry.
  void fini_linked_list(linked_list_t* list, void (*on_free)(void *) );
  
  ///< add to <strkey, value> into list.
  bool add_to_list(linked_list_t * list, const char * strkey, void * value);

  ///< remove entry from list with on_remove callback just before removal.
  ///< returns the result of on_remove function or the value (if on_remove is null)
  ///< on_remove function will be executed within writer-locked section.
  ///< arguments of on_remove is: on_remove(arg, strkey, value)
  void * rm_from_list(linked_list_t * list, const char * strkey, 
		      void* (*on_remove)(void *, const char *, void *), void * arg);

  ///< same as operate_on_entry; does nothing and returns the value.
  void * get_from_list(linked_list_t * list, const char * strkey);

  ///< do some operation to the entry of required key in the list.
  ///< returns the result of operation function, or the value (if operation is null)
  ///< operation function will be executed within reader-locked section.
  ///< arguments of operation is: operation(arg, strkey, value)
  void * operate_on_entry(linked_list_t * list, const char * strkey, 
			  void* (*operation)(void *, const char *, void *), void * arg);

  unsigned int list_size(linked_list_t* list);
  
  ///< just prints the list. use as follows print_linked_list( &linked_list_t )
  void   print_linked_list(linked_list_entry*);
  //void   test_linked_list(void);
  
  struct linked_list_entry_{
    char  * strkey;
    void  * value;
    rwlock_t  lock; //reader lock when using value, writer lock when add/remove
    struct linked_list_entry_ * next;
  };

  bool append_to_list_(linked_list_entry ** list_head, const char * strkey, void * value);
  void * rm_from_list_(linked_list_entry ** list_head, const char * strkey, 
		       void* (*on_remove)(void *, const char *, void *), void * arg);
  void * operate_on_entry_(linked_list_entry * list_entry, const char * strkey, 
			   void* (*operation)(void *, const char *, void *), void * arg);
  void   del_all_entry_(linked_list_entry * entry, void (*on_free)(void *));

  linked_list_entry * alloc_linked_list_entry__(void);
  void                free_linked_list_entry__(linked_list_entry*);
  
  char * alloc_str__(size_t str_length); // for example, 3 makes 4 byte memory space.
  void   free_str__(char*);
  
#ifdef __cplusplus
}
#endif


#endif


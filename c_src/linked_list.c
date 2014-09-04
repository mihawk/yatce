#include "linked_list.h"
#include <string.h>

void init_linked_list(linked_list_t* inee){
  rwlock_init( &(inee->lock) );
  inee->head = NULL;
  inee->count= 0;
}
void fini_linked_list(linked_list_t* to_free, void (*on_free)(void *)){
  rwlock_rwlock( &(to_free->lock) );
  if( to_free->head != NULL ){
    del_all_entry_(to_free->head, on_free);
  }
  rwlock_rwunlock( &(to_free->lock));
  rwlock_fini( &(to_free->lock));
}
void del_all_entry_(linked_list_entry * entry, void (*on_free)(void*) ){
  if( entry != NULL ){
    
#ifdef DEBUG_DONE
    fprintf(stderr, "%s %d: %s( key:%s, next:%p ) \n", __FILE__, __LINE__,  __func__, entry->strkey, entry->next );  
#endif
    rwlock_rwlock( &(entry->lock));
    del_all_entry_(entry->next, on_free);
    if( on_free != NULL && entry->value ){
      on_free( entry->value );
    }
    rwlock_rwunlock( &(entry->lock));    // The rwlock has to be in an unlocked state before being destroyed. 
    free_linked_list_entry__(entry);
  }
}

bool add_to_list(linked_list_t * list, const char * strkey, void * value){
  if(list == NULL){
    return false;
  }
  rwlock_rwlock( &(list->lock) );
  append_to_list_( &(list->head), strkey, value);
  list->count++;
  rwlock_rwunlock( &(list->lock) );
  return true;
}
bool append_to_list_(linked_list_entry ** list_head, const char * strkey, void * value){
  linked_list_entry * list_entry;
  bool ret;  //  fprintf(stderr, "%s %d: %s entry: %s %p\n", __FILE__, __LINE__, __func__, strkey, *list_head);

  if( *list_head == NULL ){ // no more key exists   //assume list_head is locked.
    list_entry = alloc_linked_list_entry__();
    rwlock_rwlock( &(list_entry->lock ));
    list_entry->strkey = alloc_str__( strlen(strkey) );
    strncpy(list_entry->strkey, strkey, strlen(strkey)+1);
    list_entry->value = value;
    //    fprintf(stderr, "%s %d: entry: %s, {%s, %s}\n", __FILE__, __LINE__, __func__, strkey, list_entry->value );    
    *list_head = list_entry;
    ret = true;
    rwlock_rwunlock( &(list_entry->lock ));
  }
  else{
    rwlock_rwlock( &((*list_head)->lock));   //giant lock!
    if( strncmp(strkey, (*list_head)->strkey, 1024) == 0 ){ //same key exists
      list_entry = *list_head;
      ret = true;
    } else{
      ret = append_to_list_( &((*list_head)->next), strkey, value);
    }
    rwlock_rwunlock( &((*list_head)->lock));
  }
  //fprintf(stderr, "%d %s succeeded. %p\n", __LINE__, __func__, list_entry->lock);
  return ret;
}

void * rm_from_list(linked_list_t * list, const char * strkey,
		    void* (*on_remove)(void *, const char *, void *), void * arg){
  void * ret;
  if(list == NULL){
    ret=NULL;
  }else{
    rwlock_rwlock( &(list->lock));
    ret=rm_from_list_( &(list->head), strkey, on_remove, arg);
    list->count--;
    rwlock_rwunlock( &(list->lock));
  }
  return ret;
}
void * rm_from_list_(linked_list_entry ** list_head, const char * strkey,
		     void* (*on_remove)(void *, const char *, void *), void * arg){
  linked_list_entry * list_entry = *list_head;
  void * ret;

  if( list_entry == NULL ){ // no more key exists 
    ret = NULL;
  }
  else{
    rwlock_rwlock( &(list_entry->lock));   //giant lock!
    if( strncmp(strkey, list_entry->strkey, 1024) == 0 ){ //same key exists
      *list_head = list_entry->next;
      if( on_remove != NULL ){
	ret = on_remove(arg, strkey, list_entry->value);
      }else{
	ret = list_entry->value;
      }
      rwlock_rwunlock(  &(list_entry->lock));
      //  fprintf(stderr, "%s %d: %s - entry: %p %p\n", __FILE__, __LINE__, __func__, ret , list_entry->lock);
      free_linked_list_entry__( list_entry );
      //  fprintf(stderr, "%s %d: %s - entry: %p %p\n", __FILE__, __LINE__, __func__, ret , list_entry->lock);
    } else{
      ret = rm_from_list_( &(list_entry->next), strkey, on_remove, arg);
      rwlock_rwunlock( &(list_entry->lock ));
    }
    //    fprintf(stderr, "%s %d: %s - entry: %p %p\n", __FILE__, __LINE__, __func__, ret , list_entry);
  }
  return ret;
}


//returns entry with rd-locked status!
void * get_from_list(linked_list_t * list_head, const char * strkey){
  return operate_on_entry(list_head, strkey, NULL, NULL );
}
void * operate_on_entry(linked_list_t * list_head, const char * strkey, 
			void* (*operation)(void *, const char *, void *), void * arg){
  void * ret = NULL;
  //  fprintf(stderr, "%s %d: entry: %s, {%p, -}\n", __FILE__, __LINE__, __func__, list_head );
  if( list_head != NULL ){
    rwlock_rdlock( &(list_head->lock ));
    ret = operate_on_entry_(list_head->head, strkey, operation, arg);
    rwlock_rdunlock( &(list_head->lock ));
  }
  return ret;
}
void * operate_on_entry_(linked_list_entry * list_entry, const char * strkey, 
			 void* (*operation)(void *, const char *, void *), void * arg){
  void * ret = NULL;
  if( list_entry != NULL ){
    rwlock_rdlock( &(list_entry->lock ));
    //    fprintf(stderr, "%s %d: entry: %s, {%s, %p}\n", __FILE__, __LINE__, __func__, strkey, list_entry->value );    
    if( strncmp(strkey, list_entry->strkey, 1024) == 0 ){ //same key exists
      if( operation != NULL ){
	ret = operation(arg, strkey, list_entry->value);
      }else{
	ret = list_entry->value;
      }
    }else{
      ret = operate_on_entry_( list_entry->next, strkey, operation, arg);
    }
    rwlock_rdunlock( &(list_entry->lock));
  }else{
    if( operation != NULL ){
      ret = operation( arg, strkey, NULL ); //record not found
    }
  }
  return ret;
}

unsigned int list_size(linked_list_t* list){
  unsigned int ret;
  if( list == NULL )
    return 0;

  rwlock_rdlock( &(list->lock));
  ret = list->count;
  rwlock_rdunlock( &(list->lock));
  return ret;
}

linked_list_entry * alloc_linked_list_entry__(void){
  linked_list_entry * ret = (linked_list_entry*)myalloc(sizeof(linked_list_entry));
  rwlock_init( &(ret->lock));
  ret->strkey = NULL;
  ret->value = NULL;
  ret->next = NULL;
  return ret;
}
void free_linked_list_entry__(linked_list_entry* to_free){

  if( to_free->strkey != NULL){
    //    fprintf(stderr, "%s %d: %s - entry: %p\n", __FILE__, __LINE__, __func__, to_free );
    free_str__(to_free->strkey);
    to_free->strkey = NULL;
  }
  rwlock_fini( &(to_free->lock));
  to_free->value = NULL;
  to_free->next  = NULL;
  myfree( to_free );
}



char * alloc_str__(size_t str_length){
  char * ret = //(char*)tcmalloc(str_length+1);
    (char*)myalloc(str_length+1); //don't do this!
  memset(ret, 0, str_length+1);
  return ret;
}
void free_str__(char* to_free){
  //fprintf(stderr, "%s %d: %s - entry: %p\n", __FILE__, __LINE__, __func__, to_free );
  //  tcfree(to_free);
  myfree(to_free);
}

#include <stdio.h>
void   print_linked_list(linked_list_entry* list_entry){
  if(list_entry==NULL){
  }else{
    rwlock_rdlock( &(list_entry->lock));
    fprintf(stderr, "\t%p: {%s, %p}-> %p\n", list_entry, list_entry->strkey, list_entry->value, list_entry->next);
    print_linked_list(list_entry->next);
    rwlock_rdunlock( &(list_entry->lock));
  }
}

/**
void        test_linked_list(void){
  int c;
  int N;
  struct pair{
    char * key;
    TCADB * value;
  };
  linked_list_entry *list_entry = NULL;
  struct pair testdata[] = {
    {"db001", NULL},
    {"db002", NULL},
    {"db003", NULL},
    {"/tmp/testh.tch", NULL},
    {"q2w3e4r5ty7u8iop][afsd", NULL},
  };
  N = 5;
  for( c = 0 ; c < N ; ++c ){
    testdata[c].value = tcadbnew();
    test("append", append_to_list(&list_entry, testdata[c].key, testdata[c].value) );
  }
  //  print_linked_list(list_entry);
  for( c = 0 ; c < N ; ++c ){
    linked_list_entry * testee = get_from_list(&list_entry, testdata[c].key );
    rwlock_rdunlock( testee->lock );
    test( "get", (testee->value == testdata[c].value));
  }
  for( c = 0 ; c < N ; ++c ){
    TCADB * tmp = (TCADB*)remove_from_list(&list_entry, testdata[c].key );
    test("pop", (tmp != NULL));
    tcadbdel(tmp);
  }
  test( "end", (list_entry==NULL));
  return ;
}
**/

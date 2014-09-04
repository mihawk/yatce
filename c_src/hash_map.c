#include "hash_map.h"

// allocates a new empty hash_map. NULL when failed.
void init_hash_map( hash_map_t * map, unsigned int L ){
  linked_list_t * iterator;

  map->hash_func  = hash_table_hash__;
  rwlock_init( &(map->lock) );
  mutex_init( &(map->mutex) );
  map->hash_table = (linked_list_t*)myalloc( L *sizeof(linked_list_t) );
  /*  int i;
  for(i=0; i<L ; ++i ){
    init_linked_list( ret->hash_table + i );
    iterator = ret->hash_table + i;
    }*/
  //  mutex_unlock(ret->mutex);//ここに入れるだけだとデッドロック
  for( iterator = map->hash_table; iterator != map->hash_table + (size_t)L; ++iterator ){ // initialize each entry
    init_linked_list(iterator); //ここで何やら起きているorz
    //    fprintf(stderr, "%d:\t%p, %p, %d\n", iterator - ret->hash_table, iterator, iterator->lock, iterator->count);
  }
  map->hash_table_length = L;
  map->count = 0;
  //  mutex_unlock(ret->mutex);//このロック解放を入れるとデッドロックしない
#ifdef DEBUG_DONE
  fprintf(stderr, "%s %d: new hash map allocated. its size is %d.\n", __FILE__, __LINE__, L );
#endif
}

// deallocates a yatce map. all tables should be closed before this.
void        fini_hash_map( hash_map_t * map, void (*on_free)(void*) ){
  linked_list_t * iterator;
  if( map == NULL ){
    return ;
  }

  rwlock_rwlock( &(map->lock) );
  for( iterator = map->hash_table ; iterator - map->hash_table < map->hash_table_length; ++iterator){
    fini_linked_list( iterator, on_free );
  }
  myfree( map->hash_table );
  //      fprintf(stderr, "%s %d: entry: %s, {%p}\n", __FILE__, __LINE__, __func__, map->lock);
  mutex_fini( &(map->mutex) );
  
  //    fprintf(stderr, "%s %d: entry: %s, {%p}\n", __FILE__, __LINE__, __func__, map->lock);
  rwlock_rwunlock( &(map->lock) );
  rwlock_fini( &(map->lock) );

  // 'issono koto' I'll leave Erlang VM release the memory, who has reference counter.
}


// if same key already exists, fails and returns false.
bool        push_hash_map(hash_map_t * map, const char * strkey, void * value){
  bool ret;
  linked_list_t * hash_slot  = map->hash_table + (map->hash_func( strkey ) % map->hash_table_length);
  // fprintf(stderr, "%d %s: %p %p\n", __LINE__, __func__, map->hash_table, hash_slot);
  rwlock_rwlock( &(map->lock));
  //  fprintf(stderr, "%d %s succeeded. %p\n", __LINE__, __func__, map->lock);
  if( add_to_list( hash_slot, strkey, value ) ){
    ret = true;
    mutex_lock(  &(map->mutex));
    (map->count)++;
    mutex_unlock( &(map->mutex));
  } else{
    ret = false;
  }
  //  fprintf(stderr, "%d %s succeeded. %p\n", __LINE__, __func__, map->lock);
  rwlock_rwunlock( &(map->lock) );
  //  fprintf(stderr, "%d %s succeeded. %p\n", __LINE__, __func__, map->lock);
  return ret;
}

// remove an entry from a yatce map.  NULL if empty || full.
void * pop_hash_map( hash_map_t * map, const char * strkey,
		     void * (*on_remove)(void *, const char *, void* ), void * arg){
  unsigned int hash_index = map->hash_func( strkey ) % map->hash_table_length ;
  void * ret;
#ifdef DEBUG_MSG
  //  fprintf(stderr, "%s %d: %s - entry: %s\n", __FILE__, __LINE__, __func__, strkey );
#endif

  rwlock_rwlock( &(map->lock));
  //  print_linked_list( (map->hash_table + hash_index)->head ) ;
  ret = rm_from_list( map->hash_table + hash_index, strkey, on_remove, arg );
  //  fprintf(stderr, "%s %d: %s - entry: %p\n", __FILE__, __LINE__, __func__, ret );
  mutex_lock( &(map->mutex));
  (map->count)--;
  mutex_unlock( &(map->mutex));
  rwlock_rwunlock( &(map->lock));
  return ret;
}

// get an TCADB object from map.
void * get_hash_map( hash_map_t * map, const char * strkey){
  return operate_on_hash_entry( map, strkey, NULL, NULL );
}
void * operate_on_hash_entry(hash_map_t * map, const char * strkey, 
			     void* (*operation)(void *, const char *, void *), void * arg){

  unsigned int hash_index = map->hash_func( strkey ) % map->hash_table_length ;
  void * ret;

#ifdef DEBUG_DONE
  fprintf(stderr, "%s %d: entry: %s, {%s, -}\n", __FILE__, __LINE__, __func__, strkey );
  fprintf(stderr, "%s %d: hash: %d, %d\n", __FILE__, __LINE__, hash_index, map->count );
#endif
  rwlock_rdlock( &(map->lock));
  ret = operate_on_entry( map->hash_table + hash_index, strkey, operation, arg);
  //  print_linked_list( (map->hash_table + hash_index)->head );
  rwlock_rdunlock( &(map->lock));
  return ret;
}

// gets a hash integer from table name.
unsigned int hash_table_hash__(const char * strkey){
  unsigned int ret=0;
  if( strkey == NULL ){
    return 0xDEADBEEF;
  }
  //calculating hash value from string
  //see  http://qune.cside.com/archives/000696.html
  const char * p;
  for( p = strkey; *p != '\0' && p-strkey < 15; p++ ){
    ret = ret * 33 + *p;
  }
#ifdef DEBUG_DONE
  printf("%s:%d %s - %s %x\n", __FILE__, __LINE__, __func__ , table_name, ret);
#endif
  return ret;
}


/**
void        test_hash_map(void){
  int c;
  int N;
  struct pair{
    char * key;
    TCADB * value;
  };
  hash_map * map;
  struct pair testdata[] = {
    {"db001", NULL},
    {"db002", NULL},
    {"db003", NULL},
    {"/tmp/testh.tch", NULL},
    {"q2w3e4r5ty7u8iop][afsd", NULL},
  };
  N = 5;

  map = new_hash_map(32, NULL, NULL );
  for( c = 0 ; c < N ; ++c ){
    testdata[c].value = tcadbnew();
    test("push", push_hash_map(map, testdata[c].key, testdata[c].value) );
  }
  test( "num", (map->count == N) );
  for( c = 0 ; c < N ; ++c ){
    TCADB * testee = (TCADB*)get_hash_map(map, testdata[c].key );
    test( "get", (testee == testdata[c].value));
  }
  for( c = 0 ; c < N ; ++c ){
    TCADB * tmp = (TCADB*)pop_hash_map(map, testdata[c].key );
    test("pop", (tmp != NULL));
    if(tmp != NULL )
      tcadbdel(tmp);
  }
  //  fprintf(stderr, "%d %s succeeded.\n", __LINE__, __func__);
  test( "num", (map->count == 0) );
  for( c = 0 ; c < N ; ++c ){
    test("push", push_hash_map(map, testdata[c].key, testdata[c].value) );
  }
  test( "num", (map->count == N) );
  test( "del_hash_map", del_hash_map(map) );
  return ;
}
**/

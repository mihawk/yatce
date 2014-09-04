#include <erl_driver.h>

#include "tc_table_operation.h"
#include "endecode.h"
#include "tokyocabinet.h"

int table_operations(int command, TCADB * table, char ** rbuf){
  int rlen, index;
  switch(command) {
  case YATCE_CLOSE:      rlen = tc_close( table, rbuf );      break;
    
  case YATCE_ITERINIT:      rlen = tc_iterinit(table, rbuf);      break;
    
  case YATCE_ITERNEXT:      rlen = tc_iternext(table, rbuf);      break;
    
  case YATCE_SYNC:      rlen = tc_sync(table, rbuf);      break;
    
  case YATCE_VANISH:      rlen = tc_vanish(table, rbuf);       break;
    
    //  case YATCE_PATH:      rlen = tc_path(table, rbuf);      break;
    
  case YATCE_RNUM:      rlen = tc_rnum(table, rbuf);      break;
    
  case YATCE_SIZE:      rlen = tc_size(table, rbuf);      break;
    
  default:
    index = 0;
    *rbuf = encode_atom_pair_in_tuple_with_version( &index, "error", "unknown_operation" );
    rlen = index;
  }
  return rlen;
}

int tc_close( TCADB * adb, char ** rbuf ){
  int index = 0;
  //fprintf(stderr, "closing %s .\n", adb->name );
  if( ! tcadbclose(adb) ){ //{error, tcadbopen_failure}
    *rbuf = encode_atom_pair_in_tuple_with_version( &index, "error", "tcadbclose_failure" );
  }

  else{  //{ok, opened}
#ifdef DEBUG_DONE
    fprintf(stderr, "closing %s succeeded.\n", "tcadb");//adb->name );
#endif
    *rbuf = encode_atom_pair_in_tuple_with_version( &index, "ok", "closed" );
  }
  return index; //if binary seems returned, you'd test binary_to_term( list_to_binary( Output ) )
}

int tc_iterinit(TCADB * table, char ** rbuf){
  int index = 0;
  if( tcadbiterinit(table) ){
    *rbuf = encode_atom_pair_in_tuple_with_version( &index, "ok", "init" );
  }else{
    *rbuf = encode_atom_pair_in_tuple_with_version( &index, "error", "failed" );      
  }
  return index;
}
int tc_iternext(TCADB * table, char ** rbuf){
  int index = 0;
  int size = 0;
  char * tmp = NULL;
  if( (tmp = tcadbiternext(table, &size))!= NULL ){
    *rbuf = encode_ok_value_in_tuple_with_version( &index, "ok", tmp, size );
    tcfree(tmp);
  }else{
    *rbuf = encode_atom_pair_in_tuple_with_version( &index, "error", "end_of_data" );      
  }
  return index;
}

int tc_sync(TCADB * table,  char ** rbuf){
  int index = 0;
  
  if(table == NULL || ! tcadbsync(table)){
    *rbuf = encode_atom_pair_in_tuple_with_version( &index, "error", "not_synced" );

  }else{
    *rbuf = encode_atom_pair_in_tuple_with_version( &index, "ok", "sync" );

  }
  return index;
}

int tc_vanish(TCADB * table, char ** rbuf ){
  int index=0;
  if( tcadbvanish(table) ){
    *rbuf = encode_atom_pair_in_tuple_with_version( &index, "ok", "vanished" );
  }else{
    *rbuf = encode_atom_pair_in_tuple_with_version( &index, "error", "not_vanished" );      
  }
  return index;
}
/** int tc_path(  TCADB * table, char ** rbuf ){
  int index = 0;
  const char * path; //must not be freed!
  if( (path = tcadbpath(table)) != NULL ){
    *rbuf = encode_atom_pair_in_tuple_with_version( &index, "ok", "hoge" );
//    *rbuf = encode_ok_value_in_tuple_with_version( &index, "ok", path, strlen(path)+1 );
    //need encoding to erlang list
    }else{
  *rbuf = encode_atom_pair_in_tuple_with_version( &index, "error", "unknown" );
  } 
  *rbuf = encode_atom_pair_in_tuple_with_version( &index, "error", "maybe_deprecated" );
  return index;
} **/
int tc_rnum(  TCADB * table, char ** rbuf ){
  int index = 0;
  *rbuf = encode_ok_int_in_tuple_with_version(&index, tcadbrnum(table));
  return index;
}
int tc_size(  TCADB * table, char ** rbuf ){
  int index = 0;
  *rbuf = encode_ok_int_in_tuple_with_version(&index, tcadbsize(table));
  return index;
}

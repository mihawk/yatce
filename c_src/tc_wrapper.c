#include "tc_wrapper.h"
#include "endecode.h"
#include "easy_test.h"
#include <string.h>

#include <ei.h> //for ei_decode_hogehoge
#include "tokyocabinet.h"

int tc_open(  TCADB * adb, const char * table_name_with_options, char ** rbuf ){
  int index = 0;
  if( ! tcadbopen( adb, table_name_with_options) ){ //{error, tcadbopen_failure}
#ifdef DEBUG_DONE
    fprintf(stderr, "%s %d: opening %s failed.\n",__FILE__, __LINE__,  table_name_with_options );
#endif
    *rbuf = encode_atom_pair_in_tuple_with_version( &index, "error", "tcadbopen_failure" );
  }
  else{    //{ok, opened}
#ifdef DEBUG_DONE
    fprintf(stderr, "%s %d: opening %s succeeded.\n", __FILE__, __LINE__, table_name_with_options );
#endif
    *rbuf = encode_atom_pair_in_tuple_with_version( &index, "ok", "opened" );
  }

  return index;
}

int tc_put(   TCADB * adb, const ErlDrvBinary * key, const ErlDrvBinary * value, char ** rbuf, int command ){
  int index = 0;
  bool result = false;
  const char * msg;
  if( command == YATCE_PUT ){
    result =  tcadbput( adb, key->orig_bytes, key->orig_size, value->orig_bytes, value->orig_size );
    msg = "inserted";
  }else if( command == YATCE_PKEEP ){
    result =  tcadbputkeep( adb, key->orig_bytes, key->orig_size, value->orig_bytes, value->orig_size );
    msg = "inserted";
    /**
  }else if( command == YATCE_PUTCAT ){
    result =  tcadbputcat( adb, key->orig_bytes, key->orig_size, value->orig_bytes, value->orig_size );
    msg = "appended";     **/
  }
  if( !result ){
#ifdef DEBUG_DONE
    fprintf(stderr, "inserting into %s failed.\n", "name");//adb->name );
#endif
    *rbuf = encode_atom_pair_in_tuple_with_version( &index, "error", "tcadbput(keep)_failure" );
  }
  else{  //{ok, opened}
#ifdef DEBUG_DONE
    fprintf(stderr, "inserting into %s succeeded.\n", "the table");// adb->name );
    fprintf(stderr, " value -> %s, %ld.\n", inspect_code(value->orig_bytes[0]), value->orig_size );
#endif
    *rbuf = encode_atom_pair_in_tuple_with_version( &index, "ok", msg );
  }
  return index;
}

int tc_key_operation( int command, TCADB * adb, const ErlDrvBinary * key, char ** rbuf ){
  int index;
  int rlen;
  switch(command) {
  case YATCE_OUT:       rlen = tc_out( adb, key, rbuf );       break;
  case YATCE_GET:       rlen = tc_get( adb, key, rbuf );       break;
    
  default:
    index = 0;
    *rbuf = encode_atom_pair_in_tuple_with_version( &index, "error", "unknown_operation" );
    rlen = index;
  }
  return rlen;
}

int tc_get(   TCADB * adb, const ErlDrvBinary * key, char ** rbuf ){
  int index = 0;
  int size  = 0;

  char * value = (char*)tcadbget( adb, key->orig_bytes, key->orig_size, &size );
  if( value == NULL ){
    *rbuf = encode_atom_pair_in_tuple_with_version( &index, "error", "record_doesnt_exist" );
  }

  else{  //{ok, opened}
#ifdef DEBUG_DONE
    fprintf(stderr, "getting from %s succeeded.\n", "table");//adb->name );
    fprintf(stderr, " value -> %s, %ld.\n", inspect_code(value[0]), size );
    int i=0;
    ei_print_term(stderr, value, &i);
    fprintf( stderr, "\t%d=?=%d\n", i, size);
#endif
    *rbuf = encode_ok_value_in_tuple_with_version( &index, "ok", value, size );
    free(value);
  }
  return index;
}

int tc_out(   TCADB * adb, const ErlDrvBinary * key, char ** rbuf ){
  int index = 0;

  if( ! tcadbout( adb, key->orig_bytes, key->orig_size) ){
    *rbuf = encode_atom_pair_in_tuple_with_version( &index, "error", "tcadbout_failure" );
  }

  else{  //{ok, deleted}
#ifdef DEBUG_DONE
    fprintf(stderr, "removing from %s succeeded.\n", "tablename");//adb->name );
#endif
    *rbuf = encode_atom_pair_in_tuple_with_version( &index, "ok", "deleted" );
  }
  return index;
}

int tc_test(char ** rbuf){
  int index = 0;
  bool result;
/*   prepare();
   test_yatce_list();
   test_yatce_map(); 
   result = print_result(__func__); */
  result = true;
  if( result ){
    *rbuf = encode_atom_pair_in_tuple_with_version( &index, "ok", "all_passed" );
  }else{
    *rbuf = encode_atom_pair_in_tuple_with_version( &index, "error", "test_failed" );
  }
  return index;
}
  /*
   http://archive.netbsd.se/view_attachment.php?id=7320381.78759
   When the C-program processed the request from erlang it will use the
   ei-interface to create an Erlang term. This term will be sent the
   erlang via a socket. The Erlang control process, will forward
   it to the client that does binary_to_term before returning the result
   to the client program.
   */


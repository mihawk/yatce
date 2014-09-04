#include "endecode.h"
#include "tokyocabinet.h"
#include <string.h>

int decode_tuple2(char ** table_namep, ErlDrvBinary ** key, const char * buf, int * index ){
  int arity;
  
  if( ei_decode_tuple_header( buf, index, &arity ) > 0 ){ // != 0; fail
    *table_namep = NULL;
    return -1;
  }
  if( arity != 2 ){
    *table_namep = NULL;
    return -1;
  }

  *table_namep = (char*)driver_alloc( sizeof(char) * MAXATOMLEN );
  if( ei_decode_atom(buf, index, *table_namep) != 0 ){
    return -1;
  }
  
  *key = decode_key( buf, index );
  return 0;
}

// memory area for table_name is not allocated, while memory area for key and value is already allocated
int decode_tuple3(char ** table_namep, ErlDrvBinary ** key,  ErlDrvBinary ** value, const char * buf, int * index ){
  int arity;
  
  if( ei_decode_tuple_header( buf, index, &arity ) > 0 ){ // != 0; fail
    *table_namep = NULL;
    return -1;
  }
  if( arity != 3 ){
    *table_namep = NULL;
    return -1;
  }

  *table_namep = (char*)driver_alloc( sizeof(char) * MAXATOMLEN );
  if( ei_decode_atom(buf, index, *table_namep) != 0 ){
    return -1;
  }
  
  *key = decode_key( buf, index );
  *value = decode_value( buf, index );

#ifdef DEBUG_DONE
  fprintf(stderr, "%s %d: %s.\n", __FILE__, __LINE__, inspect_code( (*key)->orig_bytes[0]) );
  printf("%s / %ld\n", ((*key)->orig_bytes + 1), (*key)->orig_size  );
  printf("%s / %ld\n", ((*value)->orig_bytes + 1), (*value)->orig_size  );
#endif
  return 0;
}


/** from ei.h
typedef struct {
    char ei_type;
    int arity; //size for tuple / list
    int size;  //size for binary
    union {
	long i_val;
	double d_val;
	char atom_name[MAXATOMLEN+1];
	erlang_pid pid;
	erlang_port port;
	erlang_ref ref;
    } value;
} ei_term;   */

// The idea is simple; just use ei_s_print_term to get a binary length
// accoring to erlang-VM way; and return it.
int get_binary_size(const char * buf){
  int i=0;
  ei_skip_term(buf, &i);

#ifdef DEBUG_DONE
  i=0;
  ei_print_term(stderr, buf, &i);
  fprintf(stderr, "\n");
  //  char * s = NULL;
  //  ei_s_print_term(&s, buf, &i);
  fprintf(stderr, "print:%s -size:%d\n", s, i);
#endif
  return i;
}

ErlDrvBinary * decode_term( const char * buf, int * index){
  int size = get_binary_size(buf+ *index);
  ErlDrvBinary * ret = driver_alloc_binary( size );
  memcpy( ret->orig_bytes, buf + *index, size );
#ifdef DEBUG_DONE
  DEBUG_INFO( ret->orig_bytes, ret->orig_size);
#endif
  *index += size;
  return ret;
}

// Decodes buf, into ErlDrvBinary, to be inserted into TC currently.
ErlDrvBinary * decode_key( const char * buf, int * index){
  return decode_term( buf, index );
}
ErlDrvBinary * decode_value( const char * buf, int * index ){
  return decode_term( buf, index );
}

char * read_table_name( char * buf, int * index, int len ){
  char * table_name;
  int type, size;
  if( ei_get_type(buf, index, &type, &size) != 0 ) return NULL;
  if( type != ERL_ATOM_EXT ){
#ifdef DEBUG_MSG
    fprintf( stderr, "not a atom format.\n" );
#endif
    return NULL; //go ahead!
  }
  // normal routine.
  table_name = (char*)driver_alloc(sizeof(char) * (len+1)); //len is always smaller than MAXATOMLEN
  if( ei_decode_atom( buf, index, table_name ) != 0 ){
#ifdef DEBUG_MSG
    fprintf(stderr, "can't read table name: %s\n", table_name );
#endif
    driver_free( table_name );
    return NULL;
  }
  table_name[len] = '\0';
  
  return table_name;
}

char * encode_ok_value_in_tuple_with_version( int * index, const char * result, const void * value_tmp, int size){
  int i = 0;
  char * ret = NULL;
  char * value = (char*)value_tmp;
  ei_encode_version( NULL, &i );
  ei_encode_tuple_header( NULL, &i, 2 );
  ei_encode_atom( NULL, &i, result);
  i += size;
  ret = driver_alloc( i* sizeof(char) );

  i = 0;
  ei_encode_version( ret, &i );
  ei_encode_tuple_header( ret, &i, 2 );
  ei_encode_atom( ret, &i, result);
  memcpy( ret+ i, value, size );
  i += size;
  *index = i;

  return ret;
}

// "error", "someerr"  =>  { error, someerr }
char * encode_atom_pair_in_tuple_with_version( int * index, const char * result, const char * reason ){
  int i = 0;
  char * ret = NULL;
  ei_encode_version( NULL, &i );
  ei_encode_tuple_header( NULL, &i, 2 );
  ei_encode_atom( NULL, &i, result);
  ei_encode_atom( NULL, &i, reason);
  ret = (char*)driver_alloc( sizeof( char ) * i );
  if( ret != NULL ){
    i = 0;
    ei_encode_version( ret, &i );
    ei_encode_tuple_header( ret, &i, 2 );
    ei_encode_atom( ret, &i, result);
    ei_encode_atom( ret, &i, reason);
    *index = i;
  }    
  return ret;
}

//  some int   =>   {ok, 123}
char * encode_ok_int_in_tuple_with_version(int * index, unsigned long long  result ){
  int i = 0;
  char * ret = NULL;
  ei_encode_version( NULL, &i );
  ei_encode_tuple_header( NULL, &i, 2 );
  ei_encode_atom( NULL, &i, "ok");
  ei_encode_ulonglong( NULL, &i, result );
  ret = (char*)driver_alloc( sizeof( char ) * i );
  if( ret != NULL ){
    i = 0;
    ei_encode_version( ret, &i );
    ei_encode_tuple_header( ret, &i, 2 );
    ei_encode_atom( ret, &i, "ok");
    ei_encode_ulonglong( ret, &i, result );
    *index = i;
  }    
  return ret;
}


const char * inspect_code( int code ){
  is_this_it( code, ERL_SMALL_INTEGER_EXT );
  is_this_it( code, ERL_INTEGER_EXT );
  is_this_it( code, ERL_FLOAT_EXT );
  is_this_it( code, ERL_ATOM_EXT );
  is_this_it( code, ERL_PORT_EXT );
  is_this_it( code, ERL_PID_EXT );
  is_this_it( code, ERL_STRING_EXT );
  is_this_it( code, ERL_BINARY_EXT );
  is_this_it( code, ERL_SMALL_BIG_EXT );
  is_this_it( code, ERL_LARGE_BIG_EXT );
  is_this_it( code, ERL_LIST_EXT );
  is_this_it( code, ERL_REFERENCE_EXT );
  is_this_it( code, ERL_NEW_REFERENCE_EXT );
  is_this_it( code, ERL_SMALL_TUPLE_EXT );
  is_this_it( code, ERL_LARGE_TUPLE_EXT );
  is_this_it( code, ERL_NIL_EXT );
  is_this_it( code, ERL_NEW_FUN_EXT );
  is_this_it( code, ERL_FUN_EXT );
  return "UNKNOWN_CODE";
}
const char * inspect_command( int code ){
  is_this_it( code, YATCE_OPEN );
  is_this_it( code, YATCE_CLOSE );
  is_this_it( code, YATCE_PUT );
  is_this_it( code, YATCE_PKEEP );
  //  is_this_it( code, YATCE_PUTCAT );
  is_this_it( code, YATCE_OUT );
  is_this_it( code, YATCE_GET );
  is_this_it( code, YATCE_ADDINT );
  is_this_it( code, YATCE_ADDDOUBLE );
  is_this_it( code, YATCE_SYNC );
  is_this_it( code, YATCE_ITERINIT);
  is_this_it( code, YATCE_ITERNEXT);
  is_this_it( code, YATCE_OPTIMIZE );
  is_this_it( code, YATCE_VANISH );
  is_this_it( code, YATCE_COPY );
  //  is_this_it( code, YATCE_TX );
  is_this_it( code, YATCE_PATH );
  is_this_it( code, YATCE_RNUM );
  is_this_it( code, YATCE_SIZE );
  is_this_it( code, YATCE_TEST );
  return "UNKNOWN_CODE";
}


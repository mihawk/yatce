#include "dispatcher.h"
#include "tc_wrapper.h"
#include "endecode.h"
#include <string.h> // for strncpy

#include "hash_map.h"

static ErlDrvData yatce_drv_start(ErlDrvPort port, char *buff){
  hash_map_t * map = (hash_map_t*)myalloc( sizeof( hash_map_t ) );
  init_hash_map(map, 1024);
  return (ErlDrvData)map;
}

static void yatce_drv_stop(ErlDrvData handle){
  hash_map_t * map = (hash_map_t*)handle;
  fini_hash_map(map, NULL);
  myfree(map);
  return;
}

// inspired by http://rakuto.blogspot.com/2008/07/manipulate-erlang-binary-term-format-in.html
// this function only apportions the request == calls function depending on its command
// 
// @brief
//  Entry point from Erlang code to C code. all Driver entry goes through 
//  this function. Don't miss it!!!
static int yatce_drv_control(ErlDrvData drv_data, unsigned int command, 
			     char * buf, int len, char ** rbuf, int rlen ){
  hash_map_t * d = (hash_map_t*)drv_data;
  int index, ver;
  ErlDrvBinary * key, * value;
  char * table_name;
  TCADB * table = NULL;
  int r;
  index = 0;

  //  printf("sleepstart %p\n", buf);
  //  sleep(1);
  //  printf("sleepend   %p\n", buf);
  /** selection tree
   <has the request size?>
    +-Y -> YATCE_TEST
   <is the request consists of tuple?>
    +-Y ->
    | +-<is the request consists of value?>
    |    +-Y -> < tablename, key, value >
    |    |      YATCE_PUT, YATCE_PKEEP
    |    +-N -> < tablename, key >
    |          YATCE_OUT, YATCE_GET, 
    +-N ->  <request consists of only table_name>
         YATCE_OPEN, YATCE_CLOSE, YATCE_ITERNEXT, YATCE_ITERINIT, YATCE_SYNC
         YATCE_VANISH, YATCE_RNUM, YATCE_SIZE
  **/
#ifdef DEBUG_DONE
  fprintf(stderr, "%s %d: %s( %s ) - len: %d\n", __FILE__, __LINE__,  __func__, inspect_command( command ), len );
  if( len > 0 ){
    ei_print_term(stderr, buf, &index);     fprintf(stderr, "\n");
    DEBUG_INFO(buf, len);
    index = 0;
  }
#endif

  if( len <= 0 && command == YATCE_TEST){
    return tc_test(rbuf);
  }
  // All ei_xxx functions return 0 if it will be success, else return -1
  if( ei_decode_version(buf, &index, &ver) != 0 ){// Value of 'index' will become 1.
    *rbuf = encode_atom_pair_in_tuple_with_version( &index, "error", "invalid_argument");
    return rlen;
  }

  //only uses table.
  if( (command & USE_TABLE) && !(command & USE_KEY) && !(command & USE_VALUE) ){
    table_name = read_table_name( buf, &index, len );    
    table = (TCADB*)get_hash_map(d, table_name);
    //case for YATCE_OPEN
    if( command == YATCE_OPEN ){
      if( table == NULL ){
	table = tcadbnew();

	if( ! push_hash_map( d, table_name, (void*)table ) ){
	  fprintf(stderr, "%s %d: failed in putting TCADB '%s' into map.\n", __FILE__, __LINE__,  table_name ); 
	}
	rlen = tc_open( table, table_name, rbuf );
      }
      driver_free(table_name);
      return rlen;
    }
    else if( table == NULL ){ // error: table not found and command is not open
      index = 0;
      *rbuf = encode_atom_pair_in_tuple_with_version( &index, "error", "db_not_found" );
      rlen = index;
      driver_free(table_name);
      return rlen;
    }
    //case table found and no {key,value} needed operations:
#ifdef DEBUG_DONE
    fprintf(stderr, "%s %d: %s( %s ) %s\n", __FILE__, __LINE__,  __func__, inspect_command( command ) , table_name);
#endif

    if(command == YATCE_CLOSE) pop_hash_map(d, table_name, NULL, NULL);
    rlen = table_operations(command, table, rbuf);
    driver_free(table_name);
    return rlen;
  }
  else if( (command & USE_TABLE) && (command & USE_KEY) && !(command & USE_VALUE) ){ //  YATCE_OUT, YATCE_GET, 
    r = decode_tuple2( &table_name, &key, buf, &index);
#ifdef DEBUG_DONE
    printf( "%s %d: table name : %s\n", __FILE__, __LINE__, table_name );
#endif
    if( r != 0 ){
      index = 0;
      *rbuf = encode_atom_pair_in_tuple_with_version(&index, "error", "badarith" );
      rlen = index;
    }

    else if( (table = (TCADB*)get_hash_map(d, table_name) ) == NULL ){
      index = 0;
      *rbuf = encode_atom_pair_in_tuple_with_version( &index, "error", "db_not_found" );
      rlen = index;
    }
    else {
#ifdef DEBUG_DONE
      printf( "%s %d: table name : %s\n", __FILE__, __LINE__, table_name );
      printf( "%s %d: TCADB * adb = %p\n", __FILE__, __LINE__, table );
      fprintf(stderr, "%s %d: %s( %s ) \n", __FILE__, __LINE__,  __func__, inspect_command( command ) );
#endif
      rlen = tc_key_operation(command, table, key, rbuf);
    }
    driver_free(table_name);
    driver_free_binary(key);
    return rlen;
  }

  else if( (command & USE_TABLE) && (command & USE_KEY) && (command & USE_VALUE)){ //  YATCE_OUT, YATCE_GET, 
    r = decode_tuple3( &table_name, &key, &value, buf, &index);
#ifdef DEBUG_DONE
    printf( "%s %d: table name : %s\n", __FILE__, __LINE__, table_name );
#endif
    if( r != 0 ){
      index = 0;
      *rbuf = encode_atom_pair_in_tuple_with_version(&index, "error", "badarith" );
      rlen = index;
      return rlen;
    }
    if( (table = (TCADB*)get_hash_map(d, table_name) ) == NULL ){
      index = 0;
      *rbuf = encode_atom_pair_in_tuple_with_version( &index, "error", "db_not_found" );
      rlen = index;
    }else{
#ifdef DEBUG_DONE
      printf( "%s %d: table name = %s, TCADB * adb = %p\n", __FILE__, __LINE__, table_name, table );
      fprintf(stderr, "%s %d: %s( %s ) \n", __FILE__, __LINE__,  __func__, inspect_command( command ) );
#endif
      switch(command) {
      case YATCE_PUT: //{TableName, Key, Value}
      case YATCE_PKEEP:
	rlen = tc_put( table, key, value, rbuf, command );       break;
	
      case YATCE_PUTCAT:
      case YATCE_ADDINT:
      case YATCE_ADDDOUBLE:
      default:
	index = 0;
	*rbuf = encode_atom_pair_in_tuple_with_version( &index, "error", "unsupported_operation" );
	rlen = index;
      }
    }
    driver_free(table_name);
    driver_free_binary(key);
    driver_free_binary(value);
    return rlen;
  }
  else{
#ifdef DEBUG_DONE
    fprintf(stderr, "%s %d: %s( %s ) \n", __FILE__, __LINE__,  __func__, inspect_command( command ) );
#endif
    //TODO: need assertion, or not?
    *rbuf = encode_atom_pair_in_tuple_with_version( &index, "error", "unknown_operation" );
    rlen = index;
    return rlen;
  }

#if 0 //def DEBUG_MSG
  DEBUG_INFO( (*rbuf), rlen-1 );
#endif
  fprintf(stderr, "%s:%d  ouch! program must not reach here.\n", __FILE__, __LINE__);
  *rbuf = NULL;
  return 0;
}

void init_tcadb_(void * TCADB){}
void fini_tcadb_(void * TCADB){  //  tcadbdel(TCADB);
}

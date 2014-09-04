#ifndef ENDECODE_H
#define ENDECODE_H
#include <erl_driver.h>
#include <ei.h>

// {en|de}coding library.

int decode_tuple2(char ** table_namep, ErlDrvBinary ** key, const char * buf, int * index );

int decode_tuple3(char ** table_namep, ErlDrvBinary ** key, ErlDrvBinary ** value, const char * buf, int * index );

char * read_table_name( char * buf, int * index, int len );


// Decodes buf, into ErlDrvBinary, to be inserted into TC
// currently, integer and string is only supported.
ErlDrvBinary * decode_term( const  char * buf, int * index);
ErlDrvBinary * decode_key( const  char * buf, int * index);
ErlDrvBinary * decode_value( const char * buf, int * index ); //internally, they're all same.

/**
 * modules for {en|de}code between [erlang world] <--> [real C world]
 */
char * encode_ok_value_in_tuple_with_version( int * index,  const char * result, const void * value_tmp, int size);
char * encode_atom_pair_in_tuple_with_version( int * index, const char * result, const char * reason );
char * encode_ok_int_in_tuple_with_version(int * index, unsigned long long  result );

const char * inspect_code( int code );
const char * inspect_command( int code );

void print_term(const char * buf, int len);

#define get_name( macro ) #macro
#define is_this_it( r, macro ) if( r == macro ){ return #macro; };

#define DEBUG_INFO(buf, len)  \
  {									\
    int _i_ = 0;								\
    printf("** %s:%d - %s - buflen: %d\n** ", __FILE__, __LINE__, __func__, (len)); \
    while(_i_ < (len) ){						\
      printf("%.3d ", (unsigned char)((buf)[_i_]));			\
      if( 0 ==( ++_i_ % 0x10 ) )printf("\n** ");  \
    }							\
    putchar('\n');						\
  } 



#endif


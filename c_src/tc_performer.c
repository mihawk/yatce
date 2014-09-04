#include <stdio.h>

// for TC
#include <tcutil.h>
#include <tcadb.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>

#include <string.h>

// for performer
#include <pthread.h>
#include <sys/time.h>

// for random char generator
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

//#include "yatce_map.h"

const char * get_static_char(const unsigned int len);
int get_random_char(char * buf, int size);

static char letters[] = 
  "1234567890" "-=`!@#$%^&" "*()_+|~" //17
  "qwertyuiop" "[]QWERTYUI" "OP{}" //14
  "asdfghjkl;" "'ASDFGHJKL" ":\"" //12
  "zxcvbnm,./" "ZXCVBNM<>?"; //10
//53 letters
static int num_letters = 53;

typedef struct initdata_{
  TCADB * adb;
  unsigned int Nrec;
} initdata;

void * test_put(void * arg);

void usage(){
  fprintf(stderr, "usage: "
	  " $ tc_performer <numthreads> <numrecords> \n" );
  exit(1);
}


int main(int args, char ** argv){
  unsigned int Nth, Nrec;
  initdata data;
  struct timeval start, end;
  double t;
  char * key, * value;
  unsigned int counter;
  pthread_t * threads;

  key   = (char*)malloc(sizeof(char)*1024);
  value = (char*)malloc(sizeof(char)*1024);

  if( args >= 3 ){
    Nth  = atoi( argv[1] );
    Nrec = atoi( argv[2] );
  }else{
    usage();
  }
  threads = (pthread_t*)malloc(sizeof(pthread_t)*Nth);

  data.Nrec = Nrec;
  data.adb = tcadbnew();
  if( ! tcadbopen( data.adb, "/tmp/test.tch") ){
    fprintf(stderr, "%d: open failed.\n", __LINE__);
  }

  gettimeofday(&start, NULL);

  for( counter = 0 ; counter < Nth ; ++counter ){
    pthread_create( &threads[counter], NULL, test_put, (void*)&data );
  }

  for( counter = 0 ; counter < Nth ; ++counter ){
    pthread_join( threads[counter], NULL );
  }

  gettimeofday(&end, NULL);
  t = (end.tv_sec - start.tv_sec) + (end.tv_usec - start.tv_usec) / 1000000.0 ;
  printf( "elasped time: %f sec, \tat least: %f qps\n" , t, Nth * Nrec / t ); 
	  
  tcadbclose(data.adb);
  tcadbdel(data.adb);
  free(key);
  free(value);
  //
  return 0;
}

void * test_get(void * arg){
  initdata * data = (initdata*)arg;
  unsigned int counter;
  char key[17];

  for( counter = 0 ; counter < data->Nrec ; ++counter ){
    get_random_char(key, 16);
    char * value = tcadbget2( data->adb, get_static_char(16) );
    if( value == NULL ){
      fprintf(stderr, "%d: tcadbget2 failed.\n", __LINE__);
    }
    free(value);
  }
  return NULL;
}

void * test_put(void * arg){
  initdata * data = (initdata*)arg;
  unsigned int counter;
  char key[17];
  char value[129];

  for( counter = 0 ; counter < data->Nrec ; ++counter ){
    get_random_char(key, 16);
    get_random_char(value, 128);
    if( ! tcadbput2( data->adb, key, value ) ){
      fprintf(stderr, "%d: tcadbput2 failed.\n", __LINE__);
    }
  }
  return NULL;
}


const char * get_static_char(const unsigned int len){
  if(len <= 0){
    return NULL;
  }
  else if( len >= 1024 ){
    return 
      "0987654321" "0987654321" "0987654321" "0987654321" "0987654321" "0987654321" "hoge" //64 byte
      "0987654321" "0987654321" "0987654321" "0987654321" "0987654321" "0987654321" "hoge" //64 byte
      "0987654321" "0987654321" "0987654321" "0987654321" "0987654321" "0987654321" "hoge" //64 byte
      "0987654321" "0987654321" "0987654321" "0987654321" "0987654321" "0987654321" "hoge" //64 byte //256
      "0987654321" "0987654321" "0987654321" "0987654321" "0987654321" "0987654321" "hoge" //64 byte
      "0987654321" "0987654321" "0987654321" "0987654321" "0987654321" "0987654321" "hoge" //64 byte
      "0987654321" "0987654321" "0987654321" "0987654321" "0987654321" "0987654321" "hoge" //64 byte
      "0987654321" "0987654321" "0987654321" "0987654321" "0987654321" "0987654321" "hoge" //64 byte //512
      "0987654321" "0987654321" "0987654321" "0987654321" "0987654321" "0987654321" "hoge" //64 byte
      "0987654321" "0987654321" "0987654321" "0987654321" "0987654321" "0987654321" "hoge" //64 byte
      "0987654321" "0987654321" "0987654321" "0987654321" "0987654321" "0987654321" "hoge" //64 byte
      "0987654321" "0987654321" "0987654321" "0987654321" "0987654321" "0987654321" "hoge" //64 byte 
      "0987654321" "0987654321" "0987654321" "0987654321" "0987654321" "0987654321" "hoge" //64 byte
      "0987654321" "0987654321" "0987654321" "0987654321" "0987654321" "0987654321" "hoge" //64 byte
      "0987654321" "0987654321" "0987654321" "0987654321" "0987654321" "0987654321" "hoge" //64 byte
      "0987654321" "0987654321" "0987654321" "0987654321" "0987654321" "0987654321" "hoge" //64 byte //1024
      ;
  }
  else if( len >= 512 ){
    return 
      "0987654321" "0987654321" "0987654321" "0987654321" "0987654321" "0987654321" "hoge" //64 byte
      "0987654321" "0987654321" "0987654321" "0987654321" "0987654321" "0987654321" "hoge" //64 byte
      "0987654321" "0987654321" "0987654321" "0987654321" "0987654321" "0987654321" "hoge" //64 byte
      "0987654321" "0987654321" "0987654321" "0987654321" "0987654321" "0987654321" "hoge" //64 byte //256
      "0987654321" "0987654321" "0987654321" "0987654321" "0987654321" "0987654321" "hoge" //64 byte
      "0987654321" "0987654321" "0987654321" "0987654321" "0987654321" "0987654321" "hoge" //64 byte
      "0987654321" "0987654321" "0987654321" "0987654321" "0987654321" "0987654321" "hoge" //64 byte
      "0987654321" "0987654321" "0987654321" "0987654321" "0987654321" "0987654321" "hoge" //64 byte //512
      ;
  }
  else if( len >= 256 ){
    return 
      "0987654321" "0987654321" "0987654321" "0987654321" "0987654321" "0987654321" "hoge" //64 byte
      "0987654321" "0987654321" "0987654321" "0987654321" "0987654321" "0987654321" "hoge" //64 byte
      "0987654321" "0987654321" "0987654321" "0987654321" "0987654321" "0987654321" "hoge" //64 byte
      "0987654321" "0987654321" "0987654321" "0987654321" "0987654321" "0987654321" "hoge" //64 byte //256
      ;
  }
  else if( len >= 128 ){
    return 
      "0987654321" "0987654321" "0987654321" "0987654321" "0987654321" "0987654321" "hoge" //64 byte
      "0987654321" "0987654321" "0987654321" "0987654321" "0987654321" "0987654321" "hoge" //64 byte
      ;
  }
  else if( len >= 0x10 ){
    return "0123456789" "abcde";
  }
  return "a";
}


int get_random_char(char * buf, int size){
  int bytes_read = 0;
  int fd;
  int i;
  fd = open( "/dev/urandom", O_RDONLY );
  if( fd < 0 ){
    perror("can't open /dev/urandom" );    //    exit(1); //error,...
    return -1;
  }
  while( bytes_read < size ){
    int r=read(fd, buf, size-bytes_read);
    if( r < 0 ){
      perror( "can't read(2)");
      break;
    }
    bytes_read+=r;
  }
  close(fd);
  for( i = 0 ; i < bytes_read ; ++i ){
    buf[i] = letters[ buf[i] % num_letters ];
  }
  buf[bytes_read] = '\0';
  return bytes_read;
}

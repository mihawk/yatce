#include "easy_test.h"

#include <stdlib.h> //for malloc
#include <stdio.h>  //for fprintf

#include "ei.h"

unsigned int total_test = 0;
unsigned int passed_test = 0;
unsigned int failed_test = 0;

struct test_entry * test_stack = NULL;

void prepare(void){
  total_test = passed_test = failed_test = 0;
  test_stack = NULL;
}

void test(const char * name, bool test_result ){
  struct test_entry * new_test = (struct test_entry*)malloc(sizeof(struct test_entry));
  if( new_test == NULL ){
    fprintf(stderr, "error, no_memory\n" );
    return;
  }
  //push on the stack;
  new_test->prev = test_stack;
  test_stack = new_test;
  
  new_test->name = name;
  new_test->result = test_result;

  if( test_result ){
    passed_test++;
  }else{
    failed_test++;
  }
  total_test++;
}

bool print_result(const char * name){
  struct test_entry * top = test_stack;
  struct test_entry * old;
  fprintf(stderr, "test '%s' done.\n", name);
  fprintf(stderr, "total\tok\tng\n");
  fprintf(stderr, "%d\t%d\t%d\n", total_test, passed_test, failed_test);

  if( failed_test > 0 ){
    fprintf(stderr, "failed: ");
  }

  while(top){
    if( ! top->result ){
      fprintf( stderr, "%s ", top->name );
    }
    old = top;
    top = top->prev;
    free(old);
  }
  if( failed_test > 0 ){
    fprintf( stderr, "\n");
  }
  return (( total_test == passed_test) && ( failed_test == 0 ) );
}

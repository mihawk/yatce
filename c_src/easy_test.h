#ifndef EASY_TEST_H
#define EASY_TEST_H

#include <stdbool.h>

extern unsigned int total_test;
extern unsigned int passed_test;
extern unsigned int failed_test;

extern struct test_entry * test_stack;

void prepare(void);

void test( const char * name, bool test_result );

bool print_result(const char * name);

struct test_entry{
  const char * name;
  struct test_entry * prev;
  bool result;
};


#endif


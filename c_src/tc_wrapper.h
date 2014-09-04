#ifndef TC_WRAPPER_H
#define TC_WRAPPER_H

#include <stdio.h>
#include <tcutil.h>
#include <tcadb.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>

#include <erl_driver.h>
#include "tc_table_operation.h"

void quit( TCADB * adb );

char * get_table_name(char * buff);

int tc_open(  TCADB * adb, const char * table_name_with_options, char ** rbuf );

int tc_put(   TCADB * adb, const ErlDrvBinary * key, const ErlDrvBinary * value, char ** rbuf, int command );

int tc_key_operation( int command, TCADB * adb, const ErlDrvBinary * key, char ** rbuf );

int tc_get(   TCADB * adb, const ErlDrvBinary * key, char ** rbuf );
int tc_out(   TCADB * adb, const ErlDrvBinary * key, char ** rbuf );
int tc_test(char ** rbuf);

#endif


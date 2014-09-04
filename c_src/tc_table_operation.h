#ifndef TC_TABLE_OPERATION_H
#define TC_TABLE_OPERATION_H

#include <stdio.h>
#include <tcutil.h>
#include <tcadb.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>

int table_operations(int command, TCADB * table, char ** rbuf);

int tc_close( TCADB * adb, char ** rbuf );
int tc_iterinit(TCADB * adb, char ** rbuf);
int tc_iternext(TCADB * adb, char ** rbuf);
int tc_sync(  TCADB * adb, char ** rbuf );
int tc_vanish(TCADB * adb, char ** rbuf );
//int tc_path(  TCADB * adb, char ** rbuf );
int tc_rnum(  TCADB * adb, char ** rbuf );
int tc_size(  TCADB * adb, char ** rbuf );

#endif

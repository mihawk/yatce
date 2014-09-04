#ifndef TOKYOCABINET_H
#define TOKYOCABINET_H

// must be same as tokyocabinet.hrl

/** common macros along with tokyo_cabinet.erl **/
#define DYLIB_NAME  ("libyatce")
#define IS_ATOM  (101)
#define IS_LIST  (102)
#define IS_RAW   (103)

#define USE_TABLE   (0x01)              // argument TableName
#define USE_KEY     (0x02)  // argument {TableName, Key}
#define USE_VALUE   (0x04)  // argument {TableName, Key, Value}

#define YATCE_OPEN     ( 0x80100 | USE_TABLE)
#define YATCE_CLOSE    ( 0x80200 | USE_TABLE)
#define YATCE_PUT      ( 0x80300 | USE_TABLE | USE_KEY | USE_VALUE)
#define YATCE_PKEEP    ( 0x80400 | USE_TABLE | USE_KEY | USE_VALUE)
#define YATCE_PUTCAT   ( 0x80500 | USE_TABLE | USE_KEY | USE_VALUE)
#define YATCE_OUT      ( 0x80600 | USE_TABLE | USE_KEY  )
#define YATCE_GET      ( 0x80700 | USE_TABLE | USE_KEY  )
#define YATCE_ITERINIT ( 0x80900 | USE_TABLE)
#define YATCE_ITERNEXT ( 0x81000 | USE_TABLE)

#define YATCE_ADDINT   ( 0x81200 | USE_TABLE | USE_KEY | USE_VALUE)
#define YATCE_ADDDOUBLE ( 0x81300 | USE_TABLE | USE_KEY | USE_VALUE)
#define YATCE_SYNC     ( 0x81400 | USE_TABLE)
#define YATCE_OPTIMIZE ( 0x81500 | USE_TABLE)
#define YATCE_VANISH   ( 0x81600 | USE_TABLE)
#define YATCE_COPY     ( 0x81700 | USE_TABLE)
//#define YATCE_TX       ( 0x81800 | USE_TABLE)
#define YATCE_PATH     ( 0x81900 | USE_TABLE)
#define YATCE_RNUM     ( 0x82200 | USE_TABLE)
#define YATCE_SIZE     ( 0x82300 | USE_TABLE)

#define YATCE_TEST     (0x90000)

#endif


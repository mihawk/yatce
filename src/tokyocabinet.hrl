%% -*- coding: utf-8 -*-
-define(DYLIB_NAME, "libyatce").
-define(IS_LOADED, libyatce_process).

-define(IS_ATOM,   101).
-define(IS_LIST,   102). %% a simple string
-define(IS_RAW,    103).

%% must match c_src/tokyocabinet.h
-define(YATCE_OPEN,   16#80101).
-define(YATCE_CLOSE,  16#80201).
-define(YATCE_PUT,    16#80307).
-define(YATCE_PKEEP,16#80407).
%%-define(YATCE_PUTCAT, 16#80507).
-define(YATCE_OUT,    16#80603).
-define(YATCE_GET,    16#80703).
-define(YATCE_ITERINIT, 16#80901).
-define(YATCE_ITERNEXT, 16#81001).
-define(YATCE_ADDINT, 16#81207).
-define(YATCE_ADDDOUBLE,16#81307).
-define(YATCE_SYNC,   16#81401).
-define(YATCE_OPTIMIZE, 16#81501).
-define(YATCE_VANISH, 16#81601).
-define(YATCE_COPY,   16#81701).
%-define(YATCE_TX,     16#81807).
-define(YATCE_PATH,   16#81901).

-define(YATCE_RNUM,   16#82201).
-define(YATCE_SIZE,   16#82301).

-define(YATCE_TEST,   16#90000).


%% types
-type option() :: {libdir, list()}.
-type tablename() :: atom().
-type tcadb() :: {tcadb, tablename(), [option()], port()}.

-type erl_ddll_error_desc() :: linked_in_driver | inconsistent | permanent |
                     not_loaded_by_this_process | not_loaded |  pending_reload
                     | pending_process.

-type key() :: string() | integer().
-type value() :: string() | number() | binary().

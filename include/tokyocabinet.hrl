%% -*- coding: utf-8 -*-
-define(DYLIB_NAME, "erl_tc_driver").
-define(IS_LOADED, erl_tc_driver_process).

-define(IS_ATOM,   101).
-define(IS_LIST,   102). %% a simple string
-define(IS_RAW,    103).

-define(TC_CLOSE,  1).
-define(TC_DELETE, 2).
-define(TC_FOLDL,  3).
%4
-define(TC_INFO, 5).
%6
-define(TC_OPEN_FILE, 7).
-define(TC_LOOKUP   , 8).
% 9-16(ry
-define(TC_INSERT,   17).
-define(TC_INSERT_NEW, 18).

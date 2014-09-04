%% -*- coding: utf-8 -*-

-module(tcadb, [TableName, Options, Port]).
-author('kuenishi+tc@gmail.com').

-include("tokyocabinet.hrl").
%% OO type interface, going to be compliant with tokyocabinet.idl
%% see http://d.hatena.ne.jp/cooldaemon/20080502/1209716211

-export([             %%   interface ADB { //from tokyocabinet.idl
	 open/0,      %%     boolean open(in string name);
	 close/0,     %%     boolean close();
	 put/2,       %%     boolean put(in string key, in string value);
	 putkeep/2,   %%     boolean putkeep(in string key, in string value);
%	 putcat/2,    %%     boolean putcat(in string key, in string value);
	 out/1,       %%     boolean out(in string key);
	 get/1,       %%     string get(in string key);
		      %%     long vsiz(in string key);
	 iterinit/0,  %%     boolean iterinit();
	 iternext/0,  %%     string iternext();
		      %%     List fwmkeys(in string prefix, in long max);
	 addint/2,    %%     long addint(in string key, in long num);
	 adddouble/2, %%     double adddouble(in string key, in double num);
	 sync/0,      %%     boolean sync();
	 optimize/1,  %%     boolean optimize(in string params);
	 vanish/0,    %%     boolean vanish();
	 copy/0,      %%     boolean copy(in string path);
	 tx/1,	      %%     boolean tranbegin();
		      %%     boolean trancommit();
		      %%     boolean tranabort();
	 path/0,      %%     string path();
	 rnum/0,      %%     long long rnum(); #of records
	 size/0,      %%     long long size();
		      %%     List misc(in string name, in List args);
		      %%   };
	 me/0
	]).


-spec open() -> {ok, opened} | {error, tcadbopen_failure}.
open() when is_atom( TableName )->
    yatce_server:decode( port_control( Port, ?YATCE_OPEN, yatce_server:encode(TableName) ) ).

-spec close() -> {ok, closed} | {error, tcadbclose_failure}.
close() when is_atom( TableName )->
    yatce_server:decode( port_control( Port, ?YATCE_CLOSE, yatce_server:encode(TableName)) ).

-spec put(key(), value()) -> {ok, inserted} | {error,  'tcadbput(keep)_failure'}.
put(Key,Value) ->
    yatce_server:decode( port_control( Port, ?YATCE_PUT, yatce_server:encode({TableName, Key, Value}) ) ).

-spec putkeep(key(), value()) -> {ok, inserted} | {error,  'tcadbput(keep)_failure'}.
putkeep(Key,Value)->
    yatce_server:decode( port_control( Port, ?YATCE_PKEEP, yatce_server:encode({TableName, Key, Value}) ) ).

%% -spec putcat(key(), value()) -> {ok, appended} | {error,  'tcadbput(keep)_failure'}.
%% putcat(_Key,_Value)->    
%%     {error, not_yet_supported}.

-spec out(key()) -> {ok, inserted} | {error,  'tcadbout_failure'}.
out(Key)->
    yatce_server:decode( port_control( Port, ?YATCE_OUT, yatce_server:encode({TableName, Key} ) )).

-spec get( key() )-> {ok, value()} | {error, record_doesnt_exist}.
get(Key)->
    yatce_server:decode( port_control( Port, ?YATCE_GET, yatce_server:encode( {TableName, Key}) )).

-spec iterinit() -> {ok, init} | {error, failed}.
iterinit() when is_atom(TableName)->
    yatce_server:decode( port_control( Port, ?YATCE_ITERINIT, yatce_server:encode(TableName) )).

-spec iternext() -> {ok, value()} | {error, end_of_data}.
iternext() when is_atom(TableName)->
    yatce_server:decode( port_control( Port, ?YATCE_ITERNEXT, yatce_server:encode(TableName) )).

addint(_Key,_Int)->
    {error, not_yet_supported}.

adddouble(_Key, _Double)->
    {error, not_yet_supported}.

-spec sync() -> {ok, sync} | {error, not_synced}.
sync()->
    yatce_server:decode( port_control( Port, ?YATCE_SYNC, yatce_server:encode(TableName) )).

optimize(_)->
    {error, not_yet_supported}.

-spec vanish() -> {ok, vanished} | {error, not_vanished}.
vanish() when is_atom(TableName) ->
    yatce_server:decode( port_control( Port, ?YATCE_VANISH, yatce_server:encode(TableName) )).

copy()->
    {error, not_yet_supported}.

tx(_Fun)->
    {error, not_yet_supported}.

-spec path() -> tablename().
path() when is_atom(TableName)->
    TableName.

-spec rnum() -> non_neg_integer().
rnum() when is_atom(TableName)->
    yatce_server:decode( port_control( Port, ?YATCE_RNUM, yatce_server:encode(TableName) )).

-spec size() -> non_neg_integer().
size() when is_atom(TableName)->
    yatce_server:decode( port_control( Port, ?YATCE_SIZE, yatce_server:encode(TableName) )).

me()-> THIS.




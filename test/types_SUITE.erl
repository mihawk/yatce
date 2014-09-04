%% -*- coding: utf-8 -*-

-module(types_SUITE).
-author('kuenishi@gmail.com').

-compile(export_all).

-include("ct.hrl").

-define(STR_STR, {"testkeyhogehoge", "testvaluehogehoge"}).
-define(STR_INT, {"testkey_sono_1",  123}	   ).
-define(STR_BIN, {"testkey_bin",    <<"asdfafreiuhqa4tq">> } ).
-define(STR_DBL, {"testkey_double", 28734056.2054}).

-define(INT_STR, {213, "se5dr67vt8bynu9im03w4ed5rf6t7gy8hu9ji0kf\poi"}).
-define(INT_INT, {234, 345} ).
-define(INT_BIN, {2345, <<"q34r89;of]">> }).
-define(INT_DBL, {56, 435.534} ).

-define(STR, "a6wogfpho;ijsz9043-0rf").
-define(INT, 345678).
-define(BIN, <<"a347fw8aiu ">>).
-define(DBL, 243456788.3435556).
-define(TUPLE, {hoge, hage, tuple}).
-define(LIST,  ["hoge", listtoooo, 2344]).

init_per_suite(Config) ->
    ok=application:start(yatce),
    Config.

end_per_suite(Config) ->
    yatce:stop(),
    Config.

all() -> [{group, tch},
	  {group, tcb}
%	  ,{group, tcf}
%	  ,{group, tct}
	  ,{group, '*'}
%	  ,{group, '+'}
	 ].

groups()->
    Suffices = [tch,tcb,tcf,tct, '*', '+'],
    Keys = [strk, intk, bink, dblk, tupk, listk, refk ],
    Values = [ strv, intv, binv, dblv, tupv, listv, refv ], %% I wish I could use function closure
    KeyGroups = lists:map( fun(Key) -> {group, Key} end,  Keys  ),
    Bases = [ { Suffix, [sequencial], KeyGroups } || Suffix <- Suffices ],
    Testcases = [ { Key, [sequencial], Values }   || Key <- Keys ],
    Bases ++ Testcases.
%    [ {X, [sequencial], Set} || {Suffix, Key, Value} <- Testcases ].

new_config(TableName, Config) when is_atom(TableName) ->
    TCADB = yatce:db(TableName, []),
%    {ok, opened}=TCADB:open(),
    R = TCADB:open(),
    ct:log("~p <~p>~n", [R, TableName]),
    NewConfig = [{tablename, TableName}, {tcadb, TCADB} ] ++ Config,
    NewConfig.
    
reset_config(Config)->
    TableName = ?config(tablename, Config),
    TCADB = ?config(tcadb, Config),
    ct:log("~p~n", [TCADB]),
    {ok,closed}=TCADB:close(),
    TmpConfig = lists:delete({tablename,TableName}, Config),
    NewConfig = lists:delete({tcadb, TCADB}, TmpConfig),
    NewConfig.

init_per_group(tch,Config)->    new_config( '/tmp/test.tch', Config );
init_per_group(tcb,Config)->    new_config( '/tmp/test.tcb', Config );
init_per_group(tcf,Config)->    new_config( '/tmp/test.tcf', Config );
init_per_group(tct,Config)->    new_config( '/tmp/test.tct', Config );
init_per_group('*',Config)->    new_config( '*', Config );
init_per_group('+',Config)->    new_config( '+', Config );

init_per_group(strk, Config)->    [{key, ?STR}|Config];
init_per_group(intk, Config) ->   [{key, ?INT}|Config];
init_per_group(bink, Config) ->   [{key, ?BIN}|Config];
init_per_group(dblk, Config) ->   [{key, ?DBL}|Config];
init_per_group(tupk, Config) ->   [{key, ?TUPLE}|Config];
init_per_group(listk, Config) ->  [{key, ?LIST}|Config];
init_per_group(refk,  Config) ->  [{key, make_ref()}|Config];
init_per_group(_, Config) -> Config.
    
end_per_group(tch,Config)->    reset_config(Config);
end_per_group(tcb,Config)->    reset_config(Config);
end_per_group(tcf,Config)->    reset_config(Config);
end_per_group(tct,Config)->    reset_config(Config);
end_per_group('*',Config)->    reset_config(Config);
end_per_group('+',Config)->    reset_config(Config);
end_per_group(_, Config)->
    Key = ?config(key, Config),
    NewConfig = lists:delete({key,Key}, Config),
    NewConfig.

strv(Config)->
    Value=?STR,
    Key=?config(key,Config),
    T = ?config(tcadb, Config),
    ct:log("{key,value}=~p~n", [{Key,Value}]),
    {ok, inserted}=T:put(Key,Value),
    {ok,Value}=T:get(Key).

intv(Config)->
    Value=?INT,
    Key=?config(key,Config),
    T = ?config(tcadb, Config),
    ct:log("{key,value}=~p~n", [{Key,Value}]),
    {ok, inserted}=T:put(Key,Value),
    {ok,Value}=T:get(Key).

binv(Config)->
    Value=?BIN,
    Key=?config(key,Config),
    T = ?config(tcadb, Config),
    ct:log("{key,value}=~p~n", [{Key,Value}]),
    {ok, inserted}=T:put(Key,Value),
    {ok,Value}=T:get(Key).

dblv(Config)->
    Value=?DBL,
    Key=?config(key,Config),
    T = ?config(tcadb, Config),
    ct:log("{key,value}=~p~n", [{Key,Value}]),
    {ok, inserted}=T:put(Key,Value),
    {ok,Value}==T:get(Key).

tupv(Config)->
    Value=?TUPLE,
    Key=?config(key,Config),
    T = ?config(tcadb, Config),
    ct:log("{key,value}=~p~n", [{Key,Value}]),
    {ok, inserted}=T:put(Key,Value),
    {ok,Value}==T:get(Key).

listv(Config)->
    Value=?LIST,
    Key=?config(key,Config),
    T = ?config(tcadb, Config),
    ct:log("{key,value}=~p~n", [{Key,Value}]),
    {ok, inserted}=T:put(Key,Value),
    {ok,Value}==T:get(Key).

refv(Config)->
    Value=make_ref(),
    Key=?config(key,Config),
    T = ?config(tcadb, Config),
    ct:log("{key,value}=~p~n", [{Key,Value}]),
    {ok, inserted}=T:put(Key,Value),
    {ok,Value}==T:get(Key).

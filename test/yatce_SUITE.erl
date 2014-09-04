%% -*- coding: utf-8 -*-

-module(yatce_SUITE).
-author('kuenishi@gmail.com').

-compile(export_all).

%% just calling yatce_map.c: easy test

-include("ct.hrl").

init_per_suite(Config) ->
    OptionPairs = [
 		   {'/tmp/test.tch', 
		    {'/tmp/test.tch', []}},
 		   {'/tmp/test.tch#mode=rw', 
		    {'/tmp/test.tch', [{mode,"rw"}]}},
 		   {'/tmp/test.tch#bnum=1000000', 
		    {'/tmp/test.tch', [{bnum, 1000000}]}},
 		   {'/tmp/test.tch#mode=rw#bnum=1000000', 
		    {'/tmp/test.tch', [{mode,"rw"}, {bnum,1000000}]}},
 		   {'/tmp/test.tcb#apow=100#rcnum=1024', 
		    {'/tmp/test.tcb', [{apow,100}, {rcnum,1024}]}},
 		   {'*', 
		    {'*', []}}
		  ],
    [{list, OptionPairs}|Config].

end_per_suite(Config) ->
    Config.

all() -> [ init, 
	   options,
	   fini ].

%groups()->
%%     [ {options, [parallel], OptionPairs} ].

init(Config)->
    ok=application:start(yatce),
    Config.

options(Config)->
    OptionPairs = ?config(list,Config),
    ok=check(OptionPairs),
    lists:delete(OptionPairs, Config).

check([])->
    ok;
check([Pair|OptionPairs]) ->
    {Answer, {TableName, Options}}=Pair,
    ct:log("~p~n", [{tcadb, Answer, Options, make_ref()}]),
    T = yatce:db(TableName, Options),
    {tcadb, Answer, Options, _} = T,
    {ok,opened}=T:open(),
    {ok,closed}=T:close(),
    check(OptionPairs).

fini(Config)->
    ok = yatce:stop(),
    Config.

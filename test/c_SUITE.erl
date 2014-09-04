%% -*- coding: utf-8 -*-
-module(c_SUITE).
-author('kuenishi@gmail.com').

-compile(export_all).

%% just calling yatce_map.c: easy test

-include("ct.hrl").

init_per_suite(Config) ->
    ok=application:start(yatce),
    Config.

end_per_suite(Config) ->
    yatce:stop(),
    Config.

all() -> [ dup_init, tch, tcb %, tct, '*', '+' 
	  ].

dup_init(Config)->
    {error, {already_started, yatce}} = application:start(yatce),
    Config.

tch(Config)->    
    T = yatce:db('/tmp/test.tch', []), 
    {ok, opened} = T:open(),
    {ok, closed} = T:close(),
    {ok, all_passed} = yatce_server:test(),
    Config.

tcb(Config)->    
    T = yatce:db('/tmp/test.tcb', []), 
    {ok, opened} = T:open(),
    {ok, closed} = T:close(),
    {ok, all_passed} = yatce_server:test(),
    Config.

tct(Config)->    
    T = yatce:db('/tmp/test.tct', []), 
    {ok, opened} = T:open(),
    {ok, closed} = T:close(),
    {ok, all_passed} = yatce_server:test(),
    Config.

'*'(Config)->    
    T = yatce:db('*', []), 
    {ok, opened} = T:open(),
    {ok, closed} = T:close(),
    {ok, all_passed} = yatce_server:test(),
    Config.

'+'(Config)->    
    T = yatce:db('+', []), 
    {ok, opened} = T:open(),
    {ok, closed} = T:close(),
    {ok, all_passed} = yatce_server:test(),
    Config.

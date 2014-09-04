%% -*- coding: utf-8 -*-

-module(unit_performer_SUITE).
-author('kuenishi@gmail.com').

-compile(export_all).
-include("ct.hrl").
-import(random).

%% tests perf_controller, perf_timer, unit_performer here.
-define(SERVER_NAME, ?MODULE).

-spec generate( non_neg_integer() )-> list().
generate(0)-> [];
generate(Length)->   RChar = 96+random:uniform(26),  [RChar|generate(Length-1)].

init_per_suite(Config) ->
    {A1, A2, A3}=now(),
    random:seed(A1, A2, A3),
    ok=application:start(yatce),
    Config.

end_per_suite(Config) ->
    yatce:stop(),
    Config.

all()-> [ %{group, tch} 
	 ].

init_per_group(tch, Config)->
    T=yatce:db('/tmp/test.tch', []),
    T:open(),
    [{tcadb, T}|Config].

end_per_group(tch, Config)->
    T=?config(tcadb,Config),
    T:close(),
    lists:delete({tcadb,T}, Config).
    
groups()->
    Funs = [start, init, test, start2, wait, poll, stop],
    [ {tch, [sequencial], Funs} ].

start(Config)-> 
    {ok,Pid}=gen_server:start({local, ?SERVER_NAME},unit_performer,[],[]),
    Config.
init(Config)->
    T=?config(tcadb, Config),
    Fun=fun(Table)->
	      {Key,Value}={generate(8),generate(128)},
	      Table:put(Key,Value)
      end,
    ok=gen_server:call(?SERVER_NAME, {init, Fun, T}),
    Config.
test(Config)->
    {ok,inserted}=gen_server:call(?SERVER_NAME, test),
    Config.
start2(Config)->
    ok=gen_server:cast(?SERVER_NAME, start),
    Config.
wait(Config)->
    timer:sleep(500),
    Config.
poll(Config)->
    Count=gen_server:call(?SERVER_NAME, poll),
    ct:log("count is ~p.~n",[Count]),
    Count=5,
    Config.
stop(Config)->
    ok=gen_server:cast(?SERVER_NAME, stop),
    Config.
    
    
%% get()->
%%     T=?config(tcadb,Config),
    

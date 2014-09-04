%% -*- coding: utf-8 -*-

-module(perf_timer_SUITE).
-author('kuenishi@gmail.com').

-compile(export_all).
-include("ct.hrl").

%% tests perf_controller, perf_timer, unit_performer here.
-define(SERVER_NAME, ?MODULE).

-spec generate( non_neg_integer() )-> list().
generate(0)-> [];
generate(Length)->   RChar = 96+random:uniform(26),  [RChar|generate(Length-1)].

all()-> []. %[start, stop].

init_per_suite(Config) ->    Config.
end_per_suite(Config) ->     Config.

start(Config)-> 
    {ok,Pid}=gen_fsm:start({local, ?SERVER_NAME}, perf_timer,1000,[]),
    Config.
stop(Config)->
    ok=gen_fsm:send_event(?SERVER_NAME, stop),
    Config.
    
    
%% get()->
%%     T=?config(tcadb,Config),
    

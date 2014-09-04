%% -*- coding: utf-8 -*-

-module(perf_controller_SUITE).
-author('kuenishi@gmail.com').

-compile(export_all).
-include("ct.hrl").

%% tests perf_controller, perf_timer, unit_performer here.
-define(SERVER_NAME, ?MODULE).

all()-> [ %start, stop
	 ].

init_per_suite(Config) ->    
    {ok,Pid}=gen_fsm:start({local, perf_timer}, perf_timer,1000,[]),
    Config.
end_per_suite(Config) -> 
%%    ok=gen_fsm:send_event(perf_timer, stop),
    Config.

start(Config)-> 
    ok=application:start(yatce),
    FileName = '/tmp/test_perf_controller.tch',
    {ok,Pid}=gen_server:start({local, ?SERVER_NAME}, perf_controller, {[unit_performer0, unit_performer1], FileName},[{debug,[trace,log,statistics]}]),
    Config.
stop(Config)->
    ok=gen_server:call(?SERVER_NAME, stop),
    Config.
    
    
%% get()->
%%     T=?config(tcadb,Config),
    

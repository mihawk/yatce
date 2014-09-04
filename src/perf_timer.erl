%% -*- coding: utf-8 -*-

-module(perf_timer).
-author('kuenishi').
-behaviour(gen_fsm).

%% architecture:
%%  supervisor -+- controller
%%              +- timer
%%              +- unit_performer x N
%%
%% where N is scale of parallel process hits TC.

-export([
	 init/1,
	 running/2,
	 stopped/2,
	 handle_event/3,
	 handle_sync_event/4,
	 handle_info/3,
	 terminate/3,
	 code_change/4
	 ]).

init(Timeout)->
    io:format("start: ~p!~n", [Timeout]),
    {ok,stopped,Timeout}.

running(timeout,StateData)->
    {ok,Sum,Total}=gen_server:call(perf_controller,poll),
    Timeout=StateData,
    io:format("~p: ~p tps, rnum: ~p~n", [erlang:localtime(),Sum*1000/Timeout,Total]),
    {next_state, running, Timeout, Timeout};
running(stop,StateData)->
%    {stop, normal, StateData};
    {next_state, stopped, StateData};
running(_,StateData)->
    {next_state, stopped, StateData}.

stopped(start,StateData)->
    Timeout=StateData,
    {next_state, running, Timeout, Timeout};
stopped(stop,StateData)->
    {stop, normal, StateData};
stopped(_,StateData)->
    {next_state, stopped, StateData}.

handle_event(_,_,StateData)->
    {next_state, stopped, StateData}.

handle_sync_event(_,_,StateName,StateData)->
    {next_state, StateName, StateData}.

handle_info(_,StateName,StateData)->
    {next_state, StateName, StateData}.

terminate(Reason,_StateName,_StateData)->
    ok.

code_change(_,StateName,StateData,_)->
    {ok,StateName,StateData}.

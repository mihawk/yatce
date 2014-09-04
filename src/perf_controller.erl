%% -*- coding: utf-8 -*-

-module(perf_controller).
-author('kuenishi').
-behaviour(gen_server).

-export([
	 init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3
	]).

%% architecture:
%%  supervisor -+- controller
%%              +- timer
%%              +- unit_performer x N
%%
%% where N is scale of parallel process hits TC.

-record( controllerdata,
	 {perfs=[],
	  tc_handle=none,
	  count=0,
	  total=0,
	  interval=1000 % ms
	 }).

-spec init( list() )-> {ok, term()} | {stop, term()}.
init([])->    {stop, empty_argv};
init(List)->
    {UnitPerformerNames,FileName} = List,
    io:format("UnitPerformerNames: ~p - FileName ~p, ~p started. ~n", [UnitPerformerNames, FileName, ?MODULE]),
    TCADB=yatce:newdb(FileName,[]),
    {ok,opened}=TCADB:open(),
    {ok, #controllerdata{perfs=UnitPerformerNames, tc_handle=TCADB}}.

handle_call({set_func,Fun},_From,State)->
    bcall(State#controllerdata.perfs,{set_func,Fun}),
%%    gen_server:multi_call(State#controllerdata.perfs, {set_func,Fun}),
    {reply,ok,State};
handle_call({set_handle,TCADB},_From,State)->
    bcall(State#controllerdata.perfs,{set_handle,TCADB}),
%    gen_server:multi_call(State#controllerdata.perfs, {set_handle,TCADB}),
    {reply,ok,State#controllerdata{tc_handle=TCADB}};

handle_call(poll, _From, State)->
    Replies=bcall(State#controllerdata.perfs,poll),
%    {Replies,_BadNodes}=gen_server:multi_call(State#controllerdata.perfs, poll),
    Total = State#controllerdata.total,
    Sum = lists:foldl( fun(Count, AccIn)-> AccIn + Count end,  0, Replies ),
%%    io:format("~p:~p ~p~n",[?LINE, State, Replies]),
    {reply,{ok, Sum, Sum+Total},State#controllerdata{
					  total = Total + Sum,
					  count = Sum
					 }};

handle_call(start,_From,State)->
    bcall(State#controllerdata.perfs, {set_handle, State#controllerdata.tc_handle}),
    bcast(State#controllerdata.perfs, start),
    gen_fsm:send_event(perf_timer,start),
%%    gen_server:multi_call(State#controllerdata.perfs, {set_handle, State#controllerdata.tc_handle}),
%%    gen_server:abcast(State#controllerdata.perfs, start),
    {reply,ok,State};

handle_call(stop,_From,State)->
    gen_fsm:send_event(perf_timer,stop),
    bcast(State#controllerdata.perfs,stop),
%    gen_server:multi_call(State#controllerdata.perfs, stop),
    {stop,normal,ok,State};

handle_call(_,_,State)->
    {reply, ok, State}.

handle_cast(_,State)->
    {ok,State}.

handle_info(_,State)->
    {ok,State}.

terminate(_,State) ->
%    gen_server:call(?MODULE,stop),
    {ok,closed}=(State#controllerdata.tc_handle):close(),
    ok.

code_change(_,State,_)->
    {ok,State}.

bcast(List, Msg) when is_list(List)->
    lists:map(fun(Performer)-> gen_server:cast(Performer, Msg) end, List).
		      
bcall(List, Msg) when is_list(List)->
    Replies=lists:map(fun(Performer)->
			      gen_server:call(Performer, Msg)
		      end,
		      List),
    Replies.
    

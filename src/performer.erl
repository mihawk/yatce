%% -*- coding: utf-8 -*-
%% this file is in public domain.

-module(performer).
-author('kuenishi').

-export([
	 start/3,
	 stop/0,
	 run/3
	]).

%%-record(r, {id,content}).

%% usage: 
%%   > performer:start(PathToLibdir, NumThreads, DataAmount).
%%   > 

-import(random).

-define(TABLE_FILE_NAME, '/tmp/performance_test.tch').

generate(0)-> [];
generate(Length)->
    RChar = 96+random:uniform(26),
    [RChar|generate(Length-1)].

start(YatceLib, NumThreads, T)->
    ok = yatce:init([{libdir, YatceLib}]),
    TCADB = yatce:newdb(?TABLE_FILE_NAME, []),
    {ok,opened}=TCADB:open(),
    {A1, A2, A3}=now(),
    random:seed(A1, A2+T, A3),
    Duration = perf(TCADB, NumThreads, T),
    io:format( "Total done: ~p sec, ~p qps~n", 
	       [ Duration / 1000000, NumThreads * T * 1000000 / Duration] ),
    {ok,closed}=TCADB:close(),
    yatce:fini().

perf(TCADB, NumThreads, T)->
    {Duration, _Retval} = timer:tc( ?MODULE, run, [TCADB, NumThreads, T]),
    Duration.

run(TCADB, NumThreads, T)->
    register(  coordinator, self() ),
    spawn_q(NumThreads, T, [], TCADB),
    get_result(NumThreads, 0, TCADB),
    unregister( coordinator ).

get_result(0, TotalDone, UserInfo)->
    TCADB=UserInfo,
    TotalDone;
get_result(P, TotalDone, UserInfo)->
%    io:format("~p , ~p.~n", [P, ResultArray]),
    receive
	{done, Done} ->
	    get_result( P-1, Done+TotalDone, UserInfo )
    end.
	
spawn_q(0, _T, _Processes, _UserInfo)->
    ok;
spawn_q(P, T, Processes, UserInfo)->
    Pid = spawn_link( fun()-> q( T, 0, UserInfo ) end),
    spawn_q(P-1, T, [Pid|Processes], UserInfo).
			

q(0, Done, _UserInfo)->
    coordinator ! {done, Done};
q(N, Done, UserInfo)->
    TCADB=UserInfo,
    {Key, Value} = {generate(16), generate(128)},
    %%     Reader = fun()-> mnesia:read(r, Key, read) end, %% 13k qpsくらい出る　スレッド数によっては20k行く
    
    {ok,inserted}=TCADB:put(Key,Value),
    case TCADB:get(Key) of
	{ok, _Value}-> q(N-1, Done+1, UserInfo);
	{error, _Reason} ->q(N-1, Done+1, UserInfo)
    end.
%%    q(N-1, Done+1, UserInfo).
%%     Writer = fun() ->                               %%  4k qpsくらい出る　
%% 		     Value = generate(256),
%% 		     mnesia:write(#r{id=Key,content=Value})
%% 	     end,
%%     case mnesia:transaction(Reader) of
%% %    case mnesia:transaction(Writer) of
%% 	{atomic, ok}->
%% 	    q(N-1, Done+1);
%% 	{atomic, []} -> 
%%  	{atomic, [E]} -> 
%% 	    q(N-1, Done+1);
%%  	{aborted, Reason} ->
%% 	    io:format("transaction failed: ~p~n", [Reason]),
%%  	    q(N-1, Done);
%% 	Other ->
%% 	    io:format("transaction failed: ~p~n", [Other])
%%     end.

stop()->
    ok.

%% stop(UserInfo)->
%%     TCADB=UserInfo,
%%     yatce:fini(),
%%     ok.%    mnesia:stop().
			    

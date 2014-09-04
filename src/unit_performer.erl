%% -*- coding: utf-8 -*-

-module(unit_performer).
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
%%  supervisor -+- controller (gen_server)
%%              +- timer      (gen_server)
%%              +- unit_performer x N  (gen_server)
%%
%% where N is scale of parallel process hits TC.

%% basic usage:
%%  > yatce:init().
%%  > T=yatce:newdb('/tmp/some.tch', []).
%%  > T:open().
%%  > F=fun(T)-> T:put("key","value") end.
%%  > gen_server:start({local,test},unit_performer,[],[]).
%%  > gen_server:call(test,{init,F,T}).
%%  > gen_server:call(test,test).
%%  > gen_server:cast(test,start).
%%  > gen_server:call(test,poll).
%%  > gen_server:cast(test,stop).
%%  > T:close() %% don't forget!
%%  > yatce:fini().	     

-record( perfdata, 
	 { 
	   unit_func = fun(_) -> ok end,
	   tc_handle = none,
	   count     = 0,
	   timeout   = 100  %% milliseconds
	  }
	 ).

init([])->
    {ok, #perfdata{}}.

%% synchronous call
handle_call({init, Func, TCADB}, _From, _State)-> 
    NewState = #perfdata{unit_func = Func, tc_handle=TCADB},
    {reply, ok, NewState};

handle_call({set_func, Func}, _From, State) ->
    {reply, ok, State#perfdata{unit_func=Func}};
handle_call({set_handle, TCADB}, _From, State) ->
    {reply, ok, State#perfdata{tc_handle=TCADB}};
handle_call({set_timeout, Timeout}, _From, State) ->
    {reply, ok, State#perfdata{timeout=Timeout}};

handle_call(test, _From, State) when is_record(State, perfdata)->
    io:format("~p:~p~n", [?LINE, State]),
    Reply = (State#perfdata.unit_func)( State#perfdata.tc_handle ),
    {reply, Reply, State, State#perfdata.timeout};

handle_call(poll, _From, State) ->
    {reply, State#perfdata.count, State#perfdata{count=0}, State#perfdata.timeout};

handle_call(stop, _From,  State) when is_record(State, perfdata) ->
    NewFun = fun(_)->ok end,
    {reply, ok, State#perfdata{unit_func=NewFun, count=0}};

handle_call(_,_,State) ->
    {noreply,  State}.
   
%% asynchronous call
handle_cast(start, State) when is_record(State, perfdata) -> %%and State#perfdata.tc_handle =/= none->
    {noreply, State, State#perfdata.timeout};
handle_cast(_, State) ->
    {noreply, State}.

%% other call
handle_info(timeout, State) when is_record(State, perfdata) -> % and is_function(State#perfdata.tc_handle, 1)->
%%    io:format("~p:~p~n", [?LINE, State]),
    case (State#perfdata.unit_func)( State#perfdata.tc_handle ) of
	{ok, _}->
	    Count = State#perfdata.count+1,
	    {noreply, State#perfdata{count=Count}, State#perfdata.timeout};
	_->
	    {noreply, State, State#perfdata.timeout}
    end;

handle_info(_, State) ->
    io:format("~p:~p~n", [?LINE, State]),
    {noreply, State, State#perfdata.timeout}.

terminate(_Reason, _State)->    ok.
code_change(_OldVsn, State, _Extra)-> {ok, State}.

    
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


%% -*- coding: utf-8 -*-
-module(yatce).
-author('kuenishi+tc@gmail.com').
-behaviour(application).

-export([start/2, stop/1]).
-export([start/0, stop/0, db/2]).
-include("tokyocabinet.hrl").

start(_Type, _RestartType)->
    LibPath = code:priv_dir(?MODULE)++ "/lib",
%    io:format("~p ~p: ~p~n", [?FILE, ?LINE, LibPath]),
    case erl_ddll:try_load(LibPath, ?DYLIB_NAME, []) of
	{ok, _Status} -> 
	    supervisor:start_link( {local, yatce_sup}, yatce_sup, [] );
        {error, already_loaded} ->
	    supervisor:start_link( {local, yatce_sup}, yatce_sup, [] );
	{error, Reason}->
	    io:format("you have to set exactly where libyatce.so exists: ~p , ~p~n",
		      [ erl_ddll:format_error(Reason), LibPath ]),
	    {error, Reason};
	Other ->
	    io:format("~p~n", [Other]),
	    {error, Other}
    end.

stop(_State)->
    SharedLib = ?DYLIB_NAME,
%    io:format("~p (~p, ~p) terminating.~n", [?MODULE, self(), State]),
    case erl_ddll:unload(SharedLib) of
	{error, ErrorDesc}->
	    io:format("can't unload ~p: ~p~n", [ SharedLib, erl_ddll:format_error(ErrorDesc) ]);
	ok ->
	    ok
    end.

start()-> application:start(?MODULE).    
stop()->  application:stop(?MODULE).    

-spec db( tablename(), [option()] ) -> tcadb().
db(TableName, Options)->
    yatce_server:newdb(TableName,Options).


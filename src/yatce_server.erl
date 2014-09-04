%% -*- coding: utf-8 -*-
-module(yatce_server).
-author('kuenishi+tc@gmail.com').

-include("tokyocabinet.hrl").
-import(random).

-record(yatce, {port, shared_lib}).

-export([
	 start/0,	 start/1,
	 start_link/0,	 start_link/1,
	 stop/0,
	 newdb/2,
	 encode/1,	 decode/1,
	 test/0
	 ]).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2 ]).

-spec newdb( tablename(), [option()] ) -> tcadb().
newdb(TableName, Options)->
    {ok, Port} = get_port(),
    FullTableName = generate_open_argv(TableName, Options),
    tcadb:new(FullTableName, Options, Port ).

-spec start()->{ok, pid()}.
start()->   start([{libdir, "c_src"}]).

-spec start( list() )->{ok, pid()}.
start(Options)->
%    case gen_server:start({local, ?MODULE},?MODULE, Options, [{debug, [trace,log,statistics]}]) of
    case gen_server:start({local, ?MODULE},?MODULE, Options, []) of
	{ok, Pid}->
	    timer:sleep(500),
	    {ok, Pid};
	{error, {already_started, Pid}} ->
	    timer:sleep(500),
	    {ok, Pid};
	Other ->
	    Other
    end.

-spec start_link()->{ok, pid()}.
start_link()->   start_link([{libdir, "c_src"}]).

-spec start_link( list() )->{ok, pid()}.
start_link(Options)->
    case gen_server:start_link({local, ?MODULE},?MODULE, [Options], [{debug, [trace,log,statistics]}]) of
	{ok, Pid}->
	    timer:sleep(500),
	    {ok, Pid};
	{error, {already_started, Pid}} ->
	    timer:sleep(500),
	    {ok, Pid};
	Other ->
	    Other
    end.


-spec stop() -> {stop, atom(), ok, any()}.
stop()->
    Res = gen_server:call(?MODULE, stop),
    Res.

%%~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~%%
%%                     FUNCTIONS FOR INTERNAL USE                    %%
-spec encode( term() ) -> binary().
encode(Object)->
    term_to_binary(Object).
    
%% C drivers can't return in raw binary (don't know why; 
%% set_port_control_flags doesn't work, causes error.
-spec decode( list() ) -> term().
decode(Object) when is_list(Object) ->
    binary_to_term( list_to_binary(Object) ).


%% Create a new TCADB instance. you can use this TCADB concurrently.
%% Take care when closing the instance.
%% http://tokyocabinet.sourceforge.net/spex-ja.html#tcadbapi
%% `name' specifies the name of the database. 
%% If it is "*", the database will be an on-memory hash database. If it is "+", the database will be an on-memory tree database. If its suffix is ".tch", the database will be a hash database. If its suffix is ".tcb", the database will be a B+ tree database. If its suffix is ".tcf", the database will be a fixed-length database. If its suffix is ".tct", the database will be a table database. Otherwise, this function fails. 
%% Tuning parameters can trail the name, separated by "#". 
%% Each parameter is composed of the name and the value, separated by "=". 
%% - On-memory hash database supports "bnum", "capnum", and "capsiz". 
%% - On-memory tree database supports "capnum" and "capsiz". 
%% - Hash database supports "mode", "bnum", "apow", "fpow", "opts", "rcnum", "xmsiz", and "dfunit". 
%% - B+ tree database supports "mode", "lmemb", "nmemb", "bnum", "apow", "fpow", "opts", "lcnum", "ncnum", "xmsiz", and "dfunit".
%% - Fixed-length database supports "mode", "width", and "limsiz". 
%% - Table database supports "mode", "bnum", "apow", "fpow", "opts", "rcnum", "lcnum", "ncnum", "xmsiz", "dfunit", and "idx".
%% Tuning parameters
%% + The tuning parameter "capnum" specifies the capacity number of records. 
%% "capsiz" specifies the capacity size of using memory. Records spilled the capacity are removed by the storing order. 
%% "mode" can contain "w" of writer, "r" of reader, "c" of creating, "t" of truncating, "e" of no locking, and "f" of non-blocking lock. The default mode is relevant to "wc". 
%% "opts" can contains "l" of large option, "d" of Deflate option, "b" of BZIP2 option, and "t" of TCBS option. 
%% "idx" specifies the column name of an index and its type separated by ":". 
%% For example, "casket.tch#bnum=1000000#opts=ld" means that the name of the database file is "casket.tch", and the bucket number is 1000000, and the options are large and Deflate.
-type option_key() :: bnum | capnum | capsiz | mode | bnum | apow | fpow | opts | rcnum | xmsiz | dfunit .
-type option_value() :: pos_integer() | list() .
-type open_option() :: { option_key(), option_value() }.
-spec generate_open_argv( atom() | list() , [ open_option() ] )-> atom().
generate_open_argv(TableName, []) when is_atom( TableName ) ->
    TableName;

generate_open_argv(TableName, Options) when is_atom( TableName ) ->
    generate_open_argv( atom_to_list(TableName), Options );

generate_open_argv(TableName, []) when is_list( TableName ) ->
    list_to_atom(TableName);

generate_open_argv(TableName, [{Key,Value}|Options]) when is_list( TableName ) and is_integer(Value)->
    StrKey = atom_to_list(Key),
    generate_open_argv( TableName++"#"++StrKey++"="++io_lib:write(Value), Options );
generate_open_argv(TableName, [{Key,Value}|Options]) when is_list( TableName ) and is_list(Value)->
    StrKey = atom_to_list(Key),
    generate_open_argv( TableName++"#"++StrKey++"="++Value, Options).

%% -spec generate_open_argv_( list(), list() )-> atom().
%% generate_open_argv_(TableName, Options) when is_list(TableName) ->
%%     TableName.

-spec test() -> {ok,all_passed}|{error, test_failed}.
test()->
    gen_server:call(?MODULE, test).

-spec get_port() -> {ok, port()} | {error, any()}.
get_port()->
    case gen_server:call(?MODULE, get_port) of
	Port when is_port(Port)->
	    {ok, Port};
	Other ->
	    {error, Other}
    end.

-spec init( [ option() ] ) -> ok | {error, erl_ddll_error_desc()}.
init(Options)->
    SharedLib = ?DYLIB_NAME,
    Port = open_port({spawn_driver, SharedLib}, [binary]),
    {ok, #yatce{port=Port,shared_lib=SharedLib}}.

handle_call(get_port, _From, State)->
    {reply, State#yatce.port, State};
handle_call(stop, _From, State) ->
    Port = State#yatce.port,
    Port ! {self(), close},
    receive 
	{Port , closed}->
	    {stop, normal, ok, State};
	_ -> 
	    {stop, cant_close_port, ok, State}
    after 5000->
	    {stop, cant_close_port, ok, State}
    end;
handle_call(test, _From, State)->
    Port = State#yatce.port,
    Result=decode( port_control(Port, ?YATCE_TEST, list_to_binary([]) ) ),
    {reply, Result, State};
    
handle_call(_, _From, State) ->    {noreply, State}.


handle_cast(_,State)->    {noreply, State}.

handle_info(_,State)->    {noreply, State}.

terminate(Reason,State)-> 
    io:format("~p (~p, ~p) terminating.~n", [?MODULE, self(), State]),
    case erl_ddll:unload(State#yatce.shared_lib) of
	{error, ErrorDesc}->
	    io:format("can't unload ~p: ~p~n", [ State#yatce.shared_lib, erl_ddll:format_error(ErrorDesc) ]);
	ok ->
	    ok
    end,
    {shutdown, Reason}.
    
code_change(_,_,State)->
    {ok, State}.


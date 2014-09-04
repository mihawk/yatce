#!/usr/bin/env escript

%% $ ./tctest.erl  c_src 
%%  or
%% $ escript tctest.erl <path to libyatce.so>
tcadb_test()->
%    io:format("~p~n", [YatceLib]),
    T = yatce:db('/tmp/test.tch', []),                 % tcadbnew()
    {ok,opened} =T:open(),                                % tcadbopen()
    str_str(T),
    str_int(T),
    str_bin(T),
    {ok, sync} = T:sync(),
    {ok, vanished} = T:vanish(), io:format( "~p~n", ["vanish passed."]),
    io:format("tcadb:path/0 -> ~p, tcadb:rnum/0 -> ~p, tcadb:size/0 -> ~p~n", [T:path(), T:rnum(), T:size()]),
    str_tuple(T),
    str_ref(T),
    {ok, closed} = T:close().

str_str(T)->
    {ok,inserted}=T:put("key","value"),
    {ok,"value"} =T:get("key"),
    io:format( "~p~n", ["-str_str passed."]),
    {ok,deleted}=T:out("key"),
    {error,record_doesnt_exist}=T:get("key").

str_int(T)->    
    {ok,inserted}=T:put("key2", 12),
    {ok,12} =T:get("key2"),
    io:format( "~p~n", ["str_int passed."]).

str_bin(T)->    
    {ok,inserted}=T:put("key2", <<"binary!!!!">>),
    {ok,<<"binary!!!!">>} =T:get("key2"),
    io:format( "~p~n", ["str_bin passed."]).

str_tuple(T)->    
    Key = {key, hoge}, Value = <<"binary!!!!">>,
    {ok,inserted}=T:put(Key, Value),
    {ok,Value} =T:get(Key),
    io:format( "~p passed.~n", [str_tuple]).

str_ref(T)->    
    Key = hage_hoge_ref, Value = make_ref(),
    {ok,inserted}=T:put(Key, Value),
    {ok,Value} =T:get(Key),
    io:format( "~p passed.~n", [str_ref]).

iter()->
    List=[{"key","value"}, {"afkls;d", 23536}, {"afd;sj", <<"q34refdv]90i">>}],
    T=yatce:db('/tmp/test2_iter.tch', []),
    {ok,opened}=T:open(),
    {ok,init}=T:iterinit(),
    ok = insert_all(T,List),
    ok = iter_(T,List),
    {ok,closed}=T:close().

insert_all(_, [])-> ok;
insert_all(T, List) ->
    {Key,Value} = hd(List),
    {ok, inserted} = T:put(Key,Value),
    insert_all(T,tl(List)).

iter_(_,[])-> 
    io:format( "~p~n", ["iter passed."]);
iter_(T,List) ->
    {ok,Key}=T:iternext(),
    {ok,Value}=T:get(Key),    
    case lists:member({Key,Value}, List) of
	true ->
	    iter_(T, lists:subtract(List, [{Key,Value}]));
	false ->
	    ng
    end.

main(_Argv)->
%    [YatceLib|_] = Argv,
    code:add_path("./ebin"),
    ok=application:start(yatce),
%    erlang:display( application:get_env(yatce, libdir) ),
%    {ok, _Pid} = yatce:start([{libdir, YatceLib}]), % initialize the library
    {ok, all_passed} = yatce_server:test(),
    tcadb_test(),
    iter(),
%    yatce:stop().
    application:stop(yatce).
    

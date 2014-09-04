%% -*- coding: utf-8 -*-

-module(tcadb_SUITE).
-author('kuenishi@gmail.com').

-compile(export_all).

-include("ct.hrl").

-define(TESTKEY, "testkeyhogehoge").
-define(TESTVALUE, "se5dr67vt8bynu9im03w4ed5rf6t7gy8hu9ji0kf\poi").
-define(TESTVALUE2, "|+}{\":?u9im03w4ed5rf6t7gy8hu9ji0kf\poi").

init_per_suite(Config) ->
    ok=application:start(yatce),
    Config.

end_per_suite(Config) ->  
    yatce:stop(),
    Config.

all() -> [{group, tch},
	  {group, tcb}
%	  ,{group, tcf} 
%	  ,{group, tct}
%	  ,{group, '*'}
%	  ,{group, '+'}
	 ].

groups()->
    Suffices = [tch,tcb,tcf,tct, '*', '+'],
    Set= [  open, put, get,  putkeep, vanish, iterator, close ],
    [ {X, [sequencial], Set} || X <- Suffices ].

new_config(TableName, Config) when is_tuple(TableName) ->
    {TN1, TN2} = TableName,
    T1 = yatce:db(TN1, []),    
    T2 = yatce:db(TN2, []),
    NewConfig = [{t1, T1}, {t2, T2} ] ++ Config,
    NewConfig.
    
reset_config(Config)->
    T1 = ?config(t1, Config),
    T2 = ?config(t2, Config),
    TmpConfig = lists:delete({t1, T1}, Config),
    NewConfig = lists:delete({t2, T2}, TmpConfig),
    ct:log("Config: ~p~n", [NewConfig] ),
    NewConfig.

init_per_group(tch,Config)->
    new_config( {'/tmp/test.tch','/tmp/test2.tch'}, Config );
init_per_group(tcb,Config)->
    new_config( {'/tmp/test.tcb','/tmp/test2.tcb'}, Config );
init_per_group(tcf,Config)->
    new_config( {'/tmp/test.tcf','/tmp/test2.tcf'}, Config );
init_per_group(tct,Config)->
    new_config( {'/tmp/test.tct','/tmp/test2.tct'}, Config );
init_per_group('*',Config)->    new_config( {'*', '+'}, Config );
init_per_group('+',Config)->    new_config( {'+', '*'}, Config ).


end_per_group(tch,Config)->    reset_config(Config);
end_per_group(tcb,Config)->    reset_config(Config);
end_per_group(tcf,Config)->    reset_config(Config);
end_per_group(tct,Config)->    reset_config(Config);
end_per_group('*',Config)->    reset_config(Config);
end_per_group('+',Config)->    reset_config(Config).


open(Config)->
    T1 = ?config(t1, Config),
    T2 = ?config(t2, Config),
    {ok,opened}=T1:open(),
    {ok,opened}=T2:open(),
    Config.

put(Config)->
    T1 = ?config(t1, Config),
    T2 = ?config(t2, Config),
    {ok,inserted}=T1:put(?TESTKEY, ?TESTVALUE),
    {ok,inserted}=T2:put(?TESTKEY, ?TESTVALUE2),
    Config.

get(Config)->
    T1 = ?config(t1, Config),
    T2 = ?config(t2, Config),
    {ok,?TESTVALUE}=T1:get(?TESTKEY),
    {ok,?TESTVALUE2}=T2:get(?TESTKEY),
    Config.

putkeep(Config)->
    Key = "putkeep_test",
    V1 = "hoge",
    V2 = "huga",
    T = ?config(t1, Config),
    {ok, inserted} = T:put(Key,V1),
    {error, _} = T:putkeep(Key,V2),
    {ok, V1} = T:get(Key),
    {ok, deleted}  = T:out(Key),
    Config.

vanish(Config)->
    T = ?config(t1, Config),
    {ok, vanished}  = T:vanish(),
    Config.

iterator(Config)->
    T = ?config(t1, Config),
    List=[{"key","value"}, {"afkls;d", 23536}, {"afd;sj", <<"q34refdv]90i">>}],
    {ok,vanished}=T:vanish(),
    ok = insert_all(T,List),
    {ok,init}=T:iterinit(),
    ok = iter_(T,List),
    Config.

close(Config)->
    T1 = ?config(t1, Config),
    T2 = ?config(t2, Config),
    {ok,closed}=T1:close(),
    {ok,closed}=T2:close(),
    Config.

insert_all(_, [])-> ok;
insert_all(T, List) ->
    {Key,Value} = hd(List),
    ct:log("~p key: ~p, value: ~p ~n", [T, Key,Value]),
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

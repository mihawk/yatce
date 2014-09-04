%% -*- coding: utf-8 -*-
%%   yatce
%%   Copyright (C) 2009   UENISHI Kota <kuenishi+yatce@gmail.com>
%%
%%     This program is free software: you can redistribute it and/or modify
%%     it under the terms of the GNU Lesser General Public License as published by
%%     the Free Software Foundation, either version 3 of the License, or
%%     (at your option) any later version.
%%
%%     This program is distributed in the hope that it will be useful,
%%     but WITHOUT ANY WARRANTY; without even the implied warranty of
%%     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%     GNU Lesser General Public License for more details.
%%
%%     You should have received a copy of the GNU Lesser General Public License
%%     along with this program.  If not, see <http://www.gnu.org/licenses/>.

-module(elementary_SUITE).
-author('kuenishi@gmail.com').

-compile(export_all).

-include("ct.hrl").

-define(TESTKEY, "testkeyhogehoge").
-define(TESTVALUE, "se5dr67vt8bynu9im03w4ed5rf6t7gy8hu9ji0kf\poi").

%% config(_, [])-> nil;
%% config(Key, Config)->
%%     [{TmpKey,Value}|NextConfig]=Config,
%%     case TmpKey of
%% 	Key->
%% 	    Value;
%% 	_ ->
%% 	    config(Key,NextConfig)
%%     end.

init_per_suite(Config) ->
    ok=application:start(yatce),
    Config.

end_per_suite(Config) ->
    yatce:stop(),
    Config.
%% TBD: doesn't work, don't know why.
%    {save_config, Config}.

all() -> [{group, tch},
	  {group, tcb}
%	  ,{group, tcf}
%	  ,{group, tct} %% in Debian, .tct test doesn't work.
	  ,{group, '*'}
%	  ,{group, '+'}
	 ].

groups()->
    Set= [open_close, insertion_only, one_path, deletion, putkeep,    %%	  putcat,
	  path_test, sync_test, vanish_test, iter_test, rnum_test, size_test ],
    Suffices = [tch,tcb,tcf,tct], 
    MemSet = [open_close, insertion_only, one_path, deletion, putkeep,    %%	  putcat,
	      path_test, vanish_test, iter_test, rnum_test, size_test ],
    MemSuffices = ['*', '+'],
    [ {X, [sequencial], Set} || X <- Suffices ] ++ [ {X, [sequencial], MemSet} || X <- MemSuffices].
	   
new_config(TableName, Config) when is_atom(TableName) ->
    TCADB = yatce:db(TableName, []),
    NewConfig = [{tablename, TableName}, {tcadb, TCADB} ] ++ Config,
    NewConfig.
    
reset_config(Config)->
    TableName = ?config(tablename, Config),
    TCADB = ?config(tcadb, Config),
    TmpConfig = lists:delete({tablename,TableName}, Config),
    NewConfig = lists:delete({tcadb, TCADB}, TmpConfig),
    NewConfig.

init_per_group(tch,Config)->    new_config( '/tmp/test.tch', Config );
init_per_group(tcb,Config)->    new_config( '/tmp/test.tcb', Config );
init_per_group(tcf,Config)->    new_config( '/tmp/test.tcf', Config );
init_per_group(tct,Config)->    new_config( '/tmp/test.tct', Config );
init_per_group('*',Config)->    new_config( '*', Config );
init_per_group('+',Config)->    new_config( '+', Config ).

end_per_group(tch,Config)->    reset_config(Config);
end_per_group(tcb,Config)->    reset_config(Config);
end_per_group(tcf,Config)->    reset_config(Config);
end_per_group(tct,Config)->    reset_config(Config);
end_per_group('*',Config)->    reset_config(Config);
end_per_group('+',Config)->    reset_config(Config).

open_close(Config)->
    TCADB = ?config(tcadb, Config),
    {ok, opened} = TCADB:open(),
    {ok, closed} = TCADB:close(),
    Config.

insertion_only(Config)->
    TCADB = ?config(tcadb, Config),
    {ok, opened} = TCADB:open(),
    {ok, inserted} = TCADB:put(?TESTKEY, ?TESTVALUE),
    {ok, closed} = TCADB:close(),
    Config.

one_path(Config)->
    TCADB = ?config(tcadb, Config),
    ct:log("~p key: ~p, value: ~p ~n", [TCADB, ?TESTKEY, ?TESTVALUE]),
    {ok, opened} = TCADB:open(),
    {ok, inserted} = TCADB:put(?TESTKEY, ?TESTVALUE),
    {ok,?TESTVALUE}=TCADB:get(?TESTKEY),
    {ok, closed} = TCADB:close(),
    Config.

deletion(Config)->
    TCADB = ?config(tcadb, Config),
    {ok, opened} = TCADB:open(),
%    ct:log("~p key: ~p, value: ~p ~n", [TCADB, ?TESTKEY, 
    ct:log("~p key: ~p~n", [TCADB, ?TESTKEY]),
    {ok, inserted} = TCADB:put(?TESTKEY, ?TESTVALUE),
    {ok, deleted}= TCADB:out(?TESTKEY),
    {error, record_doesnt_exist}=TCADB:get(?TESTKEY),
    {ok, closed} = TCADB:close(),
    Config.

putkeep(Config)->
    TCADB = ?config(tcadb, Config),
    {ok, opened} = TCADB:open(),
    {ok, inserted}= TCADB:putkeep(?TESTKEY, ?TESTVALUE),
    {ok, closed} = TCADB:close(),
    Config.
    
putcat(Config)->
    TCADB = ?config(tcadb, Config),
    {ok, opened} = TCADB:open(),
    {ok, appended}= TCADB:putcat(?TESTKEY, ?TESTVALUE),
    {ok, closed} = TCADB:close(),
    Config.

intvalue(Config)->
    TCADB = ?config(tcadb, Config),
    {ok, opened} = TCADB:open(),
    lists:map( fun({Key,Value})->
		       {ok,inserted} = TCADB:put(Key,Value)
	       end,
	       get_test_data()
	      ),
    {ok, closed} = TCADB:close(),
    Config.

path_test(Config)->    
    TCADB = ?config(tcadb, Config),
    TableName = ?config( tablename, Config ),
    TableName = TCADB:path(),
    Config.

sync_test(Config)->
    TCADB = ?config(tcadb, Config),
    {ok, opened} = TCADB:open(),
    {ok, sync} = TCADB:sync(), % see tc_wrapper.c#tc_sync for details
    {ok, closed} = TCADB:close(),
    Config.

vanish_test(Config)->
    TCADB = ?config(tcadb, Config),
    {ok, opened} = TCADB:open(),
    {ok, vanished} = TCADB:vanish(), % see tc_wrapper.c#tc_sync for details
    {ok, closed} = TCADB:close(),
    Config.

iter_test(Config)->
    TCADB = ?config(tcadb, Config),
    {ok, opened} = TCADB:open(),
    {Key,Value} = {"hogekey", "hogevalue"},
    {ok, inserted } = TCADB:put(Key,Value),
    {ok, init} = TCADB:iterinit(),
    {ok, Key} = TCADB:iternext(),
    {ok, Value} = TCADB:get(Key),
    {ok, closed} = TCADB:close(),
    Config.

rnum_test(Config)->
    TCADB = ?config(tcadb, Config),
    {ok, opened} = TCADB:open(),
    {ok, Rnum} = TCADB:rnum(),
    if not is_integer(Rnum)-> 
	    {ok, closed} = TCADB:close(),
	    ct:fail( "tcadb:rnum/0 failed" );
       true ->
	    ct:log("rnum for ~p: ~p.~n", [TCADB:path(), Rnum]),
	    {ok, closed} = TCADB:close()
    end,
    Config.
 
size_test(Config)->
    TCADB = ?config(tcadb, Config),
    {ok, opened} = TCADB:open(),
    {ok, Size} = TCADB:size(),
    if not is_integer(Size)->
 	    {ok, closed} = TCADB:close(),
	    ct:fail( "tcadb:size/0 failed" );
       true ->
	    ct:log("size for ~p: ~p.~n", [TCADB:path(), Size]),
	    {ok, closed} = TCADB:close()
    end,
    Config.

get_test_data()->
    [
     { "strkey_intvalue",            0},
     { "strkey_doublevalue",   234.345},
     { "strkey_binvalue", <<"asdfasf34r>>">> },
     { 0,       "intkey_strvalue____" },
     { 123,    <<"afsda4rfa">>        },
     { 31245,   24578.134985          },
     { "counter",   0 }
    ].

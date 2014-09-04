-module(perf_sup).
-author('kuenishi').

-behaviour(supervisor).
-export([start/2, init/1, stop/0,
	 go/0, poll/0]).

%% usage: 
%%   > performer_sup:start(FileName, NumProcesses).
%%   > performer_sup:stop().

%% current architecture:
%%  supervisor -+- controller
%%              +- timer
%%              +- unit_performer x N
%%
%% where N is scale of parallel process hits TC.
%%
%%
%% next architecture:
%%  supervisor -+- controller
%%              +- timer
%%              +- group_supervisor a -+- unit_performer x Na
%%              |                      +- group_controller
%%              +- group_supervisor b -+- unit_performer x Nb
%%              |                      +- group_controller
%%              +- group_supervisor c -+- unit_performer x Nc
%%              |                      +- group_controller
%%              +- group_supervisor d -+- unit_performer x Nd
%%              |                      +- group_controller
%%              +- ...
%%              ...
%%
%% where N is scale of parallel process hits TC, each group does different access.

-spec generate( non_neg_integer() )-> list().
generate(0)-> [];
generate(Length)->   RChar = 96+random:uniform(26),  [RChar|generate(Length-1)].

go()->
    start('/tmp/test_sup.tch',256),
    gen_server:call(perf_controller, 
		    {set_func, fun(T)->
				       L=random:uniform(32),
				       Key=generate(L),
				       T:put(Key,generate(256)),
				       T:get(Key)
			       end }),
    gen_server:call(perf_controller, start).
poll()->
    gen_server:call(perf_controller, poll).

-type filename() :: list() | atom().
-spec start( filename(), pos_integer() ) -> ok.
start(FileName, NumProcesses) when is_list(FileName) ->
    start( list_to_atom(FileName), NumProcesses );
start(FileName, NumProcesses) when is_atom(FileName) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [FileName, NumProcesses]).

stop()->
    gen_server:call(perf_controller,stop).

-spec init( list() )-> list(). %child_spec().
init([FileName, NumProcesses])->
    %% random seed
    {A1, A2, A3}=now(),
    random:seed(A1, A2, A3),
    %% create childspec
    WatcherFreq = 5000, %msec
    {UnitPerformerNames, UnitPerformerSpecs} = generate_children(NumProcesses, [], []),
    Children = [
		{yatce, 
		 {yatce, start_link, [{libdir, "./c_src"}]}, 
		 permanent, 2000, worker, [yatce]
		},
 		{perf_timer, 
 		 {gen_fsm, start_link, [{local,perf_timer}, perf_timer, WatcherFreq, [{debug,[trace,log,statistics]}]]},
 		 permanent, 2000, worker, [perf_timer]
 		},
    		{perf_controller,
    		 {gen_server, start_link, [{local,perf_controller}, perf_controller, {UnitPerformerNames, FileName},
					   []]},
%%					   [{debug,[trace,log,statistics]}]]},
    		 permanent, 2000, worker, [perf_controller]
    		}
	       ],
%%    io:format("~p: starting: ~p~n", [?MODULE, Children++UnitPerformerSpecs]),
    ok=supervisor:check_childspecs(UnitPerformerSpecs++Children),
    {ok, {{one_for_one, 1, 1}, UnitPerformerSpecs++Children}}.
%    {ok, {{one_for_one, 1, 1}, Children}}.
%    {ok, {{one_for_one, 1, 1}, UnitPerformerSpecs}}.

-spec generate_children(non_neg_integer(), list(), list() ) -> { list() | list() }.
generate_children(0, NameList, SpecList)-> { NameList,  SpecList };
generate_children(Num, NameList, SpecList)->
    Id = list_to_atom("unit_performer" ++ integer_to_list(Num-1)),
    Child = {Id, {gen_server, start_link, 
	      [{local,Id}, unit_performer,[], 
	       []]}, %%[{debug,[trace,log,statistics]}]]}, 
	     permanent, 2000, worker, [unit_performer]},
    generate_children(Num-1, [Id|NameList], [Child|SpecList]).
    

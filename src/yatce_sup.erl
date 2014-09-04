%% -*- coding: utf-8 -*-
-module(yatce_sup).
-author('kuenishi+tc@gmail.com').

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link()->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([])->
    Children =
	[
	 {yatce_server, {yatce_server, start, []}, permanent, 2000, worker, [yatce_server]}
	],
%    io:format("starting: ~p~n", [Children]),
    {ok, {{one_for_one, 1, 1}, Children}}.

-module(erlln_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, stop/1, init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE} , ?MODULE, []).

stop(_State) ->
    ok.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(_Args) ->
    {ok,
     {{one_for_one, 0, 10},
      [{erlln_srv,
        {erlln_srv, start_link, []},
        permanent, brutal_kill, worker,
        [erlln_srv]}]}}.

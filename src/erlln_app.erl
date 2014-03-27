-module(erlln_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================
start(_Type, _Args) ->
    erlln_sup:start_link().

stop(_State) ->
    erlln_sup:stop(),
    ok.

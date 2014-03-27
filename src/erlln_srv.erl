-module(erlln_srv).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

-define(host, "localhost").
-define(port, 5432).
-define(dbname, "postgres").
-define(user, "postgres").

-include_lib("deps/epgsql/include/pgsql.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, stop/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {conn}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:cast(self(), shutdown).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    {ok, Conn} = pgsql_connect_and_listen(),
    io:format("erlln server started~n"),
    {ok, #state{conn=Conn}}.

handle_call(_Message, _From, State) ->
    {reply, ok, State}.

%% Close the PostgreSQL connection when shutting down
handle_cast(shutdown, State) ->
    io:format("Shutting down~n"),
    pgsql:close(State#state.conn),
    {stop, normal, State};

handle_cast(_Message, State) ->
    {noreply, State}.

%% This will be invoked when epgsql sends a message to the process with the notify info
%% and only match when the channel name is erlln and the payload is stop
handle_info({pgsql, _Conn, {notification, <<"erlln">>, _Pid, <<"stop">>}}, State) ->
    io:format("~nReceived stop request via NOTIFY~n"),
    gen_server:cast(self(), shutdown),
    {noreply, State};

%% This will be invoked when epgsql sends a message to the process with the notify info
handle_info({pgsql, _Conn, {notification, Channel, _Pid, Payload}}, State) ->
    io:format("~nReceived NOTIFY on channel ~s: ~s~n", [Channel, Payload]),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, _State) ->
    io:format("Terminating: ~s~n", [Reason]),

    %% Will stop the erl interpreter when running interactively, probably a bad hack to do
    init:stop(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

pgsql_connect_and_listen() ->
    %% Connect to PostgreSQL -- {async, self()} allows the notifications to work
    {ok, Conn} = pgsql:connect(?host, ?user, "",
                               [{database, ?dbname}, {async, self()}]),

    %% Send the query to turn on LISTEN mode
    {ok, _, _} = pgsql:squery(Conn, "LISTEN erlln"),
    {ok, Conn}.

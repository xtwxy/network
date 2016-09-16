-module(tcp_echo_server_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, start_child/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []). 

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    %%{ok, Port} = application:get_env(port),
    {ok, ListenSocket} = gen_tcp:listen(2016, [{active,once}, {packet,line}]),
    spawn_link(fun empty_listeners/0),

    RestartStrategy = simple_one_for_one,
    MaxRestarts = 0,
    MaxSecondsBetweenRestarts = 1,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = temporary,
    Shutdown = 2000,
    Type = worker,

    TcpEchoServer = {tcp_echo_server, {tcp_echo_server, start_link, [ListenSocket]},
                   Restart, Shutdown, Type, [tcp_echo_server]},
    {ok, {SupFlags, [TcpEchoServer]}}.

start_child() ->
    supervisor:start_child(?MODULE, []).

empty_listeners() ->
    [start_child() || _ <- lists:seq(1, 8)],
    ok.


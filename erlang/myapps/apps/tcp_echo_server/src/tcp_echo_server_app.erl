-module(tcp_echo_server_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    tcp_echo_server_sup:start_link().

stop(_State) ->
    ok.

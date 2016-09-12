-module(door_driver_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    io:format("door_driver_app started, Pid = ~p~n", [self()]),
    door_driver_sup:start_link().

stop(_State) ->
    io:format("door_driver_app started, Pid = ~p~n", [self()]),
    ok.

-module(keyvalue_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% keyvalue_server Function Exports
%% ------------------------------------------------------------------
-export([start/1, stop/1, lookup/2, put_entry/3, remove_entry/2, rpc/2, loop/2]).

start(ProcessName) ->
	register(ProcessName, spawn(?MODULE, loop, [ProcessName, dict:new()])).

stop(ProcessName) ->
	rpc(ProcessName, stop).

lookup(ProcessName, Key) ->
	rpc(ProcessName, {lookup, Key}).

put_entry(ProcessName, Key, Value) ->
	rpc(ProcessName, {put_entry, Key, Value}).

remove_entry(ProcessName, Key) ->
	rpc(ProcessName, {remove_entry, Key}).

rpc(ProcessName, Request) ->
    ProcessName ! {self(), Request},
    receive
        {ProcessName, Response} ->
            Response
    end.

loop(ProcessName, Dict) ->
    receive
        {From, {lookup, Key}} ->
            From ! {ProcessName, dict:find(Key, Dict)},
            loop(ProcessName, Dict);
        {From, {put_entry, Key, Value}} ->
	    From ! {ProcessName, ok},
	    loop(ProcessName, dict:store(Key, Value, Dict));

	{From, {remove_entry, Key}} ->
	    From ! {ProcessName, ok},
	    loop(ProcessName, dict:erase(Key, Dict));
           
        {From, stop} ->
            From ! {ProcessName, ok};

        {From, Other} ->
	    From ! {ProcessName, {error, Other}},
	    loop(ProcessName, Dict)
    end.

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    {ok, Args}.

handle_call(_Request, _From, State) ->
    case _Request of
        {start, Name} ->
            start(Name),
            {reply, ok, State};
        
        {stop, Name} ->
            stop(Name),
            {reply, ok, State};

        _ ->
            io:format("unknow request: ~p~n", [_Request]),
            {reply, ok, State}
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------


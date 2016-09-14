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
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, dict:new(), [world]).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    io:format("init: ~p~n", [Args]),
    {ok, Args}.

handle_call(_Request, _From, State) ->
    case _Request of
        {store, Key, Value} ->
            NewState = dict:store(Key, Value, State),
            {reply, ok, NewState};

	{find, Key} ->
            Value = dict:find(Key, State),
            {reply, Value, State};
	
        _ ->
            {reply, error, State}
    end.

handle_cast(_Msg, State) ->
    io:format("message: ~p, ~p~n", [_Msg, State]),
    {noreply, State}.

handle_info(_Info, State) ->
    io:format("information: ~p, ~p~n", [_Info, State]),
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("terminate: ~p, ~p~n", [_Reason, _State]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    io:format("code change: ~p, ~p, ~p~n", [_OldVsn, State, _Extra]),
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------


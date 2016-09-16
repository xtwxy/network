-module(tcp_echo_server).
-behaviour(gen_server).

-record(state, {socket}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(ListenSocket) ->
    gen_server:start_link(?MODULE, ListenSocket, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(ListenSocket) ->
    gen_server:cast(self(), accept),
    {ok, #state{socket=ListenSocket}}.

handle_call(_Request, _From, State) ->
    io:format("_Request = ~p, _From = ~p, State = ~p~n", [_Request, _From, State]),
    {reply, ok, State}.

handle_cast(accept, S = #state{socket=ListenSocket}) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    tcp_echo_server_sup:start_child(),
    {noreply, S#state{socket=Socket}};
    
handle_cast(_Msg, State) ->
    io:format("_Msg = ~p, State = ~p~n", [_Msg, State]),
    {noreply, State}.

handle_info({tcp, Socket, Data}, State) ->
    gen_tcp:send(Socket, Data),
    inet:setopts(Socket, [{active, true}]),
    {noreply, State};

handle_info({tcp_closed, Socket}, State) ->
    io:format("Socket = ~p closed.~n", [Socket]),
    {stop, normal, State};

handle_info(_Info, State) ->
    io:format("_Info = ~p, State = ~p~n", [_Info, State]),
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("_Reason = ~p, State = ~p~n", [_Reason, _State]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    io:format("_OldVsn = ~p, State = ~p, _Extra = ~p~n", [_OldVsn, State, _Extra]),
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------


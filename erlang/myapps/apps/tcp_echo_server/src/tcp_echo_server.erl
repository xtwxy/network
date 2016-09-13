-module(tcp_echo_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-define(TCP_PORT, 2016).
-define(TCP_LISTEN_OPTIONS, [binary,
		      {packet, 0}, 
		      {reuseaddr,true}]).
-define(TCP_CONNECTION_OPTIONS, [binary,
				 {packet, 0},
				 {active, once},
				 {nodelay, true}]).

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
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([handle_listen/1, handle_accept/1, loop_socket/1]).

%% ------------------------------------------------------------------
%% tcp_echo_server Function Definitions
%% ------------------------------------------------------------------

handle_listen(Args) ->
	io:format("handle_listen(): ~n", []),
	case gen_tcp:listen(?TCP_PORT, ?TCP_LISTEN_OPTIONS) of
		{ok, Listen} ->
			io:format("Listen success: ~p~n", [Listen]),
			spawn(fun() -> handle_accept(Listen) end),
			{ok, Args};
		{_, _} ->
			io:format("Unknown error~n"),
			{error, "Listen failed."}
	end.

handle_accept(Listen) ->
	case gen_tcp:accept(Listen) of
		{ok, Socket} ->
			io:format("Client accepted: ~p~n", [Socket]),
			spawn(fun() -> handle_accept(Listen) end),
			loop_socket(Socket);
		{_, _} ->
			io:format("Accept failed.: ~p~n", [Listen]),
			{error, "Accept failed."}
	end.

loop_socket(Socket) ->
	inet:setopts(Socket, ?TCP_CONNECTION_OPTIONS),
	receive
		{tcp, Socket, Data} ->
			gen_tcp:send(Socket, Data),
			loop_socket(Socket);

		{tcp_closed, Socket} ->
			gen_tcp:close(Socket),
			io:format("Connection closed: ~p~n", [Socket]);

		{_, _} ->
			gen_tcp:close(Socket),
			io:format("Unknown error~n"),
			{error, "Socket failed."}
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
    io:format("init(): Args = ~p~n", [Args]),
    handle_listen(Args).

handle_call(_Request, _From, State) ->
    io:format("_Request = ~p, _From = ~p, State = ~p~n", [_Request, _From, State]),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    io:format("_Msg = ~p, State = ~p~n", [_Msg, State]),
    {noreply, State}.

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


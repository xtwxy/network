-module(name_server).
-export([start/0, stop/0, lookup/1, put_entry/2, remove_entry/1, rpc/1, loop/1]).

start() ->
	register(name_server, spawn(?MODULE, loop, [dict:new()])).

stop() ->
	rpc(stop).

lookup(Key) ->
	rpc({lookup, Key}).

put_entry(Key, Value) ->
	rpc({put_entry, Key, Value}).

remove_entry(Key) ->
	rpc({remove_entry, Key}).

rpc(Request) ->
	name_server ! {self(), Request},
	receive
		{name_server, Response} ->
			Response
	end.

loop(Dict) ->
	receive
		{From, {lookup, Key}} ->
			From ! {name_server, dict:find(Key, Dict)},
			loop(Dict);
		
		{From, {put_entry, Key, Value}} ->
			From ! {name_server, ok},
			loop(dict:store(Key, Value, Dict));

		{From, {remove_entry, Key}} ->
			From ! {name_server, ok},
			loop(dict:erase(Key, Dict));

		{From, Other} ->
			From ! {name_server, {error, Other}},
			loop(Dict)
	end.





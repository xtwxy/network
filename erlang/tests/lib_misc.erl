-module(lib_misc).
-export([on_exit/2, start_test/0]).

on_exit(Pid, Fun) ->
	spawn(fun() ->
			      Ref = monitor(process, Pid),
			      receive
				      {'DOWN', Ref, process, Pid, Why} ->
					      Fun(Why)
			      end
	      end).

start_test() ->
	Pid = spawn(fun() -> 
				    receive
					    X -> list_to_atom(X)
				    end
		    end),
	on_exit(Pid,
		fun(Why) ->
				io:format(" ~p died with: ~p~n", [Pid, Why])
		end),
	Pid ! hello.


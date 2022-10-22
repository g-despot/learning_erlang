-module(iter).
-export([ford/1, foru/1, sum/1]).

ford(1) -> io:format("~p~n", [1]);
ford(N) -> 
	io:format("~p~n", [N]),
	ford(N-1).
	
foru(N) ->
	foru_internal(N, 1).

foru_internal(N, M) when N > M ->
	io:format("~p~n", [M]),
	foru_internal(N, M+1);
foru_internal(N, M) when N =:= M ->
	io:format("~p~n", [M]).
	
sum(N) ->
	sum_internal(N, 0).

sum_internal(N, Sum) when N =:= 0 ->
	io:format("~p~n", [Sum]);	
sum_internal(N, Sum) when N > 0 ->
	sum_internal(N-1, Sum+N).

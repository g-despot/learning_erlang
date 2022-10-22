-module(lists2).
-export([append/2, reverse/1, flatten/1]).

append(L1, L2) ->
	append_internal(L1, L2).

append_internal(L1, []) ->
	L1;
append_internal(L1, [L2_head|L2_tail]) ->
	append_internal(L1 ++ [L2_head], L2_tail).
	
reverse(L) ->
	reverse_internal(L, []).

reverse_internal([], R) ->
	R;
reverse_internal([H|T], R) ->
	reverse_internal(T, [H] ++ R).

flatten(L) ->
	flatten_internal(L, []).

flatten_internal([], F) ->
	F;
flatten_internal([H|T], F) when not is_list(H)->
	flatten_internal(T, F ++ [H]);
flatten_internal([H|_], F) when is_list(H)->
	flatten_internal(H, F).

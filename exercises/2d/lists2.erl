-module(lists2).
-export([append/2, reverse/1, flatten/1]).

append(L1, L2) ->
    L1 ++ L2.

reverse(L) ->
    reverse_internal(L, []).

reverse_internal([], R) ->
    R;
reverse_internal([H | T], R) ->
    reverse_internal(T, [H | R]).

flatten(L) ->
    do_flatten(L, []).

do_flatten([H | T], Tail) when is_list(H) ->
    do_flatten(H, do_flatten(T, Tail));
do_flatten([H | T], Tail) ->
    [H | do_flatten(T, Tail)];
do_flatten([], Tail) ->
    Tail.

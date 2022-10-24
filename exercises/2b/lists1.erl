-module(lists1).
-export([nth/2, sublist/2, seq/2]).

nth(N, L) ->
    nth_internal(N, L, 1).

nth_internal(N, [H | _], N) ->
    H;
nth_internal(N, [_ | T], Count) when N > Count ->
    nth_internal(N, T, Count + 1).

sublist(L, N) ->
    sublist_internal(L, N, 1, []).

sublist_internal([H | _], N, N, SubList) ->
    SubList ++ [H];
sublist_internal([H | T], N, Count, SubList) when N > Count ->
    sublist_internal(T, N, Count + 1, SubList ++ [H]).

seq(Low, High) ->
    seq_internal(Low, High, []).

seq_internal(Low, High, List) when Low =:= High ->
    List ++ [Low];
seq_internal(Low, High, List) when Low < High ->
    seq_internal(Low + 1, High, List ++ [Low]).

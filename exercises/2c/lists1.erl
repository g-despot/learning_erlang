-module(lists1).
-export([insert/2, insertion_sort/1]).

insert(X, []) -> [X];
insert(X, SortedList = [H | _]) when X =< H -> [X | SortedList];
insert(X, [H | T]) -> [H | insert(X, T)].

insertion_sort(L) -> lists:foldl(fun insert/2, [], L).

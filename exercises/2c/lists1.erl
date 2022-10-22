-module(lists1).
-export([insert/2]).

insert(X, SortedList) ->
	insert_internal(X, SortedList, []).

insert_internal(X, [], _) ->
	[X];
insert_internal(X, [H|T], TmpList) ->
	if
		H >= X ->
			TmpList ++ [X] ++ [H|T];
		H < X ->
			insert_internal(X, T, TmpList ++ [H])
	end.

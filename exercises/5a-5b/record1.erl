-module(record1).
-export([create_index/1]).

-record(person, {name, birth, index}).

%% TODO: Fix this module. It should be much simpler.
create_index(FileName) ->
    %% This list is sorted by the date of birth.
    Records = list_to_record(lists:keysort(2, element(2, file:consult(FileName))), [], 0),
    IndexedList = sort_list(Records, [], no),
    present(lists:keysort(#person.index, IndexedList)).

list_to_record([H | T], PeopleRecords, Index) ->
    list_to_record(
        T,
        PeopleRecords ++ [#person{name = element(1, H), birth = element(2, H), index = Index}],
        Index + 1
    );
list_to_record([], PeopleRecords, _) ->
    PeopleRecords.

sort_list([H1 | [H2 | T]], IndexL, Updated) when length(T) > 0 ->
    if
        (H1#person.birth =:= H2#person.birth) andalso (H1#person.name > H2#person.name) ->
            Person1 = H1#person{index = H2#person.index},
            Person2 = H2#person{index = H1#person.index},
            sort_list(T, IndexL ++ [Person2] ++ [Person1], yes);
        true ->
            sort_list([H2 | T], IndexL ++ [H1], Updated)
    end;
sort_list([H1 | [H2 | []]], IndexL, Updated) ->
    if
        (H1#person.birth =:= H2#person.birth) andalso (H1#person.name > H2#person.name) ->
            Person1 = H1#person{index = H2#person.index},
            Person2 = H2#person{index = H1#person.index},
            sort_list(IndexL ++ [Person1] ++ [Person2], [], no);
        Updated =:= no ->
            IndexL ++ [H1] ++ [H2];
        Updated =:= yes ->
            sort_list(IndexL ++ [H1] ++ [H2], [], no)
    end.

present(FileName) ->
    lists:foreach(
        fun(X) ->
            {Y, M, D} = X#person.birth,
            io:format("Name: ~s, Birth: ~p.~p.~p.~n", [X#person.name, D, M, Y])
        end,
        FileName
    ).

-module(maps3).
-export([person/3, print_oldest/2]).

person(Name, Age, Address) ->
    #{"Name" => Name, "Age" => Age, "Address" => Address}.

print_oldest(
    #{"Name" := Name1, "Age" := Age1, "Address" := Address1} = Person1,
    #{"Name" := Name2, "Age" := Age2, "Address" := Address2} = Person2
) ->
    if
        Age1 > Age2 ->
            io:format("~s~n~s~n", [Name1, Address1]);
        Age1 < Age2 ->
            io:format("~s~n~s~n", [Name2, Address2]);
        Age1 =:= Age2 ->
            io:format("They are the same age and are ~p years old~n.", [Age1])
    end.

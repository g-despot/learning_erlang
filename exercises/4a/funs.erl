-module(funs).
-export([llen/1]).

llen(ListOfLists) -> [length(X) || X <- ListOfLists].

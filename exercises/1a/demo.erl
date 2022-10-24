-module(demo).
-export([double/1]).

times(X, N) ->
    X * N.
double(X) ->
    times(X, 2).

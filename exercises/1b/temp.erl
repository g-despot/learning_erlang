-module(temp).
-export([f2c/1, c2f/1]).

f2c(Temperature) -> 
	5 / 9 * (Temperature - 32).
c2f(Temperature) -> 
	9 / 5 * Temperature + 32.

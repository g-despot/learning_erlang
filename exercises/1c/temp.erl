-module(temp).
-export([convert/2]).

convert(f, Temperature) -> 
	{c, 5 / 9 * (Temperature - 32)};
convert(c, Temperature) -> 
	{f, 9 / 5 * Temperature + 32}.

-module(temp).
-export([convert/1]).

convert({f, Temperature}) when
    (is_integer(Temperature) orelse is_float(Temperature)) andalso Temperature >= -459.67
->
    {c, 5 / 9 * (Temperature - 32)};
convert({c, Temperature}) when
    (is_integer(Temperature) orelse is_float(Temperature)) andalso Temperature >= -273.15
->
    {f, 9 / 5 * Temperature + 32}.

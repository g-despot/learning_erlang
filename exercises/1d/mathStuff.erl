-module(mathStuff).
-export([perimeter/1]).

perimeter({square, Side}) -> 
	math:pow(Side, 2);
perimeter({circle, Radius}) -> 
	math:pi() * 2 * Radius;
perimeter({triangle, A , B, C}) -> 
	A + B + C.

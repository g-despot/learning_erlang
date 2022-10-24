-module(mathStuff).
-export([perimeter/1]).

perimeter({square, Side}) when (is_integer(Side) orelse is_float(Side)) andalso Side > 0 ->
    math:pow(Side, 2);
perimeter({circle, Radius}) when (is_integer(Radius) orelse is_float(Radius)) andalso Radius > 0 ->
    math:pi() * 2 * Radius;
perimeter({triangle, A, B, C}) when
    ((is_integer(A) orelse is_float(A)) andalso A > 0) andalso
        ((is_integer(B) orelse is_float(B)) andalso B > 0) andalso
        ((is_integer(C) orelse is_float(C)) andalso C > 0)
->
    A + B + C.

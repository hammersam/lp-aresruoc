(* I don't know what I am doing : / *)

val x = 34;

val y = ~18;

val z = (x - y - 100);

val abs_of_z = if z > 0 then z else 0 - z;

val abs_of_y = abs y;

(* syntax, type checking rules and evalution rules for less-than comparisons *)
(* less-than comparisons: val x = if e1 < e2 then e3 else e4; *)
(*
Syntax:
  if e1 < e2 is true then e3
  else e4 where if, then, and else are keywords and
  e1, e2, el < e2, e3, e4 are subexpressions

Tyep-checking:
  e1 and e2 can have any type (let's call it t1), but they
  must have the same type t1
  e3 and e4 can have any type (let's call it t2), but they
  must have the same type t2
  the type of the entire expression is also t2

Evalutaion rules:
  first evalute e1 to a value call it v1, evalute e2 to a value call it v2
  if v1 < v2 is true, evalute e3 and that result is the whole expression's
  result
  else, evalute e4 and that result is the whole expression's result
*)

val have_fun = if "hello" < "world" then x else y;

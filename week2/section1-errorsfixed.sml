(* Dan Grossman, CSE341, Programming Languages *)
(* Section 1: Some Errors *)

(* This program has several errors in it so we can try to debug them. *)

val x = 34

val y = x + 1

val z = if y > 0 then false else x < 4

val q = if y > 0 then 0 else 42

val a = ~5

val w = 0

val funny = 34

val v = x div (w + 1)

val fourteen = 7 + 7

(* Can't have val bindings start with numbers *)
val cse341 = true

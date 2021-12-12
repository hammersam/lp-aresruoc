(* Dan Grossman, CSE341, Programming Languages *)
(* Section 1: Some Errors *)

(* This program has several errors in it so we can try to debug them. *)

val x = 34

val y = x + 1

val z = if y > 0 then 34 else x - 4

val q = if y > 0 then 0 else 1

val a = ~5

val w = 0

fun havefun() = 34

val v = if w = 0 then x else x div w (* Does it Work? *)

val fourteen = 7 + 7

val cse341 = true

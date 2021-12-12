(* Programming Languages, Dan Grossman *)
(* Section 3: Lexical Scope *)

(* 1 *) val x = 1

(* whenever we call this function we going to evalute the
function body in the environment where the function is defined *)
(* 2 *) fun f y = x + y

(* 3 *) val x = 2

(* 4 *) val y = 3

(* 5 *) val z = f (x + y)


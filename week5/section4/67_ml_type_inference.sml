(* Programming Languages, Dan Grossman *)
(* Section 4: ML Type Inference *)

val x = 42 (* val x : int *)

fun f (y, z, w) =
    if y (* y must be bool *)
    then z + x (* z must be int *)
    else 0 (* both branches have same type *)
(* f must return an int
   f must take a bool * int * ANYTHING
   so val f : bool * int * 'a -> int 
 *)

(*
Central feature for ML type inference:
it can infer types with type variables
- Great for code reuse and understanding functions

- Languages can have type inference without type variables
- Languages can hava type variables without type inference
*)

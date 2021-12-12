(* Programming Languages, Dan Grossman *)
(* Section 1: Pairs and Tuples *)

(* pairs *)

fun swap (pr : int*bool) =
  (#2 pr, #1 pr)

fun sum_two_pairs (pr1 : int*int, pr2 : int*int) =
  (#1 pr1) + (#2 pr1) + (#1 pr2) + (#2 pr2)

(* returning a pair a real pain in Java *)
fun div_mod (x : int, y : int) = 
  (x div y, x mod y)

fun sort_pair (pr : int*int) =
  if (#1 pr) < (#2 pr)
  then pr
  else (#2 pr, #1 pr) 

  (* nested pairs *)

fun max_of_pair(pr : int * int) =
  if (#1 pr) < (#2 pr)
  then (#2 pr)
  else (#1 pr)

fun have_fun_with_tuples(tup : int * int * int) =
  max_of_pair(max_of_pair(#1 tup, #2 tup), #3 tup)

val x1 = (7,(true,9)) (* int * (bool*int) *)

val x2 = #1 (#2 x1)  (* bool *)

val x3 = (#2 x1)      (* bool*int *)

val x4 = ((3,5),((4,8),(0,0))) (* (int * int) * ((int * int) * (int * int)) *)

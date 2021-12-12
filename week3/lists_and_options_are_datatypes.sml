(*
Why pattern-matching is better for options and lists?
- No missing case, no exceptions for wrong variant...
*)
datatype my_int_list = Empty
                     | Cons of int * my_int_list

datatype my_option = My_None
                   | My_Some of int

fun append_my_list (xs, ys) =
  case xs of
       Empty => ys
     | Cons (x, xs') => Cons (x, append_my_list (xs', ys))

val xs = Cons (4, Cons (23, Cons (2008, Empty)))
val ys = Cons (1, Cons (10, Empty))

val lis = append_my_list (xs, ys)

(* better coding style for option *)
fun inc_or_zero intoption =
  case intoption of
       NONE => 0
     | SOME i => i + 1

val test1 = (inc_or_zero (SOME 2)) = 3

fun null xs =
  case xs of 
       [] => true
     | x :: xs' => false

val nulltest1 = (null []) = true
val nulltest2 = (null [1,2]) = false

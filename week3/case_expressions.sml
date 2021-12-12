(* 
values(like 'twoints (42, 10)') of self-defined type like mytype below both
have the tag that corresponds to the constructor
as well as the value that's sort of underneath that tag.
If there is a constructor that doesn't carry any data like 'Pizza' below
then the constructor will not be a function but a value of type 'mytype' instead.
*)
datatype mytype = TwoInts of int * int
                | Str of string
                | Pizza

datatype suit = Club
              | Diamond
              | Heart
              | Spade

datatype rank = Jack
              | Queen
              | King
              | Ace
              | Num of int

datatype id = StudentNum of int
            | Name of string
                      * (string option)
                      * string

datatype exp = Constant of int
             | Negate of exp
             | Add of exp * exp
             | Multiply of exp * exp
fun f x =
    case x of
         Pizza => 3
       | Str s => 4
       | TwoInts(i1, i2) => i1 + i2

fun eval exp =
    case exp of
         Constant x => x
       | Negate exp => ~ (eval exp)
       | Add (exp1, exp2) => (eval exp1) + (eval exp2)
       | Multiply (exp1, exp2) => (eval exp1) * (eval exp2)

fun number_of_adds e = 
    case e of
         Constant x => 0
       | Negate e => number_of_adds e
       | Add (e1, e2) => 1 + (number_of_adds e1) + (number_of_adds e2)
       | Multiply (e1, e2) => (number_of_adds e1) + (number_of_adds e2)

fun max_constant e =
    case e of
         Constant i => i
       | Negate e => max_constant e
       | Add (e1, e2) => Int.max(max_constant e1, max_constant e2)
       | Multiply (e1, e2) => Int.max (max_constant e1, max_constant e2)

val example_e = Multiply (Add ((Constant 10), (Negate (Constant 20))),
                          (Constant 15))

val test1 = ((eval (Add (Constant (10 + 9), Negate (Constant 3)))) = 16)

val test2 = (number_of_adds example_e) = 1

val test3 = (max_constant example_e) = 20

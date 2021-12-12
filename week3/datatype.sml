(*
TwoInts, Str, Pizza are constructors.
A constructor is a function that makes
values of the new type(make a new value of type 'mytype' in this case)
- TwoInts : int * int -> mytype
- Str : string -> mytype
- Pizza : mytype(Pizza is not a function because it doesn't carry anything,
it already is a value of type 'mytype')
Constructors are also tagged unions that say what type of 'mytype' we have.
val a = Str "hi" : mytype
*)
datatype mytype = TwoInts of int * int
                | Str of string
                | Pizza

val a = Str "hi"
val b = Str
val c = Pizza
val d = TwoInts (42, 13)
val e = a

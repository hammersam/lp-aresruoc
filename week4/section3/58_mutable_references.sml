(* Programming Languages, Dan Grossman *)
(* Section 3: Mutable References *)

val x = ref 42 

val y = ref 42 

val z = x

val _ = x := 43

val w = (!y) + (!z) (* 85 *)

(* x + 1 does not type-check *)
val h = 42 (* define a variable h *)

val i = ref h (* define a reference i and i has nothing to do with h*)

val _ = i := 43 (* update the reference i to see if it changes the value of h? -- NO*)

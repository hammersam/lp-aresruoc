fun append (xs, ys) =
  case xs of
       [] => ys
     | x :: xs' => x :: append (xs', ys)

(*
 * polyEqual: 
 * val same_thing = fn : ''a * ''a -> string
 *)
fun same_thing (x, y) =
  if x = y then "yes" else "no"

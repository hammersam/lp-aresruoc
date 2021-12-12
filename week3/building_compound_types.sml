fun append (xs, ys) =
  if null xs
  then ys
  else (hd xs) :: append (tl xs, ys)

fun map (f, xs) =
  case xs of
       [] => []
     | x :: xs' => (f x) :: (map (f, xs'))

(* List use all three building blocks (each-of, one-of, self-reference)
 * - int list contains an int and another int list or
 * it contains no data
 *)

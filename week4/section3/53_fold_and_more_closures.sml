(* Programming Languages, Dan Grossman *)
(* Section 3: Fold and More Closures *)

(* Another hall-of-fame higher-order function *)

(* note this is "fold left" if order matters 
   can also do "fold right" *)

(* f(f(f(f(acc, x1), x2), x3), x4) --> fold left *)
fun fold (f,acc,xs) =
    case xs of 
    [] => acc
      | x::xs' => fold (f,f(acc,x),xs')
(* f(x1, f(x2, f(x3, f(x4, acc)))) --> fold right *)
fun fold_right (f, acc, xs) =
    case xs of
    [] => acc
      | x :: xs' => f (fold_right (f, acc, xs'), x)

fun pow (x, y) =
    if y = 0
    then 1
    else x * pow (x, y - 1)

val fold_left_test = fold (fn (acc, x) => pow (x, acc), 2, [1,2,3])
val fold_right_test = fold_right (fn (acc, x) => pow (x, acc), 2, [1,2,3])

(* examples not using private data *)

fun f1 xs = fold ((fn (x,y) => x+y), 0, xs)

fun f2 xs = fold ((fn (x,y) => x andalso y >= 0), true, xs)

(* examples using private data *)

fun f3 (xs,lo,hi) = 
    fold ((fn (x,y) => 
          x + (if y >= lo andalso y <= hi then 1 else 0)),
          0, xs)

fun f4 (xs,s) =
    let 
    val i = String.size s
    in
    fold((fn (x,y) => x andalso String.size y < i), true, xs)
    end

fun f5 (g,xs) = fold((fn(x,y) => x andalso g y), true, xs)

fun f4again (xs,s) =
    let
    val i = String.size s
    in
    f5(fn y => String.size y < i, xs)
    end


datatype exp = Constant of int
             | Negate of exp
             | Add of exp * exp
             | Multi of exp * exp

fun eval (Constant i) extra = i + extra
  | eval (Negate e) extra = ~ (eval e extra) + extra
  | eval (Add (e1, e2)) extra = (eval e1 extra) + (eval e2 extra) + extra
  | eval (Multi (e1, e2)) extra = (eval e1 extra) * (eval e2 extra) + extra

fun append ([], ys) = ys
  | append (x :: xs', ys) = x :: append (xs', ys)

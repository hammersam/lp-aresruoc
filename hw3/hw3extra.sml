(* 1. *)
fun compose_opt f g x =
    case g x of
        SOME b => f b
      | _ => NONE

(* 2. *)
fun do_until f p x =
    if p x
    then do_until f p (f x)
    else x

(* 3. *)
fun factorial x =
    let
        val (ans, _) = do_until (fn (acc, x) => (acc * x, x - 1)) (fn (_, x) => x > 0) (1, x)
    in
        ans
    end

(* 4. *)
fun fixed_point f x =
    do_until f (fn x => f x <> x) x

(* 5. *)
fun map2 f (x, y) =
    (f x, f y)

(* 6. *)
fun app_all f g x =
    let fun aux xs =
            case xs of
                [] => []
              | x :: xs' => f x @ aux xs'
    in
        aux (g x)
    end

(* 7. *)
fun foldr f init xs =
    case xs of
        [] => init
      | x :: xs' => f (x, (foldr f init xs'))

(* 8. *)
fun partition p xs =
    case xs of
        [] => ([], [])
      | x :: xs' =>
        let val (tl1, tl2) = partition p xs'
        in
            if p x then (x :: tl1, tl2)
            else (tl1, x :: tl2)
        end

val part_test1 = partition (fn x => x mod 2 = 0) [1,2,3,4,5]

(* 9. *)
fun unfold f s =
    case f s of
        SOME (v, new_s) => v :: unfold f new_s
      | NONE => []

val unfold_test1 = unfold (fn n => if n = 0 then NONE else SOME (n, n - 1)) 5

(* yet another factorial implementation *)
(* 10. *)
fun unfold_factorial n =
    foldr (fn (x, acc) => x * acc) 1 (unfold (fn n => if n = 0 then NONE else SOME (n, n - 1)) n)

val unfold_factorial_test1 = unfold_factorial 10

(* 11. *)
fun map f xs =
    foldr (fn (x, acc) => f x :: acc) [] xs

(* 12. *)
fun filter p xs =
    foldr (fn (x, acc) => if p x then x :: acc else acc) [] xs

(* 13. *)
(* fun *)

(* 14. *)
datatype 'a tree = Node of 'a tree * 'a * 'a tree | Leaf

fun tree_map f tr =
    case tr of
        Node (lt, v, rt) => Node (tree_map f lt, f v, tree_map f rt)
      | Leaf => Leaf

fun tree_filter f tr =
    case tr of
        Node (lt, v, rt) =>
        if f v
        then Node (tree_filter f lt, v, tree_filter f rt)
        else Leaf
      | Leaf => Leaf

fun tree_fold f acc tr =
    case tr of
        Node (lt, v, rt) => f (v, tree_fold f acc lt, tree_fold f acc rt)
      | Leaf => acc

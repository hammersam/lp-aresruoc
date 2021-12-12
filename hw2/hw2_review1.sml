(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option (s, sl) =
    let fun aux (s, sl) =
        case sl of
            [] => []
          | x :: xs => if same_string (x, s) then aux (s, xs) else x :: aux (s, xs)
        val l = aux (s, sl)
    in
        if length l = length sl then NONE else SOME l
    end

fun get_sub_help (s, sl) =
    case all_except_option (s, sl) of
         SOME l => l
       | NONE => []

fun get_substitutions1 (sll, s) =
    case sll of
        [] => []
      | x :: xs => get_sub_help (s, x) @ get_substitutions1 (xs, s)

fun get_substitutions2 (sll, s) =
    let fun aux (sll, s, acc) =
          case sll of
              [] => acc
            | x :: xs => aux (xs, s, acc @ get_sub_help (s, x))
    in
        aux (sll, s, [])
    end

fun similar_names (subs, {first = a, middle = b, last = c}) =
    let fun aux sl =
            case sl of
                [] => []
              | x :: xs => {first = x, middle = b, last = c} :: aux xs
    in
        aux (a :: get_substitutions2 (subs, a))
    end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color c =
    case c of
        (Clubs, _) => Black
      | (Diamonds, _) => Red
      | (Hearts, _) => Red
      | (Spades, _) => Black

fun card_value c =
    case c of
        (_, Ace) => 11
      | (_, Num n) => n
      | _ => 10

fun remove_card (cs, c, e) =
    let fun aux (cs, c) =
        case cs of
            [] => []
          | x :: xs => if x = c then xs else x :: aux (xs, c)
        val l = aux (cs, c)
    in
        if length l = length cs then raise IllegalMove else l
    end

fun all_same_color cs =
    case cs of
        c1 :: c2 :: xs => card_color c1 = card_color c2 andalso
                          all_same_color (c2 :: xs)
      | _ => true

fun sum_cards cs =
    let fun aux (cs, acc) =
        case cs of
            [] => acc
          | x :: xs => aux (xs, acc + card_value x)
    in
        aux (cs, 0)
    end   

fun score (held, goal) =
    let val sum = sum_cards held
        val prelim = if sum > goal then 3 * (sum - goal) else goal - sum
    in
        if all_same_color held then prelim div 2 else prelim
    end

fun officiate (cl, ml, goal) =
    let fun aux (cl, ml, held) =
        case ml of
            [] => score (held, goal)
          | x :: xs => case x of
                           Discard c => aux (cl, xs, 
                           remove_card (held, c, IllegalMove))
                         | Draw => case cl of
                                       [] => score (held, goal)
                                     | y :: ys => if sum_cards (y :: held) > goal
                                                  then score (y :: held, goal)
                                                  else aux (ys, xs, y :: held)
    in
        aux (cl, ml, [])
    end

(*
fun officiate (cl, ml, goal) =
    let fun make_draw (cl, held, rest_ms, f) =
            case cl of
                [] => score (held, goal)
              | x :: xs => if sum_cards (x :: held) > goal
                           then score (x :: held, goal)
                           else f (xs, rest_ms, x :: held, f)
        fun make_move (m, cl, held, rest_ms, f) =
            case m of
                Discard c => f (cl, rest_ms, 
                remove_card (held, c, IllegalMove), f)
                handle IllegalMove => f (cl, rest_ms, held, f)
              | Draw => make_draw (cl, held, rest_ms, f)
        fun make_moves (cl, ml, held, f) =
            case ml of
                [] => score (held, goal)
              | x :: xs => make_move (x, cl, held, xs, make_moves)
    in
        make_moves (cl, ml, [], make_moves)
    end *)


(* Problem 1 *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* Problem 1-(a) *)
fun all_except_option (str : string, sl : string list) =
    case sl of
        [] => NONE
      | hd :: tl =>
        if same_string (hd, str)
        then SOME tl
        else case all_except_option (str, tl) of
                 SOME tl => SOME (hd :: tl)
               | _ => NONE

(* Problem 1-(b) *)
fun get_substitutions1 (sll : string list list, str : string) =
    case sll of
        [] => []
      | hdl :: tll =>
        case all_except_option (str, hdl) of
            SOME sl => sl @ get_substitutions1 (tll, str)
          | NONE => get_substitutions1 (tll, str)

(* Problem 1-(c) *)
fun get_substitutions2 (sll : string list list, str : string) =
    let fun aux (sll : string list list, rtl : string list) =
            case sll of
                [] => rtl
              | hdl :: tll =>
                case all_except_option (str, hdl) of
                    SOME sl => aux (tll, rtl @ sl)
                  | NONE => aux (tll, rtl)
    in
        aux (sll, [])
    end

(* Problem 1-(d) *)
type full_name = { first : string, middle : string, last : string }

fun similar_names (sll : string list list, fname : full_name) =
    (* define a local function to produce the full name list of substitutions list *)
    let val { first = f, last = l, middle = m } = fname
        fun aux (sl : string list) =
            case sl of
                [] => []
              | hd :: tl =>
                { first = hd, last = l, middle = m } :: aux tl
    in
        fname :: aux (get_substitutions2 (sll, f))
    end

(* Problem 2 *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
datatype color = Red | Black
type card = suit * rank
datatype move = Discard of card | Draw
exception IllegalMove

(* Problem 2-(a) *)
fun card_color (c : card) =
    case c of
        (Diamonds, _) => Red
      | (Hearts, _) => Red
      | (Clubs, _) => Black
      | (Spades, _) => Black

(* Problem 2-(b) *)
fun card_value (c : card) =
    case c of
        (_, Num i) => i
      | (_, Ace) => 11
      | (_, Jack) => 10
      | (_, Queen) => 10
      | (_, King) => 10

(* Problem 2-(c) *)
fun remove_card (cs : card list, c : card, e : exn) =
    case cs of
        [] => raise e
      | hd :: tl =>
        if hd = c
        then tl
        else hd :: remove_card (tl, c, e)

(* Problem 2-(d) *)
fun all_same_color (cs : card list) =
    case cs of
        [] => false
      | c :: [] => true
      | cx :: cy :: tl =>
        if card_color cx = card_color cy
        then all_same_color (cy :: tl)
        else false

(* Problem 2-(e) *)
fun sum_cards (cs : card list) =
    let fun aux (cs : card list, sum : int) =
            case cs of
                [] => sum
              | c :: tl => aux (tl, card_value c + sum)
    in
        aux (cs, 0)
    end

(* Problem 2-(f) *)
fun score (cs : card list, goal : int) =
    let val sum = sum_cards cs
        val pre_score = if sum > goal
                        then 3 * (sum - goal)
                        else goal - sum
    in
        if all_same_color cs
        then pre_score div 2
        else pre_score
    end

(* Problem 2-(g) *)
fun officiate (cs : card list, ms : move list, goal : int) =
    let fun run (cs : card list, hs : card list, ms : move list) =
            case (cs, hs, ms) of
                ([], _, _) => score (hs, goal)
              | (_, _, []) => score (hs, goal)
              | (c :: cs', hs, m :: ms') =>
                if sum_cards hs > goal
                then score (hs, goal)
                else case m of
                         Draw => run (cs', c :: hs, ms')
                       | Discard dc =>
                         run (cs, remove_card (hs, dc, IllegalMove), ms')
    in
        run (cs, [], ms)
    end

(* Problem 3-(a) *)
fun count_ace (cs : card list) =
    case cs of
        [] => 0
      | (_, r) :: cs' =>
        if r = Ace then 1 + count_ace cs'
        else count_ace cs'

fun get_sum_list (sum : int, count : int) =
    if count = 0
    then [sum]
    else (sum - count * 10) :: get_sum_list (sum, count - 1)

fun map (f, xs) =
    case xs of
        [] => []
      | x :: xs' => f x :: map (f, xs')

fun get_min (xs : int list) =
    case xs of
        [] => raise List.Empty
      | x :: [] => x
      | x :: y :: tl =>
        let val tl_min = get_min (y :: tl)
        in
            if x < tl_min
            then x
            else tl_min
        end

fun score_challenge (cs : card list, goal : int) =
    let fun calculate (sum : int) =
            let val pre_score = if sum > goal
                                then 3 * (sum - goal)
                                else goal - sum
            in
                if all_same_color cs
                then pre_score div 2
                else pre_score
            end
    in
        get_min (map(calculate, get_sum_list (sum_cards cs, count_ace cs)))
    end

fun officiate_challenge (cs : card list, ms : move list, goal : int) =
    let fun run (cs : card list, hs : card list, ms : move list) =
            case (cs, hs, ms) of
                ([], _, _) => score_challenge (hs, goal)
              | (_, _, []) => score_challenge (hs, goal)
              | (c :: cs', hs, m :: ms') =>
                if get_min (get_sum_list (sum_cards hs, count_ace hs)) > goal
                then score_challenge (hs, goal)
                else case m of
                         Draw => run (cs', c :: hs, ms')
                       | Discard dc =>
                         run (cs, remove_card (hs, dc, IllegalMove), ms')
    in
        run (cs, [], ms)
    end

(* Problem 3-(b) *)
(* assume the biggest value of cards is 11 *)
fun careful_player (cs : card list, goal : int) =
    let fun discard_or_not (draw_card : card, hs : card list) =
            let fun aux (cs : card list) =
                    case cs of
                        [] => NONE
                      | c :: cs' =>
                        if score (draw_card :: remove_card (hs, c, IllegalMove), goal) = 0
                        then SOME c
                        else aux cs'
            in
                aux hs
            end
        fun run (cs : card list, hs : card list, ms : move list) =
            if score (hs, goal) = 0 then ms
            else if (goal - sum_cards hs) > 10
            then case cs of
                     [] => ms
                   | c :: cs' => run (cs', c :: hs, ms @ [Draw])
            else case cs of
                     [] => ms
                   | c :: cs' =>
                     case discard_or_not (c, hs) of
                         SOME discard_card => run (cs', c :: remove_card (hs, discard_card, IllegalMove), ms @ [Discard discard_card, Draw])
                       | NONE => ms
    in
        run (cs, [], [])
    end

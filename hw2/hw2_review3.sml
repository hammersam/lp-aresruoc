(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(*a*)
(*fun all_except_option(s : string , sl :string list)=
    let
        fun helper (s :string , sl :string list , rl :string list) =

    in
        helper(s,sl,[])
    end*)

fun all_except_option(s, []) = NONE
  | all_except_option(s, head::tail) =
    case (same_string(s,head), all_except_option(s, tail))
       of (true, _) => SOME(tail)
        | (false, NONE) => NONE
        | (false, SOME(y)) => SOME(head::y)

(*b*)

fun get_substitutions1([], _) =
    []
  | get_substitutions1(head::tail, s) =
    case all_except_option(s, head)
       of NONE => get_substitutions1(tail, s)
        | SOME(y) => y @ get_substitutions1(tail, s)


fun get_substitutions2([], _) = []
  | get_substitutions2(sl, s) =
    let
        fun helper([], h) = h
          | helper(head::tail, h) =
            case all_except_option(s, head)
               of NONE => helper(tail, h)
                | SOME(y) => helper(tail, h @ y)
    in
        helper(sl, [])
    end




(*--------------------------*)

(*c*)

type fullName = {first:string , last:string,middle:string};

fun get_valid_full_names (validNames :string  list ,{first,last,middle}:fullName) =
    case validNames of
        [] => []
       | hd::tl => {first=hd,last=last,middle=middle} :: get_valid_full_names(tl,{first=first,last=last,middle=middle})


fun similar_names(sl :string list list,{first,last,middle}:fullName)=
    {first=first,last=last,middle=middle} :: get_valid_full_names(get_substitutions1(sl,first),{first=first,last=last,middle=middle})




(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades

datatype rank = Jack | Queen | King | Ace | Num of int 

type card = suit * rank

datatype color = Red | Black

datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

(*a*)

fun card_color(mcard :card)=
    if #1 mcard = Clubs orelse #1 mcard = Spades
    then Black else Red

(*b*)
fun card_value (mcard :card) =
    case (#2 mcard) of
        Ace => 11
       |Num(n) => n
       | _ => 10

(*c*)
fun remove_card (cl :card list ,c :card,e :exn) =
    let
        fun removeH(cl :card list ,c :card,prev :card list) =
            case cl of
                [] => raise e
               | hd::tl => if hd = c
                           then prev @ tl
                           else removeH(tl,c,prev @ [hd])
    in
        removeH(cl,c,[])
    end

(*d*)
fun all_same_color(cl :card list)=
    case cl of
        [] => true
       | hd::tl =>
       let
           fun all_like_first(firstColor :color,cl :card list)  =
               case cl of
                   [] => true
                  | hd::tl => if firstColor = card_color(hd)
                              then all_like_first(firstColor,tl)
                              else false
       in
           all_like_first(card_color(hd),tl)
       end



(*e*)


fun sum_cards(cl :card list)=
    let
        fun sumH(cl :card list , sum :int)=
            case  cl of
                [] => sum
               | hd::tl => sumH(tl,card_value(hd)+sum)
    in
        sumH(cl,0)
    end

(*f*)
fun score (cl :card list , goal :int) =
    let
        val sum = sum_cards(cl)
        val allSame = all_same_color(cl)

        fun preliminary_score (cl :card list , goal :int) =
            if sum > goal
            then  (sum - goal) * 3
            else goal - sum

        fun get_end_score (pscore :int) =
            if allSame then pscore div 2 else pscore

    in
        get_end_score(preliminary_score(cl,goal))
    end

(*e*)

fun add_card_to_held_list (c :card , hl :card list)=
    hl @ [c]

fun make_move (cl :card list , ml :move list ,hl :card list, goal :int) =
    case ml of
        [] => hl
       | headd::tl =>
       if sum_cards(hl) < goal
       then case headd of
                Draw =>
              let
                  val first_itme = hd cl

              in
                  make_move(remove_card(cl,first_itme,IllegalMove),tl,add_card_to_held_list(first_itme,hl),goal)
              end
               | Discard(c) =>  make_move(cl,tl,remove_card(hl,c,IllegalMove),goal)
       else hl


fun officiate (cl :card list , ml :move list , goal :int)=
    score(make_move(cl,ml,[],goal),goal)

fun same_string(s1 : string, s2 : string) =
    s1 = s2


fun all_except_option(str, xs) = 
    case xs of 
        [] => NONE
      | x :: xs' => if same_string(str, x) 
                      then SOME xs'
                      else case all_except_option(str, xs') of
                        NONE => NONE
                      | SOME ls => SOME (x :: ls)


fun get_substitutions1(xs, s) = 
    case xs of
        [] => []
      | x :: xs' => case all_except_option(s, x) of
        NONE => get_substitutions1(xs', s)
      | SOME ls => ls @ get_substitutions1(xs', s)


fun get_substitutions2 (xs, s) = 
    let fun aux(xs, s, acc) = 
        case xs of
            [] => acc
          | x :: xs' => case all_except_option(s, x) of
            NONE => aux(xs', s, acc)
          | SOME ls => aux(xs', s, ls @ acc)
    in aux(xs, s, [])
    end


fun similar_names(xs, name) = 
    let 
        val {first=f, middle=m, last=l} = name
        fun make_names xs = 
            case xs of
                [] => []
              | x :: xs' => {first=x, middle=m, last=l} :: (make_names(xs'))
    in 
        name :: make_names(get_substitutions2(xs, f))
    end


datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove


fun card_color(the_suit, the_rank) = 
    case the_suit of
        Spades => Black
      | Clubs => Black
      | Diamonds => Red
      | Hearts => Red


fun card_value(the_suit, the_rank) = 
    case the_rank of
        Jack => 10
      | Queen => 10
      | King => 10
      | Ace => 11
      | Num n => n


fun remove_card(cs, c, e) = 
    case cs of
        [] => raise e
      | x :: cs' => if x = c 
                    then cs'
                    else x :: remove_card(cs', c, e)


fun all_same_color(cs) = 
    case cs of
        [] => true
      | [_] => true
      | c :: cs' :: tail => card_color c = card_color cs' andalso all_same_color(cs' :: tail)


fun sum_cards(cs) = 
    let fun aux(cs, acc) = 
        case cs of 
            [] => acc
          | c :: cs' => aux (cs', acc + card_value c)
    in aux(cs, 0)
    end


  fun score(cs, x) = 
    let 
      val sum = sum_cards cs
      in 
        (if sum >= x then 3 * (sum - x) else x - sum) div (if all_same_color cs then 2 else 1)
      end


fun officiate(cards, moves, goal) = 
  let
    fun aux(held, moves_left, cards) = 
      case moves_left of
        [] => score(held, goal)
      | move :: moves' => case move of 
          Discard n => aux(remove_card(held, n, IllegalMove), moves', cards)
        | Draw => case cards of 
            [] => score(held, goal)
          | card :: cards' => 
              if sum_cards(card :: held) > goal
              then score(card :: held, goal)
              else aux(card :: held, moves', cards')
  in 
    aux([], moves, cards)
  end




                                      





    




                
      
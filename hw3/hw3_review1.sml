(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
                 | Variable of string
                 | UnitP
                 | ConstP of int
                 | TupleP of pattern list
                 | ConstructorP of string * pattern

datatype valu = Const of int
              | Unit
              | Tuple of valu list
              | Constructor of string * valu

fun g f1 f2 p =
    let 
    val r = g f1 f2
    in
    case p of
 Wildcard          => f1 ()
  | Variable x        => f2 x
  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
  | ConstructorP(_,p) => r p
  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
             | UnitT
             | IntT
             | TupleT of typ list
             | Datatype of string

(**** you can put all your code here ****)


fun only_capitals (str_list) =
    List.filter (fn str => Char.isUpper (String.sub (str,0))) str_list

fun longest_string1 (str_list) = 
    List.foldl (fn (str, max_so_far) => if String.size(str) > String.size(max_so_far)
                                        then str
                                        else max_so_far) "" str_list


fun longest_string2 (str_list) = 
    List.foldl (fn (str, max_so_far) => if String.size(str) >= String.size(max_so_far)
                                        then str
                                        else max_so_far) "" str_list

fun longest_string_helper compr str_list = 
    List.foldl (fn (str, max_so_far) => if compr(String.size(str),String.size(max_so_far))
                                        then str
                                        else max_so_far) "" str_list




val longest_string3 = fn str_list => longest_string_helper (fn (a,b) => a > b) str_list

val longest_string4 = fn str_list => longest_string_helper (fn (a,b) => a >= b) str_list


val longest_capitalized = longest_string1 o only_capitals





val rev_string = String.implode o List.rev o String.explode


fun first_answer func a_list = 
    case a_list of
 [] => raise NoAnswer
  |x::xs => case func(x) of
         NONE => first_answer func xs
          |SOME b => b


fun all_answers func a_list =
    case a_list of 
 [] => SOME []
  |x::xs => case func(x) of
         NONE => NONE
          | SOME(lst1) => case all_answers func xs of
                       NONE => NONE
                        |SOME(lst2) => SOME(lst1@lst2)


val count_wildcards = g (fn _ => 1) (fn _ => 0) 

val count_wild_and_variable_lengths = g (fn _ => 1) (String.size) 

fun comp(s) = fn str => if str = s then 1 else 0

fun count_some_var(s, p) = g (fn _ => 0) (comp(s)) p


fun check_pat (p) = 
    let
    fun get_var_names (p) =
        case p of
     Variable x        => [x]
      | TupleP ps         => List.foldl (fn (p,accum) => get_var_names(p) @ accum) [] ps
      | ConstructorP(_,p) => get_var_names(p)
      | _                 => []
    fun has_repeats (str_list) =
        case str_list of 
     x::y::xs => x = y orelse has_repeats(y::xs)
      |_ => false
    in
    not(has_repeats(get_var_names(p)))
    end

fun match (v, p) = 
    case (v, p) of
 (_, Wildcard)       => SOME []
  |(v, Variable(x))  => SOME [(x,v)]
  |(Tuple(vs), TupleP(ps))   =>
        if List.length(ps) = List.length(vs)
        then all_answers (match) (ListPair.zipEq(vs, ps))
        else NONE
  |(Constructor(s1,v),ConstructorP(s2,p))=> if s1 = s2 then match(v,p) else NONE
  |(Unit, UnitP) => SOME []
  |(Const(x), ConstP(y)) => if x = y then SOME [] else NONE
  | _ => NONE


fun first_match v p_list =
    SOME(first_answer(fn p => match(v, p)) p_list) handle NoAnswer => NONE

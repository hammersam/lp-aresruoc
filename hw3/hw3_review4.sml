(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

(**** you can put all your code here ****)

(* problem 1 *)
fun only_capitals xs =
    List.filter (fn s => Char.isUpper(String.sub(s,0))) xs

(* problem 2 *)
fun longest_string1 xs =
    List.foldl (fn (x,y) => if String.size(x) > String.size(y) then x else y) "" xs

(* problem 3 *)
fun longest_string2 xs =
    List.foldl (fn (x,y) => if String.size(x) >= String.size(y) then x else y) "" xs

(* problem 4 *)
fun longest_string_helper f xs =
    List.foldl (fn (x,y) => if f(String.size(x), String.size(y)) then x else y) "" xs

val longest_string3 = longest_string_helper (fn (x,y) => x>y)
val longest_string4 = longest_string_helper (fn (x,y) => x>=y)

(* problem 5 *)
val longest_capitalized = longest_string3 o only_capitals

(* problem 6 *)
val rev_string =
    String.implode o List.rev o String.explode

(* problem 7 , lazy and inefficient version: *)
(*
fun first_answer f xs =
    case (List.filter (fn x => isSome x)) (List.map f xs)  of
  (SOME x)::xs' => x
      | _ => raise NoAnswer
*)

(* problem 7 *)
fun first_answer f xs =
    case xs of
     [] => raise NoAnswer
      | x::xs' => let val cur = f x in
                  if isSome cur then valOf cur
                  else first_answer f xs'
                  end

(* problem 8 *)
fun all_answers f xs =
    let fun aux (lst, acc) =
            case (lst, acc) of
             ([], acc) => acc
              | (NONE::lst', _) => NONE
              | ((SOME l)::lst', (SOME acclst)) => aux(lst', SOME (acclst@l))

    in
    aux((List.map f xs), SOME [])
    end

(* problem 9-11 provided code *)
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

(* problem 9.a *)
fun count_wildcards p =
    let val acc = 0
    in
    g (fn () => acc + 1) (fn x => 0) p
    end

(* problem 9.b *)
fun count_wild_and_variable_lengths p =
    let val acc = 0
    in
    g (fn () => acc +1) (fn x => acc + String.size(x)) p
    end

(* problem 9.c *)
fun count_some_var (s,p) =
    let val acc = 0
    in
    g (fn () => 0) (fn x => if x = s then acc +1 else 0) p
    end


(* problem 10 *)
fun check_pat p =
    let
    val var_names = []
    fun get_variable_names p =
        case p of
         Variable x        => x::var_names
          | TupleP ps         => List.foldl
                             (fn (p, names) => names@get_variable_names(p)) var_names ps
          | ConstructorP(_,p) => get_variable_names(p)
          | _                 => []

    fun check_repeat vs =
        case vs of
         [] => true
          | v::vs' => (not (List.exists (fn s => s = v) vs'))
                      andalso check_repeat(vs')

    in
    (check_repeat o get_variable_names) p
    end

(* problem 11 *)
fun match (v, p) =
    case (p, v) of (* pattern and valu *)
     (Wildcard, _) => SOME []
      | ((Variable s), v') => SOME [(s, v')]
      | (UnitP, Unit) => SOME []
      | (ConstP(i), Const(j)) => if i = j then SOME [] else NONE
      | (TupleP(ps), Tuple(vs)) => if List.length(ps) = List.length(vs) then
                                   (all_answers (fn (x,y) => match(x,y)) (ListPair.zip(vs, ps)))
                                   else NONE
      | (ConstructorP(s1, p'), Constructor(s2, v')) => if s1 = s2 then match(v', p') else NONE
      | (_, _) => NONE 

(* problem 12 *)
fun first_match v ps =
    SOME (first_answer (fn p => match(v, p)) ps)
    handle NoAnswer => NONE

(**** for the challenge problem only ****)
datatype typ = Anything
	           | UnitT
	           | IntT
	           | TupleT of typ list
	           | Datatype of string

                       (* constructor name, datatype name, value of typ *)

			                 (*
fun typecheck_patterns (fs, ps) =
			                 *)

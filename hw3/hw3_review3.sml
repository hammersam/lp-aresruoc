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

fun only_capitals strlst=
    List.filter (fn x => Char.isUpper(String.sub(x,0))) strlst

fun longest_string1 strlst=
    case strlst of
[]=>""
 | s::strlst' =>List.foldl (fn (x,y)=> if String.size(x)>String.size(y) then x else y ) s strlst'

fun longest_string2 strlst=
    case strlst of
[]=>""
 | s::strlst' =>List.foldl (fn (x,y)=> if String.size(x)>=String.size(y) then x else y ) s strlst'

fun longest_string_helper f strlst=
    case strlst of
[] => ""
 | s::strlst' =>
let
val x=String.size(s)
val y=longest_string_helper f strlst'
in

if f(String.size(y),x) then y else s
end

val longest_string3 =longest_string_helper(fn (x,y)=>x>y)
val longest_string4 =longest_string_helper(fn (x,y)=>x>=y)
val longest_capitalized =longest_string1 o only_capitals
val rev_string=String.implode o List.rev o String.explode



fun first_answer f xs=
    case xs of
[] => raise NoAnswer
 | s::xs' => case f(s)of
         NONE => first_answer f xs'
          | SOME x => x

fun all_answers f xs=
    let fun helper1 f acc xss=
            case xss of
        [] => SOME acc  | x::xss' =>case f(x) of
                                NONE => NONE
                                 | SOME s => helper1 f (acc@s) xss'
    in
    helper1 f [] xs

    end



val count_wildcards = g (fn ()=>1)(fn x => 0)
val count_wild_and_variable_lengths=g (fn ()=>1)(fn x => String.size x)
fun count_some_var(x,y)= g (fn () => 0) (fn z=> if z=x then 1 else 0 ) y





fun check_pat pat=
    let fun extract_variables p=
            case p of
        Variable x        => [x]
         | TupleP ps         => List.foldl (fn (p,a) => a@extract_variables p ) [] ps
         | ConstructorP(_,p) => extract_variables p
         | _                 => []

        fun check_if_there_is_no_repeat strlst=
            case strlst of
        []=> true
         | st::strlst' => not( List.exists (fn x => x=st) strlst') andalso  check_if_there_is_no_repeat strlst'
    in
    check_if_there_is_no_repeat(extract_variables pat)
    end



fun match (vl,p)=
    case (vl,p) of
   (_,Wildcard) =>SOME []
 | (_,Variable s) =>SOME [(s,vl)]
 | (Unit,UnitP) => SOME []
 | (Const y,ConstP x) => if x=y then SOME [] else NONE
 | (Tuple v,TupleP ps)  =>if List.length ps=List.length v
                          then all_answers (fn x => match x) (ListPair.zip(v,ps))
                          else NONE
 | (Constructor(x2,y2), ConstructorP(x,y))=>   if x=x2 then match (y2,y) else NONE

 | (_,_) => NONE


fun first_match vl ptlist =
    SOME (first_answer (fn x => match(vl,x)) ptlist) 
    handle NoAnswer=>NONE

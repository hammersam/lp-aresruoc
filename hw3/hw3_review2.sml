exception NoAnswer
datatype pattern = Wildcard
                  |Variable of string
                  |UnitP
                  |ConstP of int
                  |TupleP of pattern list
                  |ConstructorP of string * pattern;

datatype valu = Const of int
               |Unit
               |Tuple of valu list
               |Constructor of string * valu;


fun g f1 f2 p =
    let
    val r = g f1 f2
    in
    case p of
     Wildcard => f1 ()
      | Variable x => f2 x
      | TupleP ps => List.foldl (fn (p, i) => (r p) + i) 0 ps
      | ConstructorP(_, p) => r p
      | _  => 0
    end;

datatype typ = Anything
              |UnitT
              |IntT
              |TupleT of typ list
              |Datatype of string;

(* problem1 *)
fun only_capitals (sl: string list) =
    List.filter (fn s => Char.isUpper(String.sub(s, 0))) sl;

(* problem 2 *)
fun longest_string1 (sl: string list) =
    case sl of
     [] => ""
      | s::sl' => List.foldl (fn (s1, s2) =>
                 if String.size(s1) > String.size(s2)
                 then s1
                 else s2
                             ) s sl';


(* problem 3 *)
fun longest_string2 (sl: string list) =
    case sl of
     [] => ""
      | s::sl' => List.foldl (fn (s1, s2) =>
                 if String.size(s1) < String.size(s2)
                 then s2
                 else s1
                             ) s sl';


(* problem 4 *)
fun longest_string_helper f sl =
    if f(1, 0)
    then longest_string1 sl
    else longest_string2 sl;

val longest_string3 = longest_string_helper (fn (x, y) => x > y);
val longest_string4 = longest_string_helper (fn (x, y) => x < y);

(* problem 5 *)
fun longest_capitalized (sl: string list) =
    let
    val com_fun = longest_string1 o only_capitals
    in
    com_fun sl
    end;


(* problem 6 *)
fun rev_string (s: string) =
    (String.implode o List.rev o String.explode) s;

(* problem 7 *)
fun first_answer f alist =
    case alist of
     [] => raise NoAnswer
      | a::alist' => case f(a) of
                      SOME v => v
                       | NONE => first_answer f alist';


(* problem 8 *)
fun all_answers f alist =
    let
    fun accu fx blist ans =
        case blist of
         [] => SOME ans
          | b::blist' =>
            case (fx b) of
             NONE => NONE
              | SOME vlist => accu fx blist' (ans@vlist)
    in
    accu f alist []
    end;

(* problem 9 *)	
val count_wildcards = g (fn _ => 1) (fn _ => 0);

val count_wild_and_variable_lengths = g (fn _ => 1) String.size;

fun count_some_var (s: string, p: pattern) =
    g (fn _ => 0) (fn x => if x=s then 1 else 0) p;

(* problem 10 *)
fun check_pat (p: pattern) =
    let
    fun find_str p' =
        case p' of
         Variable x => [x]
          | TupleP px =>
            List.foldl (fn (pi, str_list) => (find_str pi)@str_list) [] px
          | ConstructorP(_, p') => find_str p'
          | _ => []

    fun count_repeat (s_list: string list) num (s: string) =
        case s_list of
         [] => num
          | x::x_list => if s = x
                         then count_repeat x_list (num+1) s
                         else count_repeat x_list num s
    val sl = find_str p
    val count = count_repeat sl 0
    in
    List.exists (fn s => count s < 2) sl
    end;

(* problem 11 *)
fun match (v: valu, p: pattern) =
    case p of
     Wildcard => SOME []
      | Variable s => SOME [(s, v)]
      | UnitP => (case v of Unit => SOME [] | _ => NONE)
      | ConstP i => (case v of
                      Const j => (if i = j then SOME[] else NONE)
                       | _ => NONE)
      | TupleP ps => (case v of
                       Tuple vs =>
                       (if List.length(ps) = List.length(vs)
                        then (all_answers match (ListPair.zip (vs, ps)))
			                  else NONE)
		                    | _ => NONE)
      | ConstructorP(s1, px) => (case v of
				                          Constructor(s2, vx) =>
				                          (if s1 = s2
				                           then match (vx, px)
				                           else NONE)
				                           | _ => NONE);


(* problem 12 *)
fun first_match v p_list =
    let
	  val f = first_answer match
    in
	  SOME (f (List.map (fn p => (v, p)) p_list)) handle NoAnswer => NONE
    end;

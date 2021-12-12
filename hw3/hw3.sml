exception NoAnswer

(* 3.1 *)
fun only_capitals strlist =
    List.filter (fn str => Char.isUpper (String.sub (str, 0))) strlist

(* 3.2 *)
fun longest_string1 strlist =
    foldl (fn (str, acc) => if String.size str > String.size acc then str else acc) "" strlist

(* 3.3 *)
fun longest_string2 strlist =
    foldl (fn (str, acc) => if String.size acc > String.size str then acc else str) "" strlist

(* 3.4 *)
fun longest_string_helper f strlist =
    case strlist of
        [] => ""
      | [x] => x
      | x :: y :: strlist' =>
        let
            val longest_tl = longest_string_helper f (y :: strlist')
        in
            if f (String.size longest_tl, String.size x)
            then longest_tl
            else x
        end

val longest_string3 = longest_string_helper (fn (x, y) => x > y)

val longest_string4 = longest_string_helper (fn (x, y) => x >= y)

(* 3.5 *)
val longest_capitalized = longest_string1 o (List.filter (fn str => Char.isUpper (String.sub (str, 0))))

(* 3.6 *)
val rev_string = String.implode o rev o String.explode

(* 3.7 *)
fun first_answer f xs =
    case xs of
        [] => raise NoAnswer
      | x :: xs' => case f x of
                        SOME v => v
                      | NONE => first_answer f xs'

(* 3.8 *)
fun all_answers f xs =
    let fun aux xs acc =
            case xs of
                [] => SOME acc
              | x :: xs' => case f x of
                                SOME lst => aux xs' (lst @ acc)
                              | NONE => NONE
    in
        aux ([], xs)
    end


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

(* 3.9 (a) *)
fun count_wildcards p =
    g (fn () => 1) (fn x => 0) p

(* 3.9 (b) *)
fun count_wild_and_variable_lengths p =
    g (fn () => 1) (fn x => String.size x) p

(* 3.9 (c) *)
fun count_some_var (str, p) =
    g (fn () => 0) (fn x => if str = x then 1 else 0) p

(* 3.10 *)
fun check_pat p =
    let fun get_str p =
            case p of
                Variable x => [x]
              | TupleP ps => List.foldl (fn (p, acc) => get_str p @ acc) [] ps
              | ConstructorP (_, p) => get_str p
              | _ => []
        fun is_distinctive strlist =
            case strlist of
                [] => true
              | x :: xs' => (not (List.exists (fn str : string => str = x) (y :: tl)))
                                andalso
                                is_distinctive xs'
    in
        is_distinctive (get_str p)
    end

(* 3.11 *)
fun match (v, p) =
    case (p, v) of
        (Wildcard, _) => SOME []
      | (Variable s, v1) => SOME [(s, v1)]
      | (UnitP, Unit) => SOME []
      | (ConstP i1, Const i2) => if i1 = i2
                                 then SOME []
                                 else NONE
      | (TupleP ps, Tuple vs) => if List.length ps = List.length vs andalso foldl (fn ((v, p), acc) =>
                                                                                      case match (v, p) of
                                                                                          SOME _ => true andalso acc
                                                                                        | _ => false andalso acc) true (ListPair.zip (vs, ps))
                                 then all_answers match (ListPair.zip (vs, ps))
                                 else NONE
      | (ConstructorP (s1, p), Constructor (s2, v)) => if s1 = s2
                                                       then match (v, p)
                                                       else NONE
      | _ => NONE

(* 3.12 *)
fun first_match v ps =
    SOME (first_answer (fn p => match (v, p)) ps)
    handle NoAnswer => NONE

(* challenge problem *)
datatype typ = Anything
             | UnitT
             | IntT
             | TupleT of typ list
             | Datatype of string

fun typecheck_patterns (ds, ps) =
    (* string -> pattern -> valu *)
    let fun get_datatype ds (s : string) =
            case ds of
                [] => NONE
              | (c, d, t) :: ds' => if s = c then SOME (Datatype d) else get_datatype ds' s

        fun ps2ts ps =
            case ps of
                [] => SOME []
              | p :: ps' =>
                let val t = case p of
                                Wildcard => SOME Anything
                              | Variable _ => SOME Anything
                              | UnitP => SOME UnitT
                              | ConstP _ => SOME IntT
                              | TupleP ps'' => (case (ps2ts ps'') of
                                                    SOME ts => SOME (TupleT ts)
                                                  | _ => NONE)
                              | ConstructorP (s, p) => (get_datatype ds s)
                in
                    case (t, ps2ts ps') of
                        (SOME t, SOME tl) => SOME (t :: tl)
                      | _ => NONE
                end

        fun merge_two_t t1 t2 =
            let fun merge_tuple ts1 ts2 =
                    case (ts1, ts2) of
                        ([], _) => SOME []
                      | (_, [])  => SOME []
                      | (t1 :: ts1', t2 :: ts2') => (case (merge_two_t t1 t2, merge_tuple ts1' ts2') of
                                                         (NONE, _) => NONE
                                                       | (_, NONE) => NONE
                                                       | (SOME merged_t, SOME merged_tl) => SOME (merged_t :: merged_tl))
            in
                case (t1, t2) of
                    (Anything, t2) => SOME t2
                  | (t1, Anything) => SOME t1
                  | (UnitT, UnitT) => SOME UnitT
                  | (IntT, IntT) => SOME IntT
                  | (TupleT ts1, TupleT ts2) =>
                    if List.length ts1 = List.length ts2
                    then (case (merge_tuple ts1 ts2) of
                              SOME merged_tuple => SOME (TupleT merged_tuple)
                            | _ => NONE)
                    else NONE
                  | (Datatype s1, Datatype s2) => if s1 = s2 then SOME (Datatype s1) else NONE
                  | _ => NONE
            end

        fun merge ts =
            case ts of
                [] => raise NoAnswer
              | [t1] => SOME t1
              | t1 :: t2 :: ts' =>
                case (merge_two_t t1 t2) of
                    SOME t => merge (t :: ts')
                  | _ => NONE
    in
        case (ps2ts ps) of
            SOME ts => merge ts
          | _ => NONE
    end

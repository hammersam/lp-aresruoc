(*
PL Equivalence: given same inputs, same outputs and effects
-- Good: Lets us replace bad max with good max
-- Bad: Ignores performance in the extreme

Asymptotic equivalence: Ignore constant factors
-- Good: Focus on the algorithm and efficiency for large inputs
-- Bad: Ignores "four time faster"

Systems equivalence: Account for constant overheads, performance tune
-- Good: Faster means different and better
-- Bad: Beware overtuning on "wrong"(e.g., small) inputs;
definition does not let you "swap in a different algorithm"
*)
fun slow_max xs =
    case xs of
        [] => raise List.Empty
      | x :: [] => x
      | x :: xs' =>
        if x > slow_max xs' then x
        else slow_max xs'

fun fast_max xs =
    case xs of
        [] => raise List.Empty
      | x :: [] => x
      | x :: xs' =>
        let val y = fast_max xs'
        in
            if x > y then x
            else y
        end

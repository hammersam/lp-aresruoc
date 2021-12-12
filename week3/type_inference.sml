(*
The reason why function 'sum_triple' does not compile is that
the type system of sml can not determine the length of 'triple',
it may have three elements or four elements or five elements, which
causes an error message.

fun sum_triple triple =
    #1 triple + #2 triple + #3 triple
*)
fun sum_triple_can_compile (triple : int * int * int) =
    #1 triple + #2 triple + #3 triple

(* nested patterns *)
exception ListLengthMismatch

fun zip list_triple =
    case list_triple of
        ([], [], []) => []
      | (x :: xs, y :: ys, z :: zs) => (x, y, z) :: zip (xs, ys, zs)
      | _ => raise ListLengthMismatch

fun unzip triple_list =
    case triple_list of
        [] => ([], [], [])
      | (x, y, z) :: tl =>
        let val (xs, ys, zs) = unzip tl in
            (x :: xs, y :: ys, z :: zs)
        end

val lt_example = ([1, 2, 3], ["one", "two", "three"], ["yi", "er", "san"])
val zipTest = lt_example = unzip (zip lt_example)

fun nondecreasing xs =
    case xs of
        [] => true
      | _ :: [] => true
      | x :: y :: tl => x <= y andalso nondecreasing (y :: tl)

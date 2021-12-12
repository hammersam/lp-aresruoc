(* bad style *)
fun sum_triple triple =
    case triple of
        (x, y, z) => x + y +z

fun full_name r =
    case r of
        { first = x,
          middle = y,
          last = z } =>
        x ^ " " ^ y ^ " " ^ z

(* better style *)
fun sum_triple_better_style triple =
    let val (x, y, z) = triple
    in
        x + y + z
    end

fun full_name_better_style r =
    let val { first = x, middle = y, last = z } = r
    in
        x ^ " " ^ y ^ " " ^ z
    end

(* great style *)
fun sum_triple_great_style (x, y, z) =
    x + y + z

fun full_name_great_style { first = x, middle = y, last = z } =
    x ^ " " ^ y ^ " " ^ z

fun rotate_left (x, y, z) =
    (y, z, x)

fun rotate_right t =
    rotate_left (rotate_left t)

fun accumulator xs =
    let fun inner_accu (xs, sum) =
            case xs of
                [] => sum
              | x :: xs' => inner_accu (xs', sum + x)
    in
        inner_accu (xs, 0)
    end

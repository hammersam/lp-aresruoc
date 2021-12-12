fun option_max(xs : int list) =
  if null xs 
  then NONE
  else 
    let val tl_ans = option_max(tl xs)
    in 
      if isSome tl_ans andalso valOf tl_ans > hd xs
      then tl_ans
      else SOME (hd xs)
    end

fun more_option_max(xs : int list) =
  if null xs
  then NONE
  else let fun getMax(xs : int list) =
    if null (tl xs)
    then hd xs
    else 
      let val tl_ans = getMax(tl xs)
      in
        if tl_ans > hd xs
        then tl_ans
        else hd xs
      end
    in
      SOME (getMax xs)
    end

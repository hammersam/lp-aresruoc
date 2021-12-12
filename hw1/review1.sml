(* problem one *)
fun is_older (date_one : int * int * int, date_two : int * int * int) =
    if
    #1 date_one < #1 date_two
    then
        true
    else
        if #1 date_one = #1 date_two
        then
            if #2 date_one < #2 date_two
            then
                true
            else
                if #2 date_one = #2 date_two
                then
                    if #3 date_one < #3 date_two
                    then
                        true
                    else
                        false
                    else
                        false
                    else
                        false

                        (* problem two *)
fun number_in_month (dates : (int * int * int) list, month : int) =
    if null dates
    then
        0
    else
        if #2 (hd (dates)) = month
        then
            1 + number_in_month(tl dates, month)
        else
            0 + number_in_month(tl dates, month)

            (* problem three *)			       
fun number_in_months (dates: (int * int * int) list, months : int list) =
    if null dates orelse null months
    then 0
    else
        number_in_month(dates, hd months) + number_in_months(dates, tl months)

        (* problem four *)
fun dates_in_month (dates : (int * int * int) list, month : int) =
    if null dates
    then []
    else
        let
            val date = hd dates
        in
            if #2 date = month
            then
                date :: dates_in_month(tl dates, month)
            else
                dates_in_month(tl dates, month)
        end

        (* problem five *)
fun dates_in_months (dates : (int * int * int) list, months : int list) =
    if null dates orelse null months
    then []
    else
        dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

        (* problem six *)
fun get_nth (sequence : string list, index : int) =
let
    fun comparator (sequence : string list, index : int, count : int) =
        if null sequence
        then hd sequence
        else
            if index = count
            then
                hd sequence
            else
                comparator(tl sequence, index, count + 1)
        in
            comparator(sequence, index, 1)
        end

        (* problem seven *)	     
fun date_to_string (date: int * int * int) =
let
    val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"];
    val month = get_nth(months, #2 date);
in
    month ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
end

(* problem eight *)
fun number_before_reaching_sum (sum : int, integers : int list) =
let
    fun comparator (integers : int list, total : int, count : int) =
        if total >= sum
        then count
        else
            comparator(tl integers, total + (hd integers), count + 1)
in
    comparator(tl integers, hd integers, 0)
end

(* problem nine *)
fun what_month (day: int) =
let
    val days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
    val month = number_before_reaching_sum(day, days)
in
    month + 1
end

(* problem ten *)
fun count_up (day1: int, day2: int) =
let
    fun counter (x: int, day2: int) =
        if x > day2
        then []
        else x :: counter(x + 1, day2)
in
    counter(day1, day2)
end 

fun month_range (day1: int, day2: int) =
let
    val range = count_up (day1, day2)
    fun monther (days: int list) =
        if null days
        then []
        else
            what_month(hd days) :: monther (tl days)
in
    monther range
end

(* problem eleven *)
fun oldest (dates : (int * int * int) list) =
    if null dates
    then NONE
    else
        let
            fun oldest_nonempty (dates : (int * int * int) list) =
                if null (tl dates)
                then hd dates
                else
                    let
                        val tl_ans = oldest_nonempty(tl dates)
in
    if is_older(hd dates, tl_ans)
    then hd dates
    else tl_ans
end
                    in
                        SOME (oldest_nonempty dates)
                    end










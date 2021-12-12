fun is_older (date1 : int * int * int, date2 : int * int * int) =
    let
        val year1 = (#1 date1)
        val year2 = (#1 date2)
        val month1 = (#2 date1)
        val month2 = (#2 date2)
        val day1 = (#3 date1)
        val day2 = (#3 date2)
    in
        (year1 < year2)
        orelse
        ((year1 = year2) andalso (month1 < month2))
        orelse
        ((year1 = year2) andalso (month1 = month2) andalso (day1 < day2))
    end

fun number_in_month (dateslist : (int * int * int) list, month : int) =
    if null dateslist
    then 0
    else
        let val date = (hd dateslist) in 
            if (#2 date) = month
            then 1 + number_in_month(tl dateslist, month)
            else number_in_month(tl dateslist, month)
        end

fun number_in_months (dateslist : (int * int * int) list, months : int list) =
    if null months
    then 0
    else 
        number_in_month (dateslist, hd months)
        +
        number_in_months (dateslist, tl months)

fun dates_in_month (dateslist : (int * int * int) list, month : int) =
    if null dateslist
    then []
    else 
        let val date = (hd dateslist) in
            if (#2 date) = month
            then date :: dates_in_month(tl dateslist, month)
            else dates_in_month(tl dateslist, month)
        end

fun dates_in_months (dateslist : (int * int * int ) list, months : int list) =
    if null months
    then []
    else
        dates_in_month(dateslist, hd months) (* calculate how many dates in one
        month *) 
        @
        dates_in_months(dateslist, tl months) (* handle the following months *)

fun get_nth (strlist : string list, n : int) =
    if n = 1
    then hd strlist
    else
        get_nth(tl strlist, n - 1)

fun date_to_string (date : int * int * int) =
    let
        val strMonths = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
        val year = (#1 date)
        val month = (#2 date)
        val day = (#3 date)
    in
        get_nth(strMonths, month) ^
        " " ^
        Int.toString(day) ^
        ", " ^
        Int.toString(year)
    end

fun number_before_reaching_sum (sum : int, xs : int list) =
    let fun get_n (localsum : int, xs : int list) =
            if localsum + (hd xs) >= sum
            then 0
            else
                1 + get_n (localsum + (hd xs), tl xs)
    in
        get_n(0, xs)
    end

fun what_month (day : int) =
    let
        val daysOfMonths = [31,28,31,30,31,30,31,31,30,31,30,31]
    in
        1 + number_before_reaching_sum(day, daysOfMonths)
    end

fun month_range (day1 : int, day2 : int) =
    let
        fun get_month_range (day1 : int, day2 : int) =
            if day1 > day2
            then []
            else (what_month day1) :: get_month_range(day1 + 1, day2)
    in
        get_month_range(day1, day2)
    end

fun oldest (dates : (int * int * int) list) =
    if null dates
    then NONE
    else let
        fun get_oldest (dates : (int * int * int) list,
                        oldest : int * int * int) =
            if null dates
            then oldest
            else
                if is_older(hd dates, oldest)
                then get_oldest(tl dates, hd dates)
                else get_oldest(tl dates, oldest)
    in
        SOME (get_oldest(dates, hd dates))
    end

fun remove_duplicates (xs : int list) =
    let
        fun not_in (x : int, xs : int list) =
            if null xs
            then true
            else
                if x = hd xs then false
                else not_in(x, tl xs)
        fun remove_duplicates_inside (origin_xs : int list, returned_xs : int list)
            =
            if null origin_xs
            then returned_xs
            else remove_duplicates_inside(
                    tl origin_xs,
                    if not_in(hd origin_xs, returned_xs)
                    then (hd origin_xs) :: returned_xs
                    else returned_xs
                )
    in
        remove_duplicates_inside(xs, [])
    end

fun number_in_months_challenge (dateslist : (int * int * int) list,
                                months :int list) =
    number_in_months(dateslist, remove_duplicates(months))

fun dates_in_months_challenge (dateslist : (int * int * int) list,
                               months : int list) =
    dates_in_months(dateslist, remove_duplicates(months))

fun reasonable_date (date : int * int * int) =
    let
        fun appropriate_number (date : int * int * int) =
            let
                val year = #1 date
                val month = #2 date
                val day = #3 date
            in
                year > 0 andalso
                month >= 1 andalso month <= 12 andalso
                day >= 1 andalso day <= 31
            end
        fun get_nth (lis : 'a list, n : int) =
            if n = 1
            then hd lis
            else
                get_nth(tl lis, n - 1)
        fun a_real_date (date : int * int * int) =
            let
                val year = #1 date
                val month = #2 date
                val day = #3 date
                val calendar = (
                  [31,29,31,30,31,30,31,31,30,31,30,31],
                  [31,28,31,30,31,30,31,31,30,31,30,31]
                )
            in
                if (year mod 400 = 0) orelse (year mod 4 = 0 andalso year mod 100 <> 0)
                then day <= get_nth(#1 calendar, month)
                else day <= get_nth(#2 calendar, month)
            end
    in
        appropriate_number(date) andalso a_real_date(date)
    end


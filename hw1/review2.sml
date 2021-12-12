(*
   A 'date' is a tuple of type (int * int * int). Where int refers to a 'year', a 
   'month' and a 'day' respectively.  	    	  	    	                  
   A 'date' is valid if: 0 < 'year', 0 < 'month' < 13, 0 < 'day' < 32.
*)

(*
   Signature: date date -> bool
   Interpretation: Returns true if the first argument happened before the second
   		             argument; returns false otherwise.
*)
fun is_older(date1 : int * int * int, date2 : int * int * int) =
    (* If the years, months or days are differnt, return the difference between
       the two respectively. *)
    if #1 date1 <> #1 date2
    then #1 date1 < #1 date2
    else if #2 date1 <> #2 date2
         then #2 date1 < #2 date2
         else #3 date1 < #3 date2

(*
  Signature: (date list) month -> int.
  Interpretation: Returns the number of dates present in a given list of dates for a
                  given month.
  number_in_month(dates, month) = int
*)
fun number_in_month(dates : (int * int * int) list, month : int) =
    let
	fun count_dates(list_dates : (int * int * int) list, count : int) =
            if null list_dates
            then count
            else if #2 (hd list_dates) = month
                 then count_dates(tl list_dates, count + 1)
                 else count_dates(tl list_dates, count)
    in
	count_dates(dates, 0)
    end

(*
  Signature: (date list) (month list) -> int.
  Interpretation: Returns the number of dates present in a give list of dates for
  		  all months present in a list of months. Assuming there are no 
		  repeatded months.
*)
fun number_in_months(dates : (int * int * int) list, months : int list) = 
    let
	fun count_dates(list_months : int list, count : int) =
	    if null list_months
	    then count
	    else count_dates(tl list_months, count + number_in_month(dates, hd list_months))
    in
	count_dates(months, 0)
    end
 
(*
  Signature: (date list) month -> (date list).
  Interpretation: Returns a list of dates present in a given list of dates for a given month.
*)
fun dates_in_month(dates : (int * int * int) list, month : int) =
    let
        fun extract_dates(list_dates : (int * int * int) list, output : (int * int * int) list) =
            if null list_dates
            then output
            else if #2 (hd list_dates) = month
                 then extract_dates(tl list_dates, output@[hd list_dates])
                 else extract_dates(tl list_dates, output)
    in
        extract_dates(dates, [])
    end

(*
  Signature: (date list) (month list) -> (date list).
  Interpretation: Returns a list of dates present in a given list of dates for all months in
                  a given list of months.
*)
fun dates_in_months(dates : (int * int * int) list, months : int list) =
    let
	fun extract_dates(list_months : int list, output : (int * int * int) list) =
	    if null list_months
	    then output
	    else extract_dates(tl list_months, output@dates_in_month(dates, hd list_months))
    in
	extract_dates(months, [])
    end

(*
  Signature: (string list) index -> string
  Interpretation: Returns the value of index from a given list of strings. 
*)
fun get_nth(strings : string list, index : int) =
    let
	fun extract_string(list_strings : string list, counter : int) =
	    if null list_strings
	    then ""
	    else if counter = index
	         then hd list_strings
	         else extract_string(tl list_strings, counter + 1)
    in
	extract_string(strings, 1)
    end

(*
  Signature: date -> string
  Interpretation: Returns a string version of a given date.
*)
fun date_to_string(date : int * int * int) =
    let
	val months = ["January", "February", "March", "April", "May", "June",
		      "July", "August", "September", "October", "November", "December"]
	val month = get_nth(months, #2 date)
    in
	month ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end

(*
  Signature: (number list) sum -> int
  Interpretation: Returns the max number of elements in a given list that add up to less than sum.
*)
fun number_before_reaching_sum(numbers : int list, sum : int) =
    let
	exception InvalidArguments
	fun count(list_numbers : int list, adds : int, counter : int) =
	    if null list_numbers
	    then raise InvalidArguments
	    else if adds + hd list_numbers >= sum
	         then counter
		 else count(tl list_numbers, adds + hd list_numbers, counter + 1)
    in
	count(numbers, 0, 0)
    end
	
(*
  Signature: day -> string
  Interpretation: Returns what month the date is in.
*)
fun what_month(day : int) =
    let
	exception InvalidArguments
	val days = [31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365]
	fun extract_month(list_days : int list, month : int) =
	    if null list_days
	    then raise InvalidArguments
	    else if day <= hd list_days 
       	         then month
	         else extract_month(tl list_days, month + 1)
    in
	extract_month(days, 1)
    end

(*
  Signature: day day -> int list
  Interpretation: Returns a list of months ranging from day1 to day2.
*)
fun month_range(day1 : int, day2 : int) =
    let
	fun create_list(count : int, month_list : int list) =
	    if count > day2
	    then month_list
	    else create_list(count + 1, month_list@[what_month(count)])
    in
	create_list(day1, [])
    end

(*
  Signature: (date list) -> date
  Interpretation: Returns the oldest date in a given list. 
*)
fun oldest(dates : (int * int * int) list) =
    if null dates
    then NONE
    else let
	     fun find_oldest(list_dates : (int * int * int) list, oldest : (int * int * int) option) =
		 if null list_dates
		 then oldest
		 else if is_older(valOf(oldest), hd list_dates)
		      then find_oldest(tl list_dates, oldest)
		      else find_oldest(tl list_dates, SOME(hd list_dates))
         in
	    find_oldest(tl dates, SOME(hd dates))
         end
	

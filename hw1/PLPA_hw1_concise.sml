(* Homework 1 *)
(* Programming Languages, Part A (University of Washington) *)

(* Student: Jared Cooney *)
(* jaredcooney2@gmail.com *)

(* Language: Standard ML *)

(* =========================================================== *)

(* 1. *)
(* produce true iff date1 is earlier than date2 *)
fun is_older (date1 : (int * int * int), date2 : (int * int * int)) =
    if #1 date1 < #1 date2
    then true
    else if #1 date1 > #1 date2
    then false
    else if #2 date1 < #2 date2
    then true
    else if #2 date1 > #2 date2
    then false
    else #3 date1 < #3 date2
 

		      
(* 2. *)
(* produce the number of dates in the list that are in the given (int) month *)
fun number_in_month (dates : ((int * int * int) list), month : int) =
    (* rsf is a result-so-far accumulator for tail recursion *)
    let fun number_in_month (dates : ((int * int * int) list), rsf : int) =
	    if null dates
	    then rsf
	    else number_in_month(tl dates, rsf + (if #2 (hd dates) = month
						  then 1
						  else 0))
    in number_in_month(dates, 0)
    end

	

(* 3. *)
(* produce the number of dates that are in any of the given months *)
(* Assumes: the list of months contains no repeated numbers *)
fun number_in_months (dates : (int * int * int) list, months : int list) =
    if null months
    then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)



(* 4. *)
(* filter the list of dates to include only those that are in the given (int) month *)
fun dates_in_month (dates : (int * int * int) list, month : int) =
    if null dates
    then []
    else if #2 (hd dates) = month
    then (hd dates) :: dates_in_month(tl dates, month)
    else dates_in_month(tl dates, month)

		       

(* 5. *)
(* filter the list of dates to include only those that are in any of the given months *)
(* Assumes: the list of months contains no repeated numbers *)
fun dates_in_months (dates : (int * int * int) list, months : int list) =
    if null months
    then []
    else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)



(* 6. *)
(* produce the nth element of the given list, where n=1 corresponds to the first element *)
(* Assumes: the given list contains at least n elements, and n >= 1 *)
fun get_nth (strings : string list, n : int) =
    if n = 1
    then hd strings
    else get_nth(tl strings, n-1)


		
(* 7. *)
(* produce a string representation of the given date, formatted as in "April 14, 2021" *)
fun date_to_string (date : (int * int * int)) =
    let val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end



(* 8. *)
(* produce n such that the first n numbers in the list sum to less than the given sum, and the first n+1 numbers sum to at least the given sum *)
(* Assumes: sum is positive, all numbers in list are nonnegative, and entire list sums to more than the given sum *)
fun number_before_reaching_sum (sum : int, numbers : int list) =
    if sum <= hd numbers
    then 0
    else 1 + number_before_reaching_sum(sum - (hd numbers), tl numbers)

				       

(* 9. *)
(* takes a day of the year (an int in [1,365]) and returns the month (int) that day is in *)
fun what_month (day : int) =
    let val month_lengths = [31,28,31,30,31,30,31,31,30,31,30,31]
    in 1 + number_before_reaching_sum(day, month_lengths)
    end
	


(* 10. *)
(* produce a list containing the corresponding month (int) for each day of the year from day1 to day2 *)
fun month_range (day1 : int, day2: int) =
    if day1 > day2
    then []
    else what_month(day1) :: month_range(day1 + 1, day2)


					
(* 11. *)
(* produce the oldest date in the list, or None if the list is empty *)
fun oldest (dates : (int * int * int) list) =
    if null dates
    then NONE
	     (* oldest_so_far is an accumulator for tail recursion *)
    else let fun oldest_tr (dates, oldest_so_far) =
		 if null dates
		 then oldest_so_far
		 else oldest_tr(tl dates, (if is_older(hd dates, oldest_so_far)
					   then hd dates
					   else oldest_so_far))
			       
	 in SOME(oldest_tr(tl dates, hd dates))
	 end


	     
(* helper function *)
(* reverse the given list *)
fun reverse_list (lst) =
    (* acc is an accumulator for tail-recursion *)
    let fun reverse_list (lst, acc) =
	    if null lst
	    then acc
	    else reverse_list(tl lst, (hd lst)::acc)
			     
    in reverse_list(lst, [])
    end



(* helper function *)
(* produce true iff num appears in the given list *)
fun is_member (num : int, numbers : int list) =
    if null numbers
    then false
    else if (hd numbers) = num
    then true
    else is_member(num, tl numbers)
	

		  
(* helper function *)
(* remove duplicate values from the list; only the first instance of any given number remains *)
fun remove_duplicates (numbers : int list) =
    let fun remove_duplicates (numbers : int list, visited : int list, rsf : int list) =
	    if null numbers
	    then rsf
	    else if is_member((hd numbers), visited)
	    then remove_duplicates(tl numbers, visited, rsf)
	    else remove_duplicates(tl numbers, (hd numbers)::visited, (hd numbers)::rsf)
				  
    in reverse_list(remove_duplicates(numbers, [], []))
    end


	
(* 12a. *)
(* produce the number of dates that are in any of the given months *)
fun number_in_months_challenge (dates : (int * int * int) list, months : int list) =
    number_in_months(dates, remove_duplicates(months))



(* 12b. *)
(* filter the list of dates to include only those that are in any of the given months *)
fun dates_in_months_challenge (dates : (int * int * int) list, months : int list) = 
    dates_in_months(dates, remove_duplicates(months))
		   


(* 13. *)
(* produce true iff the given date describes a real date in the common era *)
fun reasonable_date (date : (int * int * int)) =
    let val norm_month_lengths = [31,28,31,30,31,30,31,31,30,31,30,31]
	val leap_month_lengths = [31,29,31,30,31,30,31,31,30,31,30,31]

	fun get_month_length (month_lengths : int list, month : int) =
	    if month = 1
	    then hd month_lengths
	    else get_month_length(tl month_lengths, month - 1)

	fun is_leap_year (year : int) =
	    (year mod 400 = 0) orelse ((year mod 4 = 0) andalso (year mod 100 <> 0))
					  
	fun reasonable_year (year : int) =
	    year  > 0

	fun reasonable_month (month : int) =
	    0 < month andalso month <= 12

	fun reasonable_day(date : (int * int * int)) =
	    0 < (#3 date) andalso (#3 date) <= (if is_leap_year(#1 date)
						then get_month_length(leap_month_lengths, (#2 date))
						else get_month_length(norm_month_lengths, (#2 date)))
						   
    in reasonable_year(#1 date) andalso reasonable_month(#2 date) andalso reasonable_day(date)
    end

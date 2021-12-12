(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1 = only_capitals ["A","B","C"] = ["A","B","C"]

val test2 = longest_string1 ["A","bc","C"] = "bc"

val test3 = longest_string2 ["A","bc","C"] = "bc"

val test4a = longest_string3 ["A","bc","C"] = "bc"

val test4b = longest_string4 ["A","B","C"] = "C"

val test5 = longest_capitalized ["A","bc","C"] = "A"

val test6 = rev_string "abc" = "cba"

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4

val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE

val test9a = count_wildcards Wildcard = 1

val test9b = count_wild_and_variable_lengths (Variable("a")) = 1

val test9c = count_some_var ("x", Variable("x")) = 1

val test10 = check_pat (Variable("x")) = true

val test11 = match (Const(1), UnitP) = NONE

val test12 = first_match Unit [UnitP] = SOME []

val challenge_test1 = typecheck_patterns ([], [ConstP 10, Variable "a"]) = SOME IntT

val challenge_test2 = typecheck_patterns ([], [ConstP 10, Variable "a", ConstructorP("SOME",Variable "x")]) = NONE

val challenge_test3 = typecheck_patterns ([], [TupleP[Variable "a", ConstP 10, Wildcard], TupleP[Variable "b", Wildcard, ConstP 11], Wildcard]) = SOME (TupleT[Anything, IntT, IntT])

val challenge_test4 = typecheck_patterns ([("Red","color",UnitT),("Green","color",UnitT),("Blue","color",UnitT)], [ConstructorP("Red", UnitP), Wildcard]) = SOME (Datatype "color")

val challenge_test5 = typecheck_patterns ([("Sedan","auto", Datatype "color"),("Truck","auto",TupleT[IntT, Datatype "color"]),("SUV","auto",UnitT)], [ConstructorP("Sedan", Variable "a"), ConstructorP("Truck", TupleP[Variable "b", Wildcard]), Wildcard]) = SOME (Datatype "auto")

val challenge_test6 = typecheck_patterns ([("Empty", "list", UnitT), ("List", "list", TupleT[Anything, Datatype "list"])], [ConstructorP("Empty",UnitP),ConstructorP("List",TupleP[ConstP 10, ConstructorP("Empty",UnitP)]), Wildcard]) = SOME (Datatype "list")

val challenge_test7 = typecheck_patterns ([("Empty","list",UnitT),("List","list",TupleT[Anything, Datatype "list"])], [ConstructorP("Empty",UnitP),ConstructorP("List",TupleP[Variable "k", Wildcard])]) = SOME (Datatype "list")

val challenge_test8 = typecheck_patterns ([("Empty", "list", UnitT), ("List", "list", TupleT[Anything, Datatype "list"])], [ConstructorP("Empty", UnitP), ConstructorP("List", TupleP[ConstructorP("Sedan", Variable "c"), Wildcard])]) = SOME (Datatype "list")

val challenge_test9 = typecheck_patterns ([], [TupleP[Variable "x", Variable "y"], TupleP[Wildcard, Wildcard]]) = SOME (TupleT[Anything, Anything])

val challenge_test10 = typecheck_patterns ([], [TupleP[Wildcard, Wildcard], TupleP[Wildcard, TupleP[Wildcard, Wildcard]]]) = SOME (TupleT[Anything, TupleT[Anything, Anything]])

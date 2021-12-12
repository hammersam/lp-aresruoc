(* The reason why the designers of sml decided to make
 * tuples defined completely as a special kind of record:
 * 1. They wanted the implementation to be easier
 * 2. They wanted the core/base language smaller and more concise
 * 3. They wanted the formal model of the language small so that proofs
 * about it would be shorter
 *)

e1 andalso e2 = if e1 then e2 else false

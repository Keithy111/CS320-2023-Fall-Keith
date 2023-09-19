#use "./../../../classlib/OCaml/MyOCaml.ml";;
(* No other library imports are allowed. *)

(* ************************************************ *)

(* Question 7: 10 points

   Given the following snippet, implement the test
   function so that isPrime returns true for prime
   number inputs and false otherwise. *)

let isPrime(n) =
let sqrt_n = int_of_float (sqrt (float_of_int n)) in
let rec is_divisible d =
  if d > sqrt_n then
    true
  else if n mod d = 0 then
    false
  else
    is_divisible (d + 1)
in
is_divisible 2

in  

  (* Check if n is less than 2; if so, it's not prime *)
  if n < 2 then false else int1_forall(n)(test)
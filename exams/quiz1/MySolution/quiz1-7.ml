#use "./../../../classlib/OCaml/MyOCaml.ml";;
(* No other library imports are allowed. *)

(* ************************************************ *)

(* Question 7: 10 points

   Given the following snippet, implement the test
   function so that isPrime returns true for prime
   number inputs and false otherwise. *)

let isPrime(n) =
  if i <= 1 then false (* Numbers less than or equal to 1 are not prime *)
  else if i == 2 then true (* 2 is prime *)
  else
    let rec is_divisible j =
      if j * j > i then true
    else if i mod j == 0 then false
    else is_divisible (j + 1)
  in
  is_divisible 2
in  

  (* Check if n is less than 2; if so, it's not prime *)
  if n < 2 then false else int1_forall(n)(test)
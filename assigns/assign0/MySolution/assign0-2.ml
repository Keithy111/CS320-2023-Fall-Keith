(* ****** ****** *)

(*
Assign0-2: 10 points
Please implement a function that tests whether
a given natural number is a prime:
fun isPrime(n0: int): bool
*)

(* ****** ****** *)

(* Helper function to check divisibility *)
let is_divisible n d =
  n mod d = 0

(* Function to test if a number is prime *)
let isPrime n0 =
  if n0 <= 1 then
    false
  else if n0 <= 3 then
    true
  else if is_divisible n0 2 || is_divisible n0 3 then
    false
  else
    let rec check_prime i =
      if i * i > n0 then
        true
      else if is_divisible n0 i || is_divisible n0 (i + 2) then
        false
      else
        check_prime (i + 6)
    in
    check_prime 5
;;


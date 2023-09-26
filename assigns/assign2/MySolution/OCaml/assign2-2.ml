(*
//
Assign2-2: 10 points
//
Please implement mylist_get_at based
on pattern matching that computes the
element of a given mylist at a given
position.
//
You should call mylist_subscript_exn if
the give position is out-of-bounds.
//
let rec
mylist_get_at(xs: 'a mylist)(i0: int): 'a = ...
//
*)
type 'a mylist =
  | MyNil
  | MyCons of 'a * 'a mylist
  | MySnoc of 'a mylist * 'a
  | MyReverse of 'a mylist
  | MyAppend2 of 'a mylist * 'a mylist

(* ****** ****** *)
exception MySubscript;;
(* ****** ****** *)

let rec mylist_length (xs: 'a mylist): int =
  match xs with
  | MyNil -> 0
  | MyCons(_, xs) -> 1 + mylist_length xs
  | MySnoc(xs, _) -> 1 + mylist_length xs
  | MyReverse(xs) -> mylist_length xs
  | MyAppend2(xs1, xs2) -> mylist_length xs1 + mylist_length xs2
  
let rec mylist_get_at (xs: 'a mylist)(i0: int): 'a =
  match xs, i0 with
  | MyNil, _ -> raise MySubscript
  | _, n when n < 0 -> raise MySubscript
  | MyCons(x, _), 0 -> x
  | MyCons(_, rest), n -> mylist_get_at rest (n - 1)
  | MySnoc(_, x), 0 -> x
  | MySnoc(rest, _), n -> mylist_get_at rest n
  | MyReverse(list), n -> mylist_get_at list n
  | MyAppend2(xs1, xs2), n ->
    let len_xs1 = mylist_length xs1 in
    if n < len_xs1 then mylist_get_at xs1 n
    else mylist_get_at xs2 (n - len_xs1)
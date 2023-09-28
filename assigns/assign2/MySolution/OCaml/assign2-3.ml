(* ****** ****** *)

(*
//
Assign2-3: 10 points
//
Please implement foldleft_to_iforeach that turns a
foldleft-loop into a iforeach-loop:
let
foldleft_to_iforeach
(foldleft: ('xs, 'x0, int) foldleft): ('xs, 'x0) iforeach = ...
*)

(* ****** ****** *)

#use "./../../assign2.ml";;
#use "./../../../../classlib/OCaml/MyOCaml.ml";;

type 'a mylist =
  | MyNil
  | MyCons of 'a * 'a mylist
  | MySnoc of 'a mylist * 'a
  | MyReverse of 'a mylist
  | MyAppend2 of 'a mylist * 'a mylist

  let rec
mylist_foreach
(xs: 'a mylist)
(work: 'a -> unit): unit =
match xs with
| MyNil -> ()
| MyCons(x1, xs) ->
  (work(x1); mylist_foreach(xs)(work))
| MySnoc(xs, x1) ->
  (mylist_foreach(xs)(work); work(x1))
| MyReverse(xs) -> mylist_rforeach(xs)(work)
| MyAppend2(xs1, xs2) ->
  (mylist_foreach(xs1)(work); mylist_foreach(xs2)(work))

and
mylist_rforeach
(xs: 'a mylist)
(work: 'a -> unit): unit =
match xs with
| MyNil -> ()
| MyCons(x1, xs) ->
  (mylist_rforeach(xs)(work); work(x1))
| MySnoc(xs, x1) ->
  (work(x1); mylist_rforeach(xs)(work))
| MyReverse(xs) -> mylist_foreach(xs)(work)
| MyAppend2(xs1, xs2) ->
  (mylist_rforeach(xs2)(work); mylist_rforeach(xs1)(work))
;;

type
('xs, 'x0) foreach = 'xs -> ('x0 -> unit) -> unit

type
('xs, 'x0) rforeach = 'xs -> ('x0 -> unit) -> unit

type
('xs, 'x0) iforeach = 'xs -> (int -> 'x0 -> unit) -> unit

type
('xs, 'x0, 'r0) foldleft = 'xs -> 'r0 -> ('r0 -> 'x0 -> 'r0) -> 'r0

let rec
list_foldleft
(xs: 'a list)
(r0: 'r0)(fopr: 'r0 -> 'a -> 'r0): 'r0 =
match xs with
| [] -> r0
| (x1 :: xs) ->
  list_foldleft(xs)(fopr(r0)(x1))(fopr)
;;

let foldleft_to_iforeach (foldleft: ('xs, 'x0, int) foldleft): ('xs, 'x0) iforeach =
  fun xs f ->
    let rec process_list xs accumulator =
      match xs with
      | [] -> ()
      | hd :: tl ->
        let new_accumulator = foldleft xs accumulator hd in
        f xs new_accumulator;
        process_list tl new_accumulator
    in
    process_list xs foldleft.initial
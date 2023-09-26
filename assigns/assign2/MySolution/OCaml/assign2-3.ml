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

let rec mylist_length (xs: 'a mylist): int =
  match xs with
  | MyNil -> 0
  | MyCons(_, xs) -> 1 + mylist_length xs
  | MySnoc(xs, _) -> 1 + mylist_length xs
  | MyReverse(xs) -> mylist_length xs
  | MyAppend2(xs1, xs2) -> mylist_length xs1 + mylist_length xs2

let foldleft_to_iforeach (foldleft: ('xs, 'x0, int) foldleft): ('xs, 'x0) iforeach =
  fun xs f ->
    let rec loop idx acc xs =
      match xs with
      | MyNil -> ()
      | MyCons (x, rest) ->
        let acc' = foldleft acc x (fun a b -> a) in
        f idx acc';
        loop (idx + 1) acc' rest
      | MySnoc (rest, x) ->
        let acc' = foldleft acc x (fun a b -> a) in
        f idx acc';
        loop (idx + 1) acc' rest
      | MyReverse xs' ->
        let acc' = foldleft acc xs' (fun a b -> a) in
        f idx acc';
        loop (idx + 1) acc' xs'
      | MyAppend2 (xs1, xs2) ->
        let acc' = foldleft acc xs1 (fun a b -> a) in
        loop idx acc' xs1;
        loop (idx + mylist_length xs1) acc' xs2
    in
    loop 0 foldleft xs

  


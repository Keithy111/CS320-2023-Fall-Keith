(* ****** ****** *)

(*
Assign3-3:
HX-2023-09-26: 10 points
//
The function [list_nchoose(xs)(n0)]
returns all the subsequences of xs that are
of length n0.
//
let
list_nchoose
(xs: 'a list)(n0: int): 'a list list =
//
Please give a NON-RECURSIVE implementation of
list_nchoose based on list-combinators. Note that
the order of the elements in a list representation
of a subsequenc is SIGNIFICANT. For instance, [1;2]
and [2;1] are DIFFERENT.
//
*)

(* ****** ****** *)

#use "./../../../../classlib/OCaml/MyOCaml.ml";;

let list_nchoose (xs: 'a list) (n0: int): 'a list list =
  let rec combinations n lst =
    match n, lst with
    | 0, _ -> [[]]
    | _, [] -> []
    | k, x::xs' ->
        let with_x = List.map (fun c -> x :: c) (combinations (k-1) xs') in
        let without_x = combinations k xs' in
        with_x @ without_x
  in
  combinations n0 xs
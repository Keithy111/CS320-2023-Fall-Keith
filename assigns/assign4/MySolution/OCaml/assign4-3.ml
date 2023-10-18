(* ****** ****** *)

(*
//
Assign4-3:
//
HX-2023-10-05: 10 points
//
Please enumerate a gtree in the manner of
depth-first search:
//
let rec (* 5 points *)
gtree_streamize_dfs(xs: 'a gtree): 'a stream
//
Please enumerate a gtree in the manner of
breadth-first search:
//
let rec (* 5 points *)
gtree_streamize_bfs(xs: 'a gtree): 'a stream
//
*)

(* ****** ****** *)
#use "./../../../../classlib/OCaml/MyOCaml.ml";;

type 'a gtree =
  | GTnil
  | GTcons of 'a * ('a gtree list)


(*
//
Assign2-4: 10 points
//
Please give a combinator-based implementation of
string_sepjoin_list:
let
string_sepjoin_list
(sep: string)(xs: string list): string = ...

For instance,
string_sepjoin_list("")(["1";"22";"333"]) = "122333"
For instance,
string_sepjoin_list(",")(["1";"22";"333"]) = "1,22,333"
For instance,
string_sepjoin_list(";;")(["11";"22";"33"]) = "11;;22;;33"
*)

(* ****** ****** *)

#use "./../../assign2.ml";;
#use "./../../../../classlib/OCaml/MyOCaml.ml";;

let string_sepjoin_list (sep : string) (xs : string list) : string =
  let concat = List.fold_left (fun acc str -> acc ^ sep ^ str) "" xs in
  if string_length concat > string_length sep then
    String.sub concat (string_length sep) (string_length concat - string_length sep)
  else
    concat
;;

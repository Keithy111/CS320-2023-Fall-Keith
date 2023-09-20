(*
assign1-2: 10 points
Given two ordered strings cs1 and cs2, please use
string_make_fwork to implement string_merge(cs1, cs2)
which returns the order string obtained from merging
cs1 and cs2.

For instance, if cs1 = "135" and cs2 = "2468", then
string_merge(cs1)(cs2) equals "1234568"

For instance, if cs1 = "abcde" and cs2 = "1234", then
string_merge(cs1)(cs2) equals "1234abcde"
*)

(* ****** ****** *)


#use "./../assign0.ml";;
#use "./../MyOCaml.ml";;

let string_merge(cs1: string)(cs2: string): string =
  let merge_fwork work =
    string_foreach cs1 work;
    string_foreach cs2 work
  in

  string_make_fwork merge_fwork
;;


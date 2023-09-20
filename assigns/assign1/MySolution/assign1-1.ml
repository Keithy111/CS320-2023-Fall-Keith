(*
assign1-1: 10 points
Given a natural number n that is not a multiple
of 10, intrev10(n) is the natural number whose
representation in base 10 reverses that of the
number n.

fun intrev10(n: int): int

For instance, if n = 12345, then intrev10(n) is
54321; if n = 10203, then intrev10(n) is 30201.

Please give a TAIL-RECURSIVE implementation of
intrev10.
*)

#use "./../assign0.ml";;

let intrev10(n: int): int =
  let rec reverse_string s acc =
    match s with
    | "" -> acc
    | _ ->
      let len = String.length s in
      let last_char = s.[len - 1] in
      let rest = String.sub s 0 (len - 1) in
      reverse_string rest (acc ^ String.make 1 last_char)
  in
  let reversed_str = reverse_string (string_of_int n) "" in
  int_of_string reversed_str
;;
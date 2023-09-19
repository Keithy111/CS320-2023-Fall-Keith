(*
Assign0-5: 10 points
Please implement a function that returns the reverse of
a given string:
fun stringrev(cs: string): string
Note that you are not allowed to use string concatenation
or your solution is disqualified.
*)

#use "./../assign0.ml";;

let stringrev(cs: string): string =
  let len = string_length cs in
  (* calculates the length of the input string cs.*)

  let result = string_init len (fun i ->
    (*creates an empty string result of the same length as cs*)
    string_get cs (len - 1 - i)
  ) in

  result
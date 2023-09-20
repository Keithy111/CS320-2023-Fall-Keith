(*
Assign0-5: 10 points
Please implement a function that returns the reverse of
a given string:
fun stringrev(cs: string): string
Note that you are not allowed to use string concatenation
or your solution is disqualified.
*)

#use "./..MyOCaml.ml"

let stringrev (cs: string): string =
  let length = string_length cs in
  let result = string_init length (fun i ->
    string_get cs (length - 1 - i)
  ) in
  result
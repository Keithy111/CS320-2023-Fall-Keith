(*
Assign0-5: 10 points
Please implement a function that returns the reverse of
a given string:
fun stringrev(cs: string): string
Note that you are not allowed to use string concatenation
or your solution is disqualified.
*)

#use "./..assign0.ml"

let stringrev (cs: string): string =
  let length = String.length cs in
  let result = String.init length (fun i ->
    String.get cs (length - 1 - i)
  ) in
  result
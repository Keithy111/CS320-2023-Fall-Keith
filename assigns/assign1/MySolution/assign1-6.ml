(*
assign1-6: 20 bonus points
A 4-letter sequence abcd is 1324-like
if a < c < b < d holds. For instance, 1234 is
not 132-like; but 2547 is 1324-like.

A string is 1324-avoid if there is no subsequence
abc in this string that is 1324-like. Note that a
subsequence of a string may contain NON-CONSEQUTIVE
letters of the string.

For instance, 123456789 is 1324-avoid;
For instance, 987654321 is 1324-avoid;
For instance, 123465789 is not 132-avoid since the
subsequence 1657 is 1324-like.

Please implement a function string_avoid_1324 that
checks if a given string is 1324-avoid; the function
returns true if and only if the given string is 1324-
avoid.

fun string_avoid_1324(cs: string): bool
*)

(* ****** ****** *)


#use "./../MyOCaml.ml";;

let is_1324_like s i =
  if i >= string_length s - 3 then
    false
  else
    let a = ord s.[i] in
    let c = ord s.[i + 1] in
    let b = ord s.[i + 2] in
    let d = ord s.[i + 3] in
    a < c && c < b && b < d

(* Function to check if a string has a 1324-like subsequence *)
let rec has_1324_like_subsequence s i =
  if i >= string_length s - 3 then
    false
  else
    if is_1324_like s i then
      true
    else
      has_1324_like_subsequence s (i + 1)

(* Main function to check if a string is 1324-avoid *)
let string_avoid_1324 cs =
  not (has_1324_like_subsequence cs 0)
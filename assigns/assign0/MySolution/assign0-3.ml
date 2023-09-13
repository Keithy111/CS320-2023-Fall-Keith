(*
Assign0-3: 10 points
Please implement a function that converts a given
integer to a string that represents the integer:
fun int2str(i0: int): string
*)


let chr = Char.chr
(* Define a character-to-integer conversion function *)
let ord = Char.code
(*it converts a character to its corresponding integer code point in the Unicode character set*)
let string_init = String.init
(* Define a function for initializing a string *)
let str(c0) = String.make 1 c0
(*
   int2str: Takes an integer i0 and returns a string representation of i0.
*)
let int2str(i0: int): string =
  let rec int2str_helper i acc =
    if i = 0 then
      acc
    else
      let digit = chr(ord('0') + (i mod 10)) in
      let new_acc = str digit ^ acc in
      int2str_helper (i / 10) new_acc
  in

  (*handling the cases of zero and negative integers.*)
  if i0 = 0 then "0"
  else if i0 > 0 then int2str_helper i0 ""
  else "-" ^ int2str_helper (-i0) ""
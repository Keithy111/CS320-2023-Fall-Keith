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
(*function is used to generate the reversed string by fetching characters from cs in reverse order and placing them in result*)
let string_length = String.length
let string_get = String.get
(*used to retrieve the character at a specific index within a given string*)
let str(c0) = String.make 1 c0

(*
   int2str: Takes an integer i0 and returns a string representation of i0.
*)
let int2str i0 =
  let rec int_to_str_aux num acc =
    if num = 0 then
      if acc = "" then "0" else acc
    else
      let remainder = num mod 10 in
      let digit_char = chr (ord '0' + remainder) in
      let digit_str = str digit_char in
      let new_acc_length = String.length digit_str + String.length acc in
      let new_acc = string_init new_acc_length (fun i ->
        if i < String.length digit_str then
          string_get digit_str i
        else
          string_get acc (i - String.length digit_str)
      ) in
      int_to_str_aux (num / 10) new_acc
  in
  int_to_str_aux i0 ""
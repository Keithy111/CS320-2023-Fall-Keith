(*
Assign0-3: 10 points
Please implement a function that converts a given
integer to a string that represents the integer:
fun int2str(i0: int): string
*)


let chr = Char.chr
(* Define a character-to-integer conversion function *)

let string_init = String.init
(* Define a function for initializing a string *)


(*
   int2str: Takes an integer i0 and returns a string representation of i0.
*)
let int2str i0 =
  let rec count_digits i count =
    (* Helper function to count the number of digits in an integer *)
    if i < 10 then
      count + 1
    else
      count_digits (i / 10) (count + 1)
  in

  let num_digits = count_digits i0 0 in
  (* Calculate the number of digits in the input integer i0 *)


  let str = string_init num_digits (fun i ->
  (* Calculate the digit value at position i *)

    let digit_value = (i0 / int_of_float (10. ** float_of_int (num_digits - i - 1))) mod 10 in
    (* Convert the digit value to a character and add to the string *)
    
    char_of_int (48 + digit_value)
  ) in

  str








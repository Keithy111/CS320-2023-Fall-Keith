(*
Assign0-4: 10 points
Please implement a function that converts a given
string to an integer:
fun str2int(cs: string): int
In particular, it is expected that str2int(int2str(x)) = x
for natural numbers x.
(You can assume that the given string is a sequence of digits)
(And the empty sequence represents the integer 0)
*)

#use "./../assign0.ml";;


let str2int (cs: string): int =
  let len = string_length cs in

  (* Define a recursive helper function to convert the string to an integer *)
  let rec convert_to_int i result =
    (* Base case: If the current index is beyond the string length, return the accumulated result *)
    if i >= len then
      result
    else
      (* Extract the character at the current index, convert it to a digit value,
         and update the result by multiplying it by 10 and adding the digit value *)
      let digit_value = ord string_get (cs, (i - ord '0')) in
      let updated_result = result * 10 + digit_value in
      (* Recursive call to process the next character in the string *)
      convert_to_int (i + 1) updated_result
  in

  (* Check if the input string is empty (length is 0) *)
  if len = 0 then
    0 (* If empty, return 0 as specified in the prompt *)
  else
    (* Start the conversion process from the beginning of the string with an initial result of 0 *)
    convert_to_int 0 0
  ;;






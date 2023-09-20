(*
Assign0-3: 10 points
Please implement a function that converts a given
integer to a string that represents the integer:
fun int2str(i0: int): string
*)

#use "./../assign0.ml";;

let int2str i0 =
  (* Define a recursive helper function to convert the integer *)
  let rec int_to_str num acc =
    if num = 0 then
      if acc = "" then "0" else acc
    else
      (* Calculate the remainder when dividing by 10 to get the last digit *)
      let remainder = abs num mod 10 in
      (* Convert the digit to a character *)
      let digit_char = char_of_int (int_of_char '0' + remainder) in
      (* Convert the character back to a string *)
      let digit_str = String.make 1 digit_char in
      (* Calculate the length of the new accumulated string *)
      let new_acc_length = String.length digit_str + String.length acc in
      (* Create the new accumulated string by interleaving characters from digit_str and acc *)
      let new_acc = String.init new_acc_length (fun i ->
        if i < String.length digit_str then
          String.get digit_str i
        else
          String.get acc (i - String.length digit_str)
      ) in
      (* Recursively convert the remaining part of the number *)
      int_to_str (num / 10) new_acc
  in
  if i0 < 0 then
    String.init (String.length (int_to_str (abs i0) "" ) + 1) (fun i ->
      if i = 0 then '-'
      else String.get (int_to_str (abs i0) "") (i - 1)
    )
  else
    int_to_str i0 ""
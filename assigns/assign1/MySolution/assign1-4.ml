(*
assign1-4: 20 points
Given two strings ds1 and ds2 of digits, please
implement intrep_add(ds1)(ds2) that returns the
string representing the sum of the two integers
represented by ds1 and ds2. Note that the returned
string should contain no leading zeros. (And we
use the empty string to represent the number zero).

fun
intrep_add(ds1: string)(ds2: string): string

For instance, intrep_add("1116123")("222987") = "1339110"

Note that ds1 and ds2 can be arbitrarily long. Thus,
converting ds1 and ds2 to integers can cause overflow.
*)

(* ****** ****** *)

#use "./../assign0.ml";;

let intrep_add(ds1: string)(ds2: string): string =
  let len1 = String.length ds1 in
  let len2 = String.length ds2 in
  let max_len = max len1 len2 in

  let add_digits d1 d2 carry =
    let sum = digit_of_char d1 + digit_of_char d2 + carry in
    let new_digit = sum mod 10 in
    let new_carry = sum / 10 in
    (char_of_digit new_digit, new_carry)
  in

  let rec add_helper i carry result =
    if i < 0 then
      if carry > 0 then char_of_digit carry :: result
      else result
    else
      let d1 = if i < len1 then ds1.[i] else '0' in
      let d2 = if i < len2 then ds2.[i] else '0' in
      let (sum_digit, new_carry) = add_digits d1 d2 carry in
      add_helper (i - 1) new_carry (sum_digit :: result)
  in

  let result_chars = add_helper (max_len - 1) 0 [] in
  let result_string = String.of_seq (List.to_seq result_chars) in

  (* Remove leading zeros from the result *)
  let rec remove_leading_zeros s =
    match String.length s with
    | 0 -> "0"
    | 1 -> s
    | _ ->
        if s.[0] = '0' then
          remove_leading_zeros (String.sub s 1 (String.length s - 1))
        else s
  in

  remove_leading_zeros result_string
;;
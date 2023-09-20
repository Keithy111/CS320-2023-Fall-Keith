(*
assign1-5: 20 points

A sequence of chars is ascending if any char in
the sequence is less than or equal to the following
one (when the following one does exist).
Given a string cs, please implement a function
that find the longest ascending subsequence of [cs].
If there are more than one such sequences, the left
most one should be returned.

fun string_longest_ascend(xs: string): string

For instance, given "1324561111", the function
string_longest_ascend returns "13456"

For instance, given "1234561111", the function
string_longest_ascend returns "123456"

For instance, given "1234511111", the function
string_longest_ascend returns "111111".
*)

(* ****** ****** *)

let string_longest_ascend(xs: string): string =
  let len = String.length xs in
  if len <= 1 then xs (* Handle the case of empty string or single character string *)
  else begin
    let current = ref (String.sub xs 0 1) in
    let longest = ref (String.sub xs 0 1) in
    let prev_char = ref (String.get xs 0) in

    for i = 1 to len - 1 do
      let c = String.get xs i in
      if Char.code c >= Char.code !prev_char then
        current := !current ^ (String.make 1 c)
      else begin
        if String.length !current > String.length !longest then
          longest := !current;
        current := String.make 1 c;
      end;
      prev_char := c;
    done;

    if String.length !current > String.length !longest then
      longest := !current;

    !longest
  end
;;
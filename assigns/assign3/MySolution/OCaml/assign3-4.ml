
(*
Assign3-4:
HX-2023-09-26: 20 points
Given a word x of length n, another word is a buddy
of x if x and y differ exactly at one position. For
instance, "live" is a buddy "love" (and "love" is also
a buddy of "live").
//
Please give a NON-RECURSIVE implementation of
list_of_buddies that returns a list of all the buddies
of a given word.
//
let
list_of_buddies(word: string): string list = ...

(*
FYI. The concept of word buddies is used in the following game:
https://xanadu-lang.github.io/xats2js/docgen/CodeBook/Doublet/2020-11-29/
https://github.com/xanadu-lang/xats2js/tree/master/docgen/CodeBook/Doublet
*)
*)

#use "./../../../../classlib/OCaml/MyOCaml.ml";;

let list_of_buddies(word: string): string list =
  let n = string_length word in
  let buddies = ref [] in

  (* Helper function to generate buddy words by changing one character *)
  let generate_buddies i =
    for j = 0 to 25 do
      let new_char =
        let current_char = word.[i] in
        let offset = (j + ord 'a') - ord current_char in
        chr (ord current_char + offset)
      in
      if new_char <> word.[i] then begin
        let buddy = 
          string_append (string_append (string_sub word 0 i) new_char) (string_sub word (i + 1) (n - i - 1))
        in
        buddies := buddy :: !buddies
      end
    done
  in

  (* Iterate through the word and generate buddies *)
  for i = 0 to n - 1 do
    generate_buddies i
  done;

  !buddies
;;
#use "./../../../classlib/OCaml/MyOCaml.ml";;

(*

Please implement the interp function following the
specifications described in CS320_Fall_2023_Project-1.pdf

Notes:
1. You are only allowed to use library functions defined in MyOCaml.ml
   or ones you implement yourself.
2. You may NOT use OCaml standard library functions directly.

*)

type const =
  | Int of int
  | Bool of bool
  | Unit of unit

type coms =
  | Push of const
  | Pop
  | Trace
  | Add
  | Sub
  | Mul
  | Div
  | And
  | Or
  | Not
  | Lt
  | Gt

(* Helper function to convert a digit to a string *)
let rec dig_to_string(d : int) : string =
  let newDigit = char_of_digit d in
  str(newDigit)

(* Helper function to convert an integer to a string *)
let rec int_to_string(i : int) : string =
  let rec helper (num : int) (acc : string) =
    if num != 0 then
      let digitSelector = num mod 10 in
      let digitString = dig_to_string digitSelector in
      helper (num/10) (string_append digitString acc)
    else 
      acc
  in helper i ""

(* Convert a constant to a string representation *)
let toString(c : const) : string =
  match c with
  | Bool c -> if (c = true) then "True" else "False"
  | Unit c -> "Unit"
  | Int c -> int_to_string(c)
  
(* Helper function to parse an integer from a list of characters *)
let rec parse_int acc = function
  | c :: cs when c >= '0' && c <= '9' -> parse_int (10 * acc + (int_of_char c - int_of_char '0')) cs
  | cs -> acc

(* Helper function to extract the stack from a tuple *)
let extract_stack(x : const list * string list) : const list =
  match x with
  | (stack, _) -> stack

(* Helper function to extract the output from a tuple *)
let extractOutput(x : const list * string list) : string list =
  match x with
  | (_, string_list) -> string_list

(* Recursive function to convert a list of characters to a list of commands *)
let rec toProgram (char_list: char list)(program : coms list): coms list = 
  match char_list with
  | 'P' :: 'u' :: 's' :: 'h' :: rest -> 
    (match rest with
    | ' ' :: '-' :: x :: rest when x >= '0' && x <= '9' ->
       let n = (parse_int 0 (x :: rest)) * -1 in
       toProgram rest (Push(Int n) :: program)
    | ' ' :: x :: rest when x >= '0' && x <= '9' ->
       let n = parse_int 0 (x :: rest) in
       toProgram rest (Push(Int n) :: program)
    | ' ' :: 't' :: 'r' :: 'u' :: 'e' :: rest ->
      toProgram rest (Push(Bool true) :: program)
    | ' ' :: 'f' :: 'a' :: 'l' :: 's' :: 'e' :: rest ->
      toProgram rest (Push(Bool false) :: program)
    )
  | 'P' :: 'o' :: 'p' :: ';' :: rest -> toProgram rest (Pop :: program)
  | 'T' :: 'r' :: 'a' :: 'c' :: 'e' :: ';' :: rest -> toProgram rest (Trace :: program)
  | 'A' :: 'd' :: 'd' :: ';' :: rest -> toProgram rest (Add :: program)
  | 'S' :: 'u' :: 'b' :: ';' :: rest -> toProgram rest (Sub :: program)
  | 'M' :: 'u' :: 'l' :: ';' :: rest -> toProgram rest (Mul :: program)
  | 'D' :: 'i' :: 'v' :: ';' :: rest -> toProgram rest (Div :: program)
  | 'A' :: 'n' :: 'd' :: ';' :: rest -> toProgram rest (And :: program)
  | 'O' :: 'r' :: ';' :: rest -> toProgram rest (Or :: program)
  | 'N' :: 'o' :: 't' :: ';' :: rest -> toProgram rest (Not :: program)
  | 'L' :: 't' :: ';' :: rest -> toProgram rest (Lt :: program)
  | 'G' :: 't' :: ';' :: rest -> toProgram rest (Gt :: program)
  | x :: rest when x >= '0' && x <= '9' -> toProgram rest program
  | ' ' :: rest -> toProgram rest program
  | '\n' :: rest -> toProgram rest program
  | '\t' :: rest -> toProgram rest program
  | '\r' :: rest -> toProgram rest program
  | ';' :: rest -> toProgram rest program
  | _ -> program

(* Helper function to push a constant onto the stack *)
let push(cons : const)(stack : const list) : const list = cons :: stack

(* Helper function to pop an element from the stack *)
let pop(stack : const list)(output : string list) : const list * string list =
  match stack with
  | c :: rest -> (rest, output)
  | [] -> ([], "Panic" :: output)

(* Helper function to trace the top element of the stack *)
let trace(stack : const list)(output : string list): const list * string list = 
  match stack with
  | x :: xs -> 
    let string_element = toString(x) in
    let stack = extract_stack(pop(stack)(output)) in
    let stack = push(Unit ())(stack) in
    let output = string_element :: output in
    (stack, output)
  | [] -> ([], "Panic" :: output)

(* Helper function to perform numeric operations on the stack *)
let numOperator(stack : const list)(operation: coms list)(output : string list) : const list * string list =   
  match stack with
  | Int i :: Int j :: rest ->
    let result = 
      match operation with
      | Add :: _ -> Int (i + j)
      | Sub :: _ -> Int (i - j)
      | Mul :: _ ->  Int (i * j)
      | Div :: _ when j != 0 -> Int(j / i)
      | Div :: _ when j=0 -> Int(-11111111)
      | Lt :: _ -> Bool(i < j)
      | Gt :: _ -> Bool(i > j)
      | _ -> failwith "Invalid operation" 
    in
    if (result = Int(-1111)) then 
      ([], "Panic" :: output) 
    else
      let stack = extract_stack(pop (stack)(output)) in
      let stack = extract_stack(pop (stack)(output)) in
      let stack = push(result)(stack) in
      (stack, output)

  | Bool _ :: _ :: rest -> ([], "Panic" :: output)
  | _ :: Bool _ :: rest -> ([], "Panic" :: output)
  | Unit _ :: _ :: rest -> ([], "Panic" :: output)
  | _ :: Unit _ :: rest -> ([], "Panic" :: output)
  | _ -> ([], "Panic" :: output)

(* Helper function to perform boolean operations on the stack *)
let boolOperator (stack : const list)(operation: coms list)(output : string list) : const list * string list = 
  match stack with
  | Bool i :: Bool j :: rest ->
    let com =
      match operation with
      | And :: rest -> Bool (i && j)
      | Or :: rest -> Bool (i || j)
    in

    let stack = extract_stack(pop (stack)(output)) in
    let stack = extract_stack(pop (stack)(output)) in
    let stack = push(com)(stack) in
    (stack, output)

  | (Int _ | Unit _) :: (Int _ | Unit _) :: rest -> ([], "Panic" :: output)
  | [] -> ([], "Panic" :: output)

(* Helper function to perform the NOT operation on the stack *)
let notOperator(stack : const list)(output : string list) : const list * string list =
    match stack with
    | Bool x :: rest ->
      let com = Bool (not x) in
      let stack = extract_stack(pop (stack)(output)) in
      let stack = push (com)(stack) in
      (stack, output)
    | _ :: rest -> ([], "Panic" :: output)
    | [] -> ([], "Panic" :: output)

(* Main function to interpret the input string *)
let interp (s: string) : string list option =
  let empty : coms list = [] in
  let stack : const list = [] in
  let output : string list = [] in
  let new_pro = list_reverse (toProgram (string_listize s) empty) in
  let rec helper (curr : coms list) (stack_output : const list * string list) : string list option =
    if (extract_stack stack_output) = [] && (extractOutput stack_output) = ["Panic"] then
      Some (extractOutput stack_output)
    else
      match curr with
      | command :: rest ->
        (begin
          match command with
          | Push constant -> helper rest (push constant (extract_stack stack_output), extractOutput stack_output)
          | Pop -> helper rest (pop (extract_stack stack_output) (extractOutput stack_output))
          | Trace -> helper rest (trace (extract_stack stack_output) (extractOutput stack_output))
          | Add | Sub | Mul | Div | Lt | Gt -> helper rest (numOperator (extract_stack stack_output) curr (extractOutput stack_output))
          | And | Or -> helper rest (boolOperator (extract_stack stack_output) curr (extractOutput stack_output))
          | Not -> helper rest (notOperator (extract_stack stack_output) (extractOutput stack_output))
        end)
      | [] -> Some (extractOutput stack_output)
  in helper new_pro (stack, output)

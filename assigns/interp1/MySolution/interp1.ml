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
  | Unit 

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

(* Convert a constant to a string representation *)
let constant_to_string = function
| Int i -> string_of_int i
| Bool b -> if b then "True" else "False"
| Unit -> "Unit"

let is_int = function
  | Int _ -> true
  | _ -> false

let is_bool = function
  | Bool _ -> true
  | _ -> false

(* Helper function to convert a list of characters back into a string *)
let rec string_of_char_list char_list =
   let rec aux chars acc =
     match chars with
     | [] -> acc
     | c :: cs -> aux cs (acc ^ str c)
   in aux char_list ""

(*remove the negative sign of a string*)
let rem_negative s =
  let char_list = string_listize s in
  match char_list with
  | '-' :: rest -> (true, string_of_char_list rest) 
  | _ -> (false, s) 

let parse_constant s = 
  match s with
  | "True" -> Some (Bool true)
  | "False" -> Some (Bool false)
  | "Unit" -> Some Unit
  | _ ->
      let (is_negative, digits) = rem_negative s in
      let rec parse_digits chars total =
        match chars with
        | [] -> Some total
        | c :: cs -> 
            if char_isdigit c then
              let digit = digit_of_char c in
              parse_digits cs (10 * total + digit)
            else
              None
      in
      match parse_digits (string_listize digits) 0 with
      | Some n -> Some (Int (if is_negative then -n else n))
      | None -> None

(* Function to parse a string into a command option *)
let parse_command s =
  match s with
  | "Pop" -> Some Pop
  | "Trace" -> Some Trace
  | "Add" -> Some Add
  | "Sub" -> Some Sub
  | "Mul" -> Some Mul
  | "Div" -> Some Div
  | "And" -> Some And
  | "Or" -> Some Or
  | "Not" -> Some Not
  | "Lt" -> Some Lt
  | "Gt" -> Some Gt
  | "" -> None
  | _ -> None

(*helper function that breaks down into individual commands based on semicolons*)
let separator (s : string) : string list =
  let rec aux (cs : char list) (current : string) (acc : string list) : string list =
    match cs with
    | [] -> 
        if current = "" then acc else current :: acc 
    | ';' :: rest -> 
        aux rest "" (current :: acc) 
    | c :: rest ->
        if char_iswhitespace c then
          if current = "" then
            aux rest current acc 
          else
            aux rest "" (current :: acc)
        else
          aux rest (current ^ str c) acc 
  in 
  list_reverse (aux (string_listize s) "" []) 

let parse_program s =
  let tokens = separator s in
  let rec parse tokens =
    match tokens with
    | [] -> Some []
    | "" :: rest -> parse rest
    | "Push" :: value :: rest -> 
        (match parse_constant value with
        | Some const -> (match parse rest with
                         | Some cmds -> Some (Push const :: cmds)
                         | None -> None)
        | None -> None)
    | "Push" :: [] -> None
    | "Pop" :: rest -> 
        (match parse rest with
        | Some cmds -> Some (Pop :: cmds)
        | None -> None)
    | "Trace" :: rest -> 
        (match parse rest with
        | Some cmds -> Some (Trace :: cmds)
        | None -> None)
    | "Add" :: rest ->
         (match parse rest with
         | Some cmds -> Some (Add :: cmds)
         | None -> None)
    | "Sub" :: rest ->
         (match parse rest with
         | Some cmds -> Some (Sub :: cmds)
         | None -> None)
    | "Mul" :: rest ->
         (match parse rest with
         | Some cmds -> Some (Mul :: cmds)
         | None -> None)
    | "Div" :: rest ->
         (match parse rest with
         | Some cmds -> Some (Div :: cmds)
         | None -> None)
    | "And" :: rest ->
         (match parse rest with
         | Some cmds -> Some (And :: cmds)
         | None -> None)
    | "Or" :: rest ->
         (match parse rest with
         | Some cmds -> Some (Or :: cmds)
         | None -> None)
    | "Not" :: rest ->
         (match parse rest with
         | Some cmds -> Some (Not :: cmds)
         | None -> None)
    | "Lt" :: rest ->
         (match parse rest with
         | Some cmds -> Some (Lt :: cmds)
         | None -> None)
    | "Gt" :: rest ->
         (match parse rest with
         | Some cmds -> Some (Gt :: cmds)
         | None -> None)
    | _ -> None
  in
  parse tokens

let handle_binary_op op stack trace =
  match stack with
  | Int j :: Int i :: rest -> Some ((Int (op i j)) :: rest, trace)
  | _ -> Some ([], "Panic" :: trace)

let handle_unary_op op stack trace =
  match stack with
  | Bool i :: Bool j :: rest -> Some ((Bool (op i j)) :: rest, trace)
  | _ -> Some ([], "Panic" :: trace)

let handle_comparison_op op stack trace =
  match stack with
  | Int j :: Int i :: rest -> Some ((Bool (op i j)) :: rest, trace)
  | _ -> Some ([], "Panic" :: trace)

let eval_command cmd (stack, trace) =
  match cmd with
  | Push c -> Some (c :: stack, trace)
  | Pop ->
    (match stack with
    | [] -> Some ([], "Panic" :: trace)
    | _ :: rest -> Some (rest, trace))
  | Trace ->
    (match stack with
    | [] -> Some ([Unit], "Panic" :: trace)
    | c :: rest -> Some (Unit :: rest, (constant_to_string c) :: trace))
  | Add -> handle_binary_op (+) stack trace
  | Sub -> handle_binary_op (-) stack trace
  | Mul -> handle_binary_op ( * ) stack trace
  | Div ->
    (match stack with
    | Int j :: Int i :: rest ->
      if j = 0 then Some ([], "Panic" :: trace)
      else Some ((Int (i / j)) :: rest, trace)
    | _ -> Some ([], "Panic" :: trace))
  | And -> handle_unary_op (&&) stack trace
  | Or -> handle_unary_op (||) stack trace
  | Not ->
    (match stack with
    | Bool i :: rest -> Some ((Bool (not i)) :: rest, trace)
    | _ -> Some ([], "Panic" :: trace))
  | Lt -> handle_comparison_op (<) stack trace
  | Gt -> handle_comparison_op (>) stack trace

let rec contains_panic trace = 
  match trace with
  | [] -> false
  | x :: xs -> if x = "Panic" then true else contains_panic xs

let rec eval_commands cmds state =
  match cmds with
  | [] -> Some state
  | cmd :: rest ->
      match eval_command cmd state with
      | Some (new_stack, new_trace) -> 
          if contains_panic new_trace then
            Some (new_stack, new_trace)
          else
            eval_commands rest (new_stack, new_trace)
      | None -> None

let interp (s : string) : string list option =
  match parse_program s with
  | None -> None 
  | Some cmds -> 
      let initial_state = ([], []) in
      match eval_commands cmds initial_state with
      | Some (_, trace) -> Some (list_reverse trace) 
      | None -> Some ["Panic"]
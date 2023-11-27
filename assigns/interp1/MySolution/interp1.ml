#use "./../../../classlib/OCaml/MyOCaml.ml";;

(*

Please implement the interp function following the
specifications described in CS320_Fall_2023_Project-1.pdf

Notes:
1. You are only allowed to use library functions defined in MyOCaml.ml
   or ones you implement yourself.
2. You may NOT use OCaml standard library functions directly.

*)
type config = {
  stack: value list;
  trace: string list;
  program: command list;
}

and command =
  | Push of int
  | Add
  | Sub
  | Mul
  | Div
  | Rem
  | Neg
  | Swap
  | Pop
  | Quit
  | True
  | False
  | Not
  | Equal
  | Lt
  | Gt

and value =
  | Int of int
  | Bool of bool

let rec parse_prog (tokens: string list) : command list option =
  let p = many' parse_command in
  match string_parse p (string_concat_list tokens) with
  | Some (cmds, _) -> Some cmds
  | None -> None

and parse_command () : command parser =
  (keyword "push" >> whitespaces1 >> natural >|= fun n -> Push n)
  <|> (keyword "add" >| Add)
  <|> (keyword "sub" >| Sub)
  <|> (keyword "mul" >| Mul)
  <|> (keyword "div" >| Div)
  <|> (keyword "rem" >| Rem)
  <|> (keyword "neg" >| Neg)
  <|> (keyword "swap" >| Swap)
  <|> (keyword "pop" >| Pop)
  <|> (keyword "quit" >| Quit)
  <|> (keyword "true" >| True)
  <|> (keyword "false" >| False)
  <|> (keyword "not" >| Not)
  <|> (keyword "equal" >| Equal)
  <|> (keyword "lt" >| Lt)
  <|> (keyword "gt" >| Gt)

let initial_config (commands: command list) : config =
  { stack = []; trace = []; program = commands }

let interp_step (config: config) : config option =
  match config.program, config.stack with
  | [], _ -> Some { config with trace = "Success" :: config.trace; stack = []; program = [] }
  | Quit :: _, _ -> Some { config with trace = "Success" :: config.trace; stack = []; program = [] }
  | Push n :: rest, _ -> interp_step { config with stack = Int n :: config.stack; program = rest }
  | Pop :: rest, Int _ :: stack' -> interp_step { config with stack = stack'; program = rest }
  | Pop :: _, _ -> Some { config with trace = "Panic" :: config.trace; stack = []; program = [] }
  | Add :: rest, Int y :: Int x :: stack' -> interp_step { config with stack = Int (x + y) :: stack'; program = rest }
  | Add :: _, Int _ :: _ -> Some { config with trace = "Panic" :: config.trace; stack = []; program = [] }
  | Add :: _, _ -> Some { config with trace = "Panic" :: config.trace; stack = []; program = [] }

  | _ :: _, [] -> Some { config with trace = "Panic" :: config.trace; stack = []; program = [] }
  | _ -> Some { config with trace = "Panic" :: config.trace; stack = []; program = [] }


let interp (s : string) : string list option = 
   let tokens = String.split_on_char ' ' program in
   match parse_prog tokens with
   | Some commands ->
     let initial_configuration = initial_config commands in
     (match interp_step initial_configuration with
      | Some final_configuration ->
        Some (final_configuration.trace |> foreach_to_listize(string_foreach))
      | None -> None)
   | None -> None
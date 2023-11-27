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

type com =
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

type coms = com list

type config = {
  stack: const list;
  trace: string list;
  program: coms;
}

let initial_config program = { stack = []; trace = []; program }

let rec interp_step (config: config) : config option =
  match config.program with
  | [] -> Some config
  | command :: rest ->
    match command with
    | Push c -> interp_step { config with stack = c :: config.stack; program = rest }
    | Pop ->
      (match config.stack with
       | [] -> Some { config with trace = "Panic" :: config.trace; stack = []; program = [] } (* PopError *)
       | _ :: stack' -> interp_step { config with stack = stack'; program = rest })
    | Trace ->
      (match config.stack with
       | [] -> Some { config with trace = "Panic" :: config.trace; stack = []; program = [] } (* TraceError *)
       | c :: stack' ->
         let trace_entry = toString c in
         interp_step { config with stack = Unit :: stack'; trace = trace_entry :: config.trace; program = rest })
    | Add ->
      (match config.stack with
       | Int i :: Int j :: stack' -> interp_step { config with stack = Int (i + j) :: stack'; program = rest }
       | Int _ :: _ :: _ -> Some { config with trace = "Panic" :: config.trace; stack = []; program = [] } (* AddError1 *)
       | _ :: _ :: _ -> Some { config with trace = "Panic" :: config.trace; stack = []; program = [] } (* AddError1 *)
       | _ :: stack' -> Some { config with trace = "Panic" :: config.trace; stack = []; program = [] } (* AddError2 *)
       | _ -> Some { config with trace = "Panic" :: config.trace; stack = []; program = [] } (* AddError3 *)
      )
    | Sub ->
      (match config.stack with
       | Int i :: Int j :: stack' -> interp_step { config with stack = Int (i - j) :: stack'; program = rest }
       | Int _ :: _ :: _ -> Some { config with trace = "Panic" :: config.trace; stack = []; program = [] } (* SubError1 *)
       | _ :: _ :: _ -> Some { config with trace = "Panic" :: config.trace; stack = []; program = [] } (* SubError1 *)
       | _ :: stack' -> Some { config with trace = "Panic" :: config.trace; stack = []; program = [] } (* SubError2 *)
       | _ -> Some { config with trace = "Panic" :: config.trace; stack = []; program = [] } (* SubError3 *)
      )
    | Mul ->
      (match config.stack with
       | Int i :: Int j :: stack' -> interp_step { config with stack = Int (i * j) :: stack'; program = rest }
       | Int _ :: _ :: _ -> Some { config with trace = "Panic" :: config.trace; stack = []; program = [] } (* MulError1 *)
       | _ :: _ :: _ -> Some { config with trace = "Panic" :: config.trace; stack = []; program = [] } (* MulError1 *)
       | _ :: stack' -> Some { config with trace = "Panic" :: config.trace; stack = []; program = [] } (* MulError2 *)
       | _ -> Some { config with trace = "Panic" :: config.trace; stack = []; program = [] } (* MulError3 *)
      )
    | Div ->
      (match config.stack with
       | Int i :: Int j :: stack' ->
         if j = 0 then Some { config with trace = "Panic" :: config.trace; stack = []; program = [] } (* DivError0 *)
         else interp_step { config with stack = Int (i / j) :: stack'; program = rest }
       | Int _ :: _ :: _ -> Some { config with trace = "Panic" :: config.trace; stack = []; program = [] } (* DivError1 *)
       | _ :: _ :: _ -> Some { config with trace = "Panic" :: config.trace; stack = []; program = [] } (* DivError1 *)
       | _ :: stack' -> Some { config with trace = "Panic" :: config.trace; stack = []; program = [] } (* DivError2 *)
       | _ -> Some { config with trace = "Panic" :: config.trace; stack = []; program = [] } (* DivError3 *)
      )
    | And ->
      (match config.stack with
       | Bool a :: Bool b :: stack' -> interp_step { config with stack = Bool (a && b) :: stack'; program = rest }
       | Bool _ :: _ :: _ -> Some { config with trace = "Panic" :: config.trace; stack = []; program = [] } (* AndError1 *)
       | _ :: _ :: _ -> Some { config with trace = "Panic" :: config.trace; stack = []; program = [] } (* AndError1 *)
       | _ :: stack' -> Some { config with trace = "Panic" :: config.trace; stack = []; program = [] } (* AndError2 *)
       | _ -> Some { config with trace = "Panic" :: config.trace; stack = []; program = [] } (* AndError3 *)
      )
    | Or ->
      (match config.stack with
       | Bool a :: Bool b :: stack' -> interp_step { config with stack = Bool (a || b) :: stack'; program = rest }
       | Bool _ :: _ :: _ -> Some { config with trace = "Panic" :: config.trace; stack = []; program = [] } (* OrError1 *)
       | _ :: _ :: _ -> Some { config with trace = "Panic" :: config.trace; stack = []; program = [] } (* OrError1 *)
       | _ :: stack' -> Some { config with trace = "Panic" :: config.trace; stack = []; program = [] } (* OrError2 *)
       | _ -> Some { config with trace = "Panic" :: config.trace; stack = []; program = [] } (* OrError3 *)
      )
    | Not ->
      (match config.stack with
       | Bool a :: stack' -> interp_step { config with stack = Bool (not a) :: stack'; program = rest }
       | Bool _ :: _ -> Some { config with trace = "Panic" :: config.trace; stack = []; program = [] } (* NotError1 *)
       | _ :: stack' -> Some { config with trace = "Panic" :: config.trace; stack = []; program = [] } (* NotError2 *)
       | _ -> Some { config with trace = "Panic" :: config.trace; stack = []; program = [] } (* NotError3 *)
      )
    | Lt ->
      (match config.stack with
       | Int x :: Int y :: stack' -> interp_step { config with stack = Bool (y < x) :: stack'; program = rest }
       | Int _ :: _ :: _ -> Some { config with trace = "Panic" :: config.trace; stack = []; program = [] } (* LtError1 *)
       | _ :: _ :: _ -> Some { config with trace = "Panic" :: config.trace; stack = []; program = [] } (* LtError1 *)
       | _ :: stack' -> Some { config with trace = "Panic" :: config.trace; stack = []; program = [] } (* LtError2 *)
       | _ -> Some { config with trace = "Panic" :: config.trace; stack = []; program = [] } (* LtError3 *)
      )
    | Gt ->
      (match config.stack with
       | Int x :: Int y :: stack' -> interp_step { config with stack = Bool (y > x) :: stack'; program = rest }
       | Int _ :: _ :: _ -> Some { config with trace = "Panic" :: config.trace; stack = []; program = [] } (* GtError1 *)
       | _ :: _ :: _ -> Some { config with trace = "Panic" :: config.trace; stack = []; program = [] } (* GtError1 *)
       | _ :: stack' -> Some { config with trace = "Panic" :: config.trace; stack = []; program = [] } (* GtError2 *)
       | _ -> Some { config with trace = "Panic" :: config.trace; stack = []; program = [] } (* GtError3 *)
      )

   let interp (s : string) : string list option = 
      let tokens = String.split_on_char ' ' s in
      match parse_prog tokens with
      | Some commands ->
        let initial_configuration = initial_config commands in
        (match interp_step initial_configuration with
        | Some final_configuration ->
          Some (final_configuration.trace |> foreach_to_listize string_foreach)
        | None -> None)
      | None -> None
    
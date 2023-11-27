#use "./interp1.ml";;

(* Function to read the entire stdin as a string *)
let rec read_input acc =
  try
    let line = input_line stdin in
    read_input (acc ^ line ^ "\n")
  with
  | End_of_file -> acc

(* Main program *)
let () =
  (* Read the entire stdin as a string *)
  let input_string = read_input "" in

  (* Print the input string with a separator *)
  print_endline "----------------------------------------";
  print_endline "Input Program:";
  print_string input_string;
  print_endline "----------------------------------------";

  (* Call interp function *)
  let result = interp input_string in

  (* Print the result to stdout *)
  match result with
  | Some output -> List.iter (fun s -> print_endline s) output
  | None ->
    print_endline "Syntax error!"
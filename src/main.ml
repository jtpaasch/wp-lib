open !Core_kernel
open Bap.Std

let run filepath1 filepath2 func property =
  let proj1 = Loader.load filepath1 in
  let prog1 = Project.program proj1 in
  let proj2 = Loader.load filepath2 in
  let prog2 = Project.program proj2 in

  let status = Verifier.check prog1 prog2 func property in

  match status with
  | Z3.Solver.UNSATISFIABLE ->
    print_endline "UNSAT"
  | Z3.Solver.SATISFIABLE ->
    print_endline "SAT"
  | Z3.Solver.UNKNOWN ->
    print_endline "Unknown"

let get_arg_or_exit ~arg ~error =
  if (Array.length Sys.argv) < (arg + 1) then
    begin
      print_endline error;
      exit 1
    end
  else Sys.argv.(arg)

let () =
  let error = "Error: must specify path to two executables" in
  let filepath1 = get_arg_or_exit ~arg:1 ~error in 
  let filepath2 = get_arg_or_exit ~arg:2 ~error in
  let error2 = "Error: must specify a function name" in
  let func = get_arg_or_exit ~arg:3 ~error:error2 in
  let error3 = "Error: must specify an s-exp" in
  let property_raw = get_arg_or_exit ~arg:4 ~error:error3 in
  let property = Sexp.of_string property_raw in
  run filepath1 filepath2 func property

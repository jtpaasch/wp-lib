open !Core_kernel
open Bap.Std

let _ = Bap_main.init ()

let loader = "llvm"

let load (filename : string) : Project.t =
  let input = Project.Input.file ~loader ~filename in
  match Project.create input ~package:filename with
  | Ok proj -> proj
  | Error e ->
    begin
      let msg = Printf.sprintf "Load error: %s" (Error.to_string_hum e) in
      failwith msg
    end

open !Core_kernel
open Bap.Std
open Bap_wp

let check (orig_prog : Program.t) (patch_prog : Program.t)
    (func : string) (property : Sexp.t) : Z3.Solver.status =

  let get_sub prog name =
    let subs = Term.enum sub_t prog in
    Seq.find ~f:(fun s -> String.equal (Sub.name s) name) subs
  in

  let orig_sub = get_sub orig_prog func in
  let patch_sub = get_sub patch_prog func in

  let orig_func = Option.value_exn orig_sub in
  let patch_func = Option.value_exn patch_sub in

  let z3_ctx = Environment.mk_ctx () in
  let var_gen = Environment.mk_var_gen () in

  let env_1 = Precondition.mk_env z3_ctx var_gen in
  let env_2 = Precondition.mk_env z3_ctx var_gen in
  let env_2 = Environment.set_freshen env_2 true in

  let vars_1 = Precondition.get_vars env_1 orig_func in
  let vars_2 = Precondition.get_vars env_2 patch_func in
  let _, env_1 = Precondition.init_vars vars_1 env_1 in
  let _, env_2 = Precondition.init_vars vars_2 env_2 in

  let smtlib_hyp = "" in
  let smtlib_post = Sexp.to_string property in
  let postconds, hyps =
    Compare.compare_subs_smtlib ~smtlib_hyp ~smtlib_post in

  let precond, _env_1, _env_2 = Compare.compare_subs
      ~postconds:[postconds] ~hyps:[hyps]
      ~original:(orig_func, env_1) ~modified:(patch_func, env_2) in

  let solver = Z3.Solver.mk_solver z3_ctx None in
  Precondition.check solver z3_ctx precond

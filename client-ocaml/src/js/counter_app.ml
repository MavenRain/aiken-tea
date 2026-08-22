(* Bind the pure counter app to its compiled validator (from
   plutus.json) and to its exported update program (from
   uplc/counter-update.json). *)

open Tea_pure

let app ~(compiled_code : string) ~(exported_update : string) :
    (Counter.model, Counter.msg) Tea.app =
  let program = Uplc.of_compiled_code ~compiled_code:exported_update in
  {
    (* The counter never halts: every message steps to a next model. *)
    Tea.update = (fun msg model -> Some (Counter.update msg model));
    model_to_data = Counter.model_to_data;
    model_of_data = Counter.model_of_data;
    msg_to_data = Counter.msg_to_data;
    (* counter.update returns the bare model on-chain; wrap it in the
       Some verdict the runtime compares against. *)
    uplc_step =
      Some
        (fun ~msg ~model ->
          Result.map
            (fun next -> "d8799f" ^ next ^ "ff")
            (Uplc.step program ~msg ~model));
    finish = (fun (_ : Counter.msg) builder -> builder);
    validator = Lucid.plutus_v3_validator compiled_code;
  }

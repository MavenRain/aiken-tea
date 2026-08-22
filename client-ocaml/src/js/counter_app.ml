(* Bind the pure counter app to its compiled validator (from
   plutus.json). *)

open Tea_pure

let app (compiled_code : string) : (Counter.model, Counter.msg) Tea.app =
  {
    (* The counter never halts: every message steps to a next model. *)
    Tea.update = (fun msg model -> Some (Counter.update msg model));
    model_to_data = Counter.model_to_data;
    model_of_data = Counter.model_of_data;
    msg_to_data = Counter.msg_to_data;
    finish = (fun (_ : Counter.msg) builder -> builder);
    validator = Lucid.plutus_v3_validator compiled_code;
  }

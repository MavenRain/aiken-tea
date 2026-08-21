(* Bind the pure counter app to its compiled validator (from
   plutus.json). *)

open Tea_pure

let app (compiled_code : string) : (Counter.model, Counter.msg) Tea.app =
  {
    Tea.update = Counter.update;
    model_to_data = Counter.model_to_data;
    model_of_data = Counter.model_of_data;
    msg_to_data = Counter.msg_to_data;
    finish = Fun.id;
    validator = Lucid.plutus_v3_validator compiled_code;
  }

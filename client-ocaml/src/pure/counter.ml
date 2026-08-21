(* Counter: the client half of lib/tea/counter.ak. `update` mirrors the
   Aiken step function; the emulator suite proves the mirror faithful,
   because the validator recomputes the step and rejects any divergence.
   Data layout matches the Aiken side: the model is Constr 0 [count],
   the message enum is Constr i [] in declaration order. *)

type model = { count : int }

type msg =
  | Increment
  | Decrement
  | Reset

(* The pure TEA step function, mirroring `counter.update` in Aiken. *)
let update msg model =
  match msg with
  | Increment -> { count = model.count + 1 }
  | Decrement -> { count = model.count - 1 }
  | Reset -> { count = 0 }

let model_to_data model = Tea_data.encode (Constr (0, [ Int model.count ]))

let model_of_data text =
  Result.bind
    (Result.map_error Tea_data.error_to_string (Tea_data.decode text))
    (fun data ->
      match data with
      | Tea_data.Constr (0, [ Tea_data.Int count ]) -> Ok { count }
      | Tea_data.Int _ | Tea_data.Constr (_, _) ->
        Error ("not a counter model: " ^ Tea_data.to_string data))

let msg_index msg =
  match msg with
  | Increment -> 0
  | Decrement -> 1
  | Reset -> 2

let msg_to_data msg = Tea_data.encode (Constr (msg_index msg, []))

let msg_to_string msg =
  match msg with
  | Increment -> "Increment"
  | Decrement -> "Decrement"
  | Reset -> "Reset"

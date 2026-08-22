(* Pure encodings for the batched-dispatch path (step 8), mirroring
   `tea.Action`, `tea.Queued` and `queue.Msg` in Aiken. CBOR composes,
   so wrapping an already-encoded message hex inside a constructor
   reproduces exactly what [Tea_data.encode] would emit for the same
   value. *)

(* Action: Single(msg) wraps the bare message as constructor 0. *)
let single_redeemer msg_hex = "d8799f" ^ msg_hex ^ "ff"

(* Action: Batch { queue } names the queue script hash, constructor 1. *)
let batch_redeemer ~queue_hash =
  Result.fold
    ~ok:(fun raw -> Ok (Tea_data.encode (Constr (1, [ Bytes raw ]))))
    ~error:(fun error -> Error (Tea_data.error_to_string error))
    (Tea_data.string_of_hex queue_hash)

(* queue.Msg: Process | Reclaim, field-free constructors. *)
let process_redeemer = Tea_data.encode (Constr (0, []))
let reclaim_redeemer = Tea_data.encode (Constr (1, []))

(* tea.Queued { author, msg }: the entry's inline datum. [msg_hex] is
   the application message already encoded as Data. *)
let queued_datum ~author_hash ~msg_hex =
  Result.fold
    ~ok:(fun author -> Ok ("d8799f" ^ Tea_data.encode (Bytes author) ^ msg_hex))
    ~error:(fun error -> Error (Tea_data.error_to_string error))
    (Tea_data.string_of_hex author_hash)
  |> Result.map (fun opening -> opening ^ "ff")

(* The message payload of a Queued datum, re-encoded on its own. *)
let queued_msg_hex datum_hex =
  Result.bind
    (Result.map_error Tea_data.error_to_string (Tea_data.decode datum_hex))
    (fun data ->
      match data with
      | Tea_data.Constr (0, [ Tea_data.Bytes _; msg ]) ->
        Ok (Tea_data.encode msg)
      | Tea_data.Int _ | Tea_data.Bytes _ | Tea_data.Constr (_, _) ->
        Error ("not a queued entry: " ^ Tea_data.to_string data))

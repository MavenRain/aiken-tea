(* Registry: the client half of lib/tea/registry.ak. The model is the
   deployed frontend (IPFS cid, version counter, bundle hash); the one
   message publishes a new bundle and bumps the version. The emulator
   suite proves the mirror faithful, because the validator recomputes
   this step on-chain and rejects any divergence. Data layout matches
   the Aiken side: the model is Constr 0 [cid, version, frontend_hash],
   the message is Constr 0 [cid, frontend_hash]. *)

type model = { cid : string; version : int; frontend_hash : string }

type msg = Publish of { cid : string; frontend_hash : string }

(* CIP-67 label (100) prefix + "aiken-tea", as hex: the name of the
   reference NFT marking the state UTxO. *)
let reference_token_name_hex = "000643b061696b656e2d746561"

(* The pure TEA step function, mirroring `registry.update` in Aiken. *)
let update msg model =
  match msg with
  | Publish { cid; frontend_hash } ->
    { cid; version = model.version + 1; frontend_hash }

(* Mirrors `registry.well_formed`: non-empty cid, 32-byte hash. *)
let well_formed cid frontend_hash =
  String.length cid > 0 && String.length frontend_hash = 32

let model_to_data model =
  Tea_data.encode
    (Constr
       (0, [ Bytes model.cid; Int model.version; Bytes model.frontend_hash ]))

let model_of_data text =
  Result.bind
    (Result.map_error Tea_data.error_to_string (Tea_data.decode text))
    (fun data ->
      match data with
      | Tea_data.Constr
          ( 0,
            [ Tea_data.Bytes cid;
              Tea_data.Int version;
              Tea_data.Bytes frontend_hash
            ] ) -> Ok { cid; version; frontend_hash }
      | Tea_data.Int _ | Tea_data.Bytes _ | Tea_data.Constr (_, _) ->
        Error ("not a registry model: " ^ Tea_data.to_string data))

let msg_to_data msg =
  match msg with
  | Publish { cid; frontend_hash } ->
    Tea_data.encode (Constr (0, [ Bytes cid; Bytes frontend_hash ]))

let msg_to_string msg =
  match msg with
  | Publish { cid; _ } -> "Publish " ^ cid

(* Bind the pure registry app to the two-purpose validator from
   plutus.json: apply the (owner, seed) parameters, derive the policy
   id, and decorate every dispatch with the owner's required
   signature. [genesis] is the one-shot mint that creates the
   reference NFT and the version-0 state UTxO. *)

open Js_of_ocaml
open Tea_pure

let void_redeemer = Tea_data.encode (Constr (0, []))

let reference_unit ~(policy_id : string) : string =
  policy_id ^ Registry.reference_token_name_hex

(* The validator's parameters as Plutus Data: the owner key hash and
   the seed OutputReference (Constr 0 [transaction_id, output_index]). *)
let params ~(owner_hash : string) ~(seed_tx_hash : string)
    ~(seed_index : int) : (string list, string) result =
  Result.map_error Tea_data.error_to_string
    (Result.bind (Tea_data.string_of_hex owner_hash) (fun owner ->
       Result.map
         (fun tx_hash ->
           [ Tea_data.encode (Bytes owner);
             Tea_data.encode (Constr (0, [ Bytes tx_hash; Int seed_index ]))
           ])
         (Tea_data.string_of_hex seed_tx_hash)))

(* The compiled validator with (owner, seed) applied, plus its policy
   id (= script hash: the mint and spend halves are one script). *)
let applied_validator ~(compiled_code : string) ~(owner_hash : string)
    ~(seed_tx_hash : string) ~(seed_index : int) :
    (Lucid.validator * string, string) result =
  Result.map
    (fun param_hexes ->
      let validator =
        Lucid.plutus_v3_validator
          (Lucid.apply_params_to_script
             ~params:(List.map Lucid.data_of_cbor_hex param_hexes)
             compiled_code)
      in
      (validator, Lucid.minting_policy_to_id validator))
    (params ~owner_hash ~seed_tx_hash ~seed_index)

let app ~(compiled_code : string) ~(owner_hash : string)
    ~(owner_address : string) ~(seed_tx_hash : string) ~(seed_index : int) :
    ((Registry.model, Registry.msg) Tea.app * string, string) result =
  Result.map
    (fun (validator, policy_id) ->
      ( { Tea.update = Registry.update;
          model_to_data = Registry.model_to_data;
          model_of_data = Registry.model_of_data;
          msg_to_data = Registry.msg_to_data;
          (* Every build carries the owner's signature; the terminal
             retire also burns the reference NFT under the script's own
             policy (the spend attachment already witnesses the mint:
             both halves are one script). *)
          finish =
            (fun msg builder ->
              let signed = Lucid.add_signer ~address:owner_address builder in
              match msg with
              | Registry.Publish _ -> signed
              | Registry.Retire ->
                Lucid.mint_assets
                  ~assets:[ (reference_unit ~policy_id, Lucid.bigint "-1") ]
                  ~redeemer:void_redeemer signed);
          validator
        },
        policy_id ))
    (applied_validator ~compiled_code ~owner_hash ~seed_tx_hash ~seed_index)

(* Retire the registry: burn the reference NFT and end the state
   UTxO. Resolves to the transaction hash. *)
let retire (handle : (Registry.model, Registry.msg) Tea.handle) :
    (string, string) result Promise_js.t =
  Tea.halt handle Registry.Retire

(* One-shot genesis: consume the seed UTxO, mint the reference NFT
   under the registry's own policy, and lock it at the script address
   with the version-0 model as inline datum. Resolves to the
   transaction hash. *)
let genesis (handle : (Registry.model, Registry.msg) Tea.handle)
    ~(policy_id : string) ~(seed : Lucid.utxo) ~(initial : Registry.model)
    ~(lovelace : Lucid.lovelace) : (string, string) result Promise_js.t =
  let unit = reference_unit ~policy_id in
  Promise_js.attempt
    (Promise_js.bind
       (Lucid.new_tx handle.Tea.lucid
       |> Lucid.collect_from_wallet ~utxos:[ seed ]
       |> Lucid.mint_assets
            ~assets:[ (unit, Lucid.bigint "1") ]
            ~redeemer:void_redeemer
       |> Lucid.attach_minting_policy ~validator:handle.Tea.app.Tea.validator
       |> Lucid.pay_to_contract_assets ~address:handle.Tea.address
            ~datum:(Registry.model_to_data initial)
            ~assets:
              (Lucid.assets_of
                 [ ("lovelace", lovelace); (unit, Lucid.bigint "1") ])
       |> Lucid.complete)
       Lucid.sign_and_submit)

(* --- Step 4: the pinning pipeline, JS half --- *)

(* Raw bytes of a file through Node's fs, hex round-tripped so bytes
   past 0x7f survive the JS string boundary; null on any fs error. *)
let read_file_js =
  Js.Unsafe.eval_string
    "(function (path) { try { return \
     require('fs').readFileSync(path).toString('hex'); } catch (err) { \
     return null; } })"

let read_file ~(path : string) : (string, string) result =
  let value =
    Js.Unsafe.fun_call read_file_js [| Js.Unsafe.inject (Js.string path) |]
  in
  if Lucid.nullish value then Error ("cannot read " ^ path)
  else
    Result.map_error Tea_data.error_to_string
      (Tea_data.string_of_hex (Js.to_string (Js.Unsafe.coerce value)))

(* Publish a bundle through the TEA runtime: derive its CID and
   blake2b-256 hash with Bundle, dispatch the Publish message. *)
let publish_bundle (handle : (Registry.model, Registry.msg) Tea.handle)
    ~(bytes : string) :
    (Registry.model * string, string) result Promise_js.t =
  Tea.dispatch handle (Bundle.publish_msg bytes)

(* Add-and-pin on a kubo daemon's HTTP API, with the flags that make
   the daemon derive the same CIDv1 the pure code computes: raw leaf
   or chunked dag-pb, since the daemon's default chunker and balanced
   layout match of_bundle's parameters. Resolves to the daemon's CID
   string. *)
let add_to_daemon_js =
  Js.Unsafe.eval_string
    "(function (endpoint, hex) {\
       var bytes = Uint8Array.from(hex.match(/../g) || [], function (h) {\
         return parseInt(h, 16); });\
       var form = new FormData();\
       form.append('file', new Blob([bytes]));\
       return fetch(endpoint\
           + '/api/v0/add?cid-version=1&raw-leaves=true&pin=true',\
         { method: 'POST', body: form })\
         .then(function (res) {\
           if (!res.ok) { throw new Error('daemon status ' + res.status); }\
           return res.json();\
         })\
         .then(function (body) { return body.Hash; });\
     })"

(* Pin the bundle and require the daemon's CID to equal the locally
   derived one: a differential check of the pure CID construction
   against the pinning service. *)
let pin_bundle ~(endpoint : string) ~(bytes : string) :
    (string, string) result Promise_js.t =
  let local = Bundle.cid bytes in
  Promise_js.bind
    (Promise_js.attempt
       (Promise_js.of_any
          (Js.Unsafe.fun_call add_to_daemon_js
             [| Js.Unsafe.inject (Js.string endpoint);
                Js.Unsafe.inject (Js.string (Tea_data.hex_of_string bytes))
             |])))
    (fun outcome ->
      Promise_js.return
        (Result.bind outcome (fun pinned ->
           let pinned = Js.to_string (Js.Unsafe.coerce pinned) in
           if pinned = local then Ok local
           else
             Error
               (Printf.sprintf "daemon pinned %s but the local CID is %s"
                  pinned local))))

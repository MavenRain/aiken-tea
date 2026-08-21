(* Bind the pure registry app to the two-purpose validator from
   plutus.json: apply the (owner, seed) parameters, derive the policy
   id, and decorate every dispatch with the owner's required
   signature. [genesis] is the one-shot mint that creates the
   reference NFT and the version-0 state UTxO. *)

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
          finish = Lucid.add_signer ~address:owner_address;
          validator
        },
        policy_id ))
    (applied_validator ~compiled_code ~owner_hash ~seed_tx_hash ~seed_index)

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

(* Bind the pure counter app to its two-purpose validator (from
   plutus.json) and to its exported update program (from
   uplc/counter-update.json): apply the seed parameter, derive the
   policy id, and expose the one-shot genesis that mints the reference
   NFT marking the state UTxO. *)

open Tea_pure

let void_redeemer = Tea_data.encode (Constr (0, []))

let reference_unit ~(policy_id : string) : string =
  policy_id ^ Counter.reference_token_name_hex

(* The validator's parameter as Plutus Data: the seed OutputReference
   (Constr 0 [transaction_id, output_index]). *)
let params ~(seed_tx_hash : string) ~(seed_index : int) :
    (string list, string) result =
  Result.map_error Tea_data.error_to_string
    (Result.map
       (fun tx_hash ->
         [ Tea_data.encode (Constr (0, [ Bytes tx_hash; Int seed_index ])) ])
       (Tea_data.string_of_hex seed_tx_hash))

(* The compiled validator with the seed applied, plus its policy id
   (= script hash: the mint and spend halves are one script). *)
let applied_validator ~(compiled_code : string) ~(seed_tx_hash : string)
    ~(seed_index : int) : (Lucid.validator * string, string) result =
  Result.map
    (fun param_hexes ->
      let validator =
        Lucid.plutus_v3_validator
          (Lucid.apply_params_to_script
             ~params:(List.map Lucid.data_of_cbor_hex param_hexes)
             compiled_code)
      in
      (validator, Lucid.minting_policy_to_id validator))
    (params ~seed_tx_hash ~seed_index)

let app ~(compiled_code : string) ~(exported_update : string)
    ~(seed_tx_hash : string) ~(seed_index : int) :
    ((Counter.model, Counter.msg) Tea.app * string, string) result =
  let program = Uplc.of_compiled_code ~compiled_code:exported_update in
  Result.map
    (fun (validator, policy_id) ->
      ( { (* The counter never halts: every message steps to a next
             model. *)
          Tea.update = (fun msg model -> Some (Counter.update msg model));
          model_to_data = Counter.model_to_data;
          model_of_data = Counter.model_of_data;
          msg_to_data = Counter.msg_to_data;
          msg_of_data = Counter.msg_of_data;
          (* The counter validator takes tea.Action: a single dispatch
             is the message wrapped in the Single constructor. *)
          msg_to_redeemer =
            (fun msg -> Queue.single_redeemer (Counter.msg_to_data msg));
          (* counter.update returns the bare model on-chain; wrap it in
             the Some verdict the runtime compares against. *)
          uplc_step =
            Some
              (fun ~msg ~model ->
                Result.map
                  (fun next -> "d8799f" ^ next ^ "ff")
                  (Uplc.step program ~msg ~model));
          finish = (fun (_ : Counter.msg) builder -> builder);
          validator
        },
        policy_id ))
    (applied_validator ~compiled_code ~seed_tx_hash ~seed_index)

(* One-shot genesis: consume the seed UTxO, mint the reference NFT
   under the counter's own policy, and lock it at the script address
   with [initial] as inline datum (the validator demands the zero
   model). Resolves to the transaction hash. *)
let genesis (handle : (Counter.model, Counter.msg) Tea.handle)
    ~(policy_id : string) ~(seed : Lucid.utxo) ~(initial : Counter.model)
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
            ~datum:(Counter.model_to_data initial)
            ~assets:
              (Lucid.assets_of
                 [ ("lovelace", lovelace); (unit, Lucid.bigint "1") ])
       |> Lucid.complete)
       Lucid.sign_and_submit)

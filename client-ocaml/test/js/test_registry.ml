(* Emulator suite for the registry app over the two-purpose validator.
   Acceptance dispatches are the mirror-parity oracle: the validator
   recomputes the OCaml-mirrored update on-chain, so a divergence
   fails the test. The reject tests hand-build transactions bypassing
   dispatch to prove the validator (not the client) enforces the
   upgrade policy. Run via test/js/run.mjs with this bundle's path. *)

open Js_of_ocaml
open Tea_pure
open Tea_client

let state_lovelace = "5000000"

let genesis_model : Registry.model =
  { cid = "bafy-genesis"; version = 0; frontend_hash = String.make 32 '\017' }

let upgrade : Registry.msg =
  Registry.Publish
    { cid = "bafy-upgrade"; frontend_hash = String.make 32 '\034' }

(* Oracle for publish steps. A publish always yields a next model; the
   [None] poison makes an accidental terminal verdict fail the equality
   checks loudly instead of masking it. *)
let next_of msg model =
  Registry.update msg model
  |> Option.fold
       ~none:
         { Registry.cid = "<update returned None>";
           version = -1;
           frontend_hash = ""
         }
       ~some:Fun.id

type ctx = {
  emulator : Lucid.emulator;
  lucid : Lucid.t;
  handle : (Registry.model, Registry.msg) Tea.handle;
  policy_id : string;
  owner_address : string;
}

let app_of ctx = ctx.handle.Tea.app
let unit_of ctx = Registry_app.reference_unit ~policy_id:ctx.policy_id

(* Full value of the continuing state output: the pinned lovelace plus
   the reference NFT. *)
let state_assets ctx =
  Lucid.assets_of
    [ ("lovelace", Lucid.bigint state_lovelace);
      (unit_of ctx, Lucid.bigint "1")
    ]

(* Fresh emulator and wallet; the wallet's single UTxO is the seed the
   validator parameters pin. Stops short of genesis so the mint tests
   can build their own. *)
let prepare ((compiled, exported) : string * string) :
    (ctx * Lucid.utxo, string) result Promise_js.t =
  let account = Lucid.generate_emulator_account ~lovelace:"100000000000" in
  let emulator = Lucid.emulator [ account ] in
  Promise_js.bind (Lucid.connect_custom emulator) (fun lucid ->
    Lucid.select_wallet_from_seed lucid (Lucid.account_seed account);
    Promise_js.bind (Lucid.wallet_address lucid) (fun owner_address ->
      Promise_js.bind (Lucid.wallet_utxos lucid) (fun utxos ->
        match utxos with
        | [] -> Promise_js.return (Error "wallet has no seed UTxO")
        | seed :: _ ->
          Registry_app.app ~compiled_code:compiled
            ~exported_update:exported
            ~owner_hash:(Lucid.payment_credential_hash owner_address)
            ~owner_address
            ~seed_tx_hash:(Lucid.utxo_tx_hash seed)
            ~seed_index:(Lucid.utxo_output_index seed)
          |> Result.fold
               ~error:(fun message -> Promise_js.return (Error message))
               ~ok:(fun (app, policy_id) ->
                 Promise_js.return
                   (Ok
                      ( { emulator;
                          lucid;
                          handle = Tea.connect lucid app;
                          policy_id;
                          owner_address
                        },
                        seed ))))))

let setup (code : string * string) : (ctx, string) result Promise_js.t =
  Promise_js.bind_ok (prepare code) (fun (ctx, seed) ->
    Promise_js.bind_ok
      (Registry_app.genesis ctx.handle ~policy_id:ctx.policy_id ~seed
         ~initial:genesis_model ~lovelace:(Lucid.bigint state_lovelace))
      (fun (_ : string) ->
        Lucid.await_block ctx.emulator 1;
        Promise_js.return (Ok ctx)))

let model_text (model : Registry.model) =
  Printf.sprintf "{cid=%s; version=%d}" model.Registry.cid
    model.Registry.version

let expect_confirmed ctx expected : (unit, string) result Promise_js.t =
  Promise_js.bind_ok (Tea.current_model ctx.handle) (fun model ->
    Promise_js.return
      (Harness.check
         (Printf.sprintf "confirmed %s, expected %s" (model_text model)
            (model_text expected))
         (model = expected)))

let step ctx msg : (Registry.model, string) result Promise_js.t =
  Promise_js.bind_ok (Tea.dispatch ctx.handle msg)
    (fun (predicted, (_ : string)) ->
      Lucid.await_block ctx.emulator 1;
      Promise_js.return (Ok predicted))

let step_expect ctx msg expected : (unit, string) result Promise_js.t =
  Promise_js.bind_ok (step ctx msg) (fun predicted ->
    Promise_js.return
      (Harness.check
         (Printf.sprintf "predicted %s, expected %s" (model_text predicted)
            (model_text expected))
         (predicted = expected)))

(* A hand-built upgrade bypassing dispatch: spend the state UTxO with
   [msg], land [pays] as (datum, assets) contract outputs in order,
   and add the owner requirement only when [signed]. *)
let hand_built ctx utxo msg ~signed pays : Lucid.tx_builder =
  let opening =
    Lucid.new_tx ctx.lucid
    |> Lucid.collect_from ~utxos:[ utxo ]
         ~redeemer:(Registry.msg_to_data msg)
    |> Lucid.attach_spending_validator ~validator:(app_of ctx).Tea.validator
  in
  let paid =
    List.fold_left
      (fun builder (datum, assets) ->
        Lucid.pay_to_contract_assets ~address:ctx.handle.Tea.address ~datum
          ~assets builder)
      opening pays
  in
  if signed then Lucid.add_signer ~address:ctx.owner_address paid else paid

(* Step 4 fixture: the exact bytes of fixture-bundle.html, with cid
   and blake2b-256 hash pinned to python3 (hashlib, base64.b32encode). *)
let fixture_cid =
  "bafkreig4ltrhflupn7zg4oxoakcme5u5tr3yywijdzemdc5j3uvpgzarja"

let fixture_hash =
  Tea_data.string_of_hex
    "f3a39b85cda458814a0f9df6a93b34bab135242cfe8e00f0ceda39f033f5e611"
  |> Result.value ~default:""

let fixture_path () : (string, string) result =
  let dir = Js.Unsafe.get Js.Unsafe.global (Js.string "FixtureDir") in
  if Lucid.nullish dir then Error "globalThis.FixtureDir missing"
  else Ok (Js.to_string (Js.Unsafe.coerce dir) ^ "fixture-bundle.html")

let fixture_bytes () : (string, string) result =
  Result.bind (fixture_path ()) (fun path -> Registry_app.read_file ~path)

let ipfs_api_js =
  Js.Unsafe.eval_string
    "(function () { return typeof process === 'undefined' ? null : \
     (process.env.IPFS_API || null); })"

let ipfs_api () : string option =
  let value = Js.Unsafe.fun_call ipfs_api_js [||] in
  if Lucid.nullish value then None
  else Some (Js.to_string (Js.Unsafe.coerce value))

let tests code =
  [ ( "genesis deploys version zero with the reference NFT",
      fun () ->
        Promise_js.bind_ok (setup code) (fun ctx ->
          Promise_js.bind_ok (Tea.state_utxo ctx.handle) (fun utxo ->
            let quantity =
              Lucid.lovelace_to_string
                (Lucid.utxo_asset utxo ~unit:(unit_of ctx))
            in
            Harness.check ("reference NFT quantity = " ^ quantity)
              (quantity = "1")
            |> Result.fold
                 ~error:(fun message -> Promise_js.return (Error message))
                 ~ok:(fun () -> expect_confirmed ctx genesis_model))) );
    ( "confirms each published upgrade on-chain",
      fun () ->
        Promise_js.bind_ok (setup code) (fun ctx ->
          let first = next_of upgrade genesis_model in
          let second_msg =
            Registry.Publish
              { cid = "bafy-final"; frontend_hash = String.make 32 '3' }
          in
          Promise_js.bind_ok (step_expect ctx upgrade first) (fun () ->
            Promise_js.bind_ok (expect_confirmed ctx first) (fun () ->
              Promise_js.bind_ok
                (step_expect ctx second_msg
                   (next_of second_msg first))
                (fun () ->
                  expect_confirmed ctx (next_of second_msg first))))) );
    ( "rejects an unsigned publish",
      fun () ->
        Promise_js.bind_ok (setup code) (fun ctx ->
          Promise_js.bind_ok (Tea.state_utxo ctx.handle) (fun utxo ->
            let next =
              Registry.model_to_data (next_of upgrade genesis_model)
            in
            Harness.expect_reject ~probe:"script"
              (hand_built ctx utxo upgrade ~signed:false
                 [ (next, state_assets ctx) ]))) );
    ( "rejects a forged version jump",
      fun () ->
        Promise_js.bind_ok (setup code) (fun ctx ->
          Promise_js.bind_ok (Tea.state_utxo ctx.handle) (fun utxo ->
            let forged =
              Registry.model_to_data
                { (next_of upgrade genesis_model) with version = 2 }
            in
            Harness.expect_reject
              (hand_built ctx utxo upgrade ~signed:true
                 [ (forged, state_assets ctx) ]))) );
    ( "rejects a publish that strips the reference NFT",
      fun () ->
        Promise_js.bind_ok (setup code) (fun ctx ->
          Promise_js.bind_ok (Tea.state_utxo ctx.handle) (fun utxo ->
            let next =
              Registry.model_to_data (next_of upgrade genesis_model)
            in
            Harness.expect_reject
              (hand_built ctx utxo upgrade ~signed:true
                 [ ( next,
                     Lucid.assets_of
                       [ ("lovelace", Lucid.bigint state_lovelace) ] )
                 ]))) );
    ( "rejects an ill-formed publish with an empty cid",
      fun () ->
        Promise_js.bind_ok (setup code) (fun ctx ->
          Promise_js.bind_ok (Tea.state_utxo ctx.handle) (fun utxo ->
            let empty =
              Registry.Publish
                { cid = ""; frontend_hash = String.make 32 '\017' }
            in
            let next =
              Registry.model_to_data (next_of empty genesis_model)
            in
            Harness.expect_reject
              (hand_built ctx utxo empty ~signed:true
                 [ (next, state_assets ctx) ]))) );
    ( "rejects a genesis that mints more than one reference token",
      fun () ->
        Promise_js.bind_ok (prepare code) (fun (ctx, seed) ->
          Harness.expect_reject
            (Lucid.new_tx ctx.lucid
            |> Lucid.collect_from_wallet ~utxos:[ seed ]
            |> Lucid.mint_assets
                 ~assets:[ (unit_of ctx, Lucid.bigint "2") ]
                 ~redeemer:Registry_app.void_redeemer
            |> Lucid.attach_minting_policy
                 ~validator:(app_of ctx).Tea.validator
            |> Lucid.pay_to_contract_assets ~address:ctx.handle.Tea.address
                 ~datum:(Registry.model_to_data genesis_model)
                 ~assets:
                   (Lucid.assets_of
                      [ ("lovelace", Lucid.bigint state_lovelace);
                        (unit_of ctx, Lucid.bigint "2")
                      ]))) );
    ( "publishes a real bundle whose cid and hash match the oracle",
      fun () ->
        Promise_js.bind_ok (setup code) (fun ctx ->
          fixture_bytes ()
          |> Result.fold
               ~error:(fun message -> Promise_js.return (Error message))
               ~ok:(fun bytes ->
                 Promise_js.bind_ok
                   (Registry_app.publish_bundle ctx.handle ~bytes)
                   (fun (predicted, (_ : string)) ->
                     Lucid.await_block ctx.emulator 1;
                     let expected =
                       { Registry.cid = fixture_cid;
                         version = 1;
                         frontend_hash = fixture_hash
                       }
                     in
                     Harness.check
                       (Printf.sprintf "predicted %s, expected %s"
                          (model_text predicted) (model_text expected))
                       (predicted = expected)
                     |> Result.fold
                          ~error:(fun message ->
                            Promise_js.return (Error message))
                          ~ok:(fun () -> expect_confirmed ctx expected)))) );
    ( "retires the registry: burns the NFT and ends the state",
      fun () ->
        Promise_js.bind_ok (setup code) (fun ctx ->
          Promise_js.bind_ok (Registry_app.retire ctx.handle)
            (fun (_ : string) ->
              Lucid.await_block ctx.emulator 1;
              Promise_js.bind (Tea.state_utxo ctx.handle) (fun gone ->
                Result.is_error gone
                |> Harness.check "state UTxO gone after the retire"
                |> Result.fold
                     ~error:(fun message -> Promise_js.return (Error message))
                     ~ok:(fun () ->
                       Promise_js.bind (Lucid.wallet_utxos ctx.lucid)
                         (fun utxos ->
                           let held =
                             List.exists
                               (fun utxo ->
                                 Lucid.lovelace_to_string
                                   (Lucid.utxo_asset utxo ~unit:(unit_of ctx))
                                 <> "0")
                               utxos
                           in
                           Promise_js.return
                             (Harness.check "reference NFT burned" (not held))))))) );
    ( "rejects an unsigned retire",
      fun () ->
        Promise_js.bind_ok (setup code) (fun ctx ->
          Promise_js.bind_ok (Tea.state_utxo ctx.handle) (fun utxo ->
            Harness.expect_reject ~probe:"script"
              (hand_built ctx utxo Registry.Retire ~signed:false []
              |> Lucid.mint_assets
                   ~assets:[ (unit_of ctx, Lucid.bigint "-1") ]
                   ~redeemer:Registry_app.void_redeemer))) );
    ( "rejects a retire that keeps a state output",
      fun () ->
        Promise_js.bind_ok (setup code) (fun ctx ->
          Promise_js.bind_ok (Tea.state_utxo ctx.handle) (fun utxo ->
            Harness.expect_reject
              (hand_built ctx utxo Registry.Retire ~signed:true
                 [ ( Registry.model_to_data genesis_model,
                     Lucid.assets_of
                       [ ("lovelace", Lucid.bigint state_lovelace) ] )
                 ]
              |> Lucid.mint_assets
                   ~assets:[ (unit_of ctx, Lucid.bigint "-1") ]
                   ~redeemer:Registry_app.void_redeemer))) );
    ( "rejects a retire that pockets the NFT instead of burning",
      fun () ->
        Promise_js.bind_ok (setup code) (fun ctx ->
          Promise_js.bind_ok (Tea.state_utxo ctx.handle) (fun utxo ->
            Harness.expect_reject
              (hand_built ctx utxo Registry.Retire ~signed:true []))) );
    ( "pins on the daemon with an equal cid (needs IPFS_API)",
      fun () ->
        (ipfs_api ()
        |> Option.fold
             ~none:(fun () ->
               print_endline "      (pin skipped: IPFS_API unset)";
               Promise_js.return (Ok ()))
             ~some:(fun endpoint () ->
               fixture_bytes ()
               |> Result.fold
                    ~error:(fun message -> Promise_js.return (Error message))
                    ~ok:(fun bytes ->
                      Promise_js.bind_ok
                        (Registry_app.pin_bundle ~endpoint ~bytes)
                        (fun pinned ->
                          Promise_js.return
                            (Harness.check ("daemon cid " ^ pinned)
                               (pinned = fixture_cid))))))
          () );
    ( "uplc publish and retire match the mirror",
      fun () ->
        Promise_js.return
          (Result.bind (Harness.exported_update ~app:"registry")
             (fun exported ->
               let uplc_step =
                 Uplc.step (Uplc.of_compiled_code ~compiled_code:exported)
               in
               List.fold_left
                 (fun acc msg ->
                   Result.bind acc (fun () ->
                     Result.bind
                       (uplc_step
                          ~msg:(Registry.msg_to_data msg)
                          ~model:(Registry.model_to_data genesis_model))
                       (fun on_chain ->
                         let mirrored =
                           Tea.verdict_to_data Registry.model_to_data
                             (Registry.update msg genesis_model)
                         in
                         Harness.check
                           (Printf.sprintf "uplc %s, mirror %s" on_chain
                              mirrored)
                           (String.equal on_chain mirrored))))
                 (Ok ())
                 [ upgrade; Registry.Retire ])) );
    ( "a divergent registry mirror is caught before submission",
      fun () ->
        Promise_js.bind_ok (setup code) (fun ctx ->
          let divergent =
            { ctx.handle with
              Tea.app =
                { (app_of ctx) with
                  Tea.update =
                    (fun msg model ->
                      Registry.update msg model
                      |> Option.map (fun next ->
                           { next with
                             Registry.version = next.Registry.version + 1
                           }))
                }
            }
          in
          Promise_js.bind (Tea.dispatch divergent upgrade) (fun outcome ->
            Result.fold
              ~ok:(fun ((_ : Registry.model), (_ : string)) ->
                Promise_js.return
                  (Error "dispatch succeeded on a divergent mirror"))
              ~error:(fun message ->
                Harness.check ("divergence not reported: " ^ message)
                  (Harness.mentions "diverges" message)
                |> Result.fold
                     ~error:(fun inner -> Promise_js.return (Error inner))
                     ~ok:(fun () -> expect_confirmed ctx genesis_model))
              outcome)) )
  ]

let () =
  Result.bind (Harness.compiled_code ~title:"registry.registry.spend")
    (fun compiled ->
      Result.map
        (fun exported -> (compiled, exported))
        (Harness.exported_update ~app:"registry"))
  |> Result.fold
       ~ok:(fun code -> Harness.run (tests code))
       ~error:(fun message ->
         print_endline ("setup: " ^ message);
         Lucid.set_exit_code 1)

(* Emulator suite for the OCaml TEA client runtime. The acceptance
   tests are also the
   mirror-parity oracle: every dispatched transition is re-verified
   on-chain by the Aiken validator, so an OCaml/Aiken `update`
   divergence fails the test. The reject tests hand-build transactions
   that bypass `dispatch` to prove the validator (not the client) is
   what enforces the protocol.

   Run via test/js/run.mjs, which injects @lucid-evolution/lucid as
   globalThis.LucidLib and the parsed plutus.json as
   globalThis.Blueprint before importing this bundle. *)

open Tea_pure
open Tea_client

let state_lovelace = "5000000"

type ctx = {
  emulator : Lucid.emulator;
  lucid : Lucid.t;
  handle : (Counter.model, Counter.msg) Tea.handle;
}

(* Fresh emulator, wallet, and deployed state per test (the port of
   the vitest beforeEach). *)
let setup app : (ctx, string) result Promise_js.t =
  let account = Lucid.generate_emulator_account ~lovelace:"100000000000" in
  let emulator = Lucid.emulator [ account ] in
  Promise_js.bind (Lucid.connect_custom emulator) (fun lucid ->
    Lucid.select_wallet_from_seed lucid (Lucid.account_seed account);
    let handle = Tea.connect lucid app in
    Promise_js.bind_ok
      (Tea.deploy handle ~initial:{ Counter.count = 0 }
         ~lovelace:(Lucid.bigint state_lovelace))
      (fun (_ : string) ->
        Lucid.await_block emulator 1;
        Promise_js.return (Ok { emulator; lucid; handle })))

let step ctx msg : (Counter.model, string) result Promise_js.t =
  Promise_js.bind_ok (Tea.dispatch ctx.handle msg)
    (fun (predicted, (_ : string)) ->
      Lucid.await_block ctx.emulator 1;
      Promise_js.return (Ok predicted))

let expect_confirmed ctx expected : (unit, string) result Promise_js.t =
  Promise_js.bind_ok (Tea.current_model ctx.handle) (fun model ->
    Promise_js.return
      (Harness.check
         (Printf.sprintf "confirmed count = %d, expected %d" model.Counter.count
            expected)
         (model.Counter.count = expected)))

let expect_predicted expected model : (unit, string) result =
  Harness.check
    (Printf.sprintf "predicted count = %d, expected %d" model.Counter.count
       expected)
    (model.Counter.count = expected)

(* Dispatch [msg] and require the optimistic model to hit [expected]. *)
let step_expect ctx msg expected : (unit, string) result Promise_js.t =
  Promise_js.bind_ok (step ctx msg) (fun model ->
    Promise_js.return (expect_predicted expected model))


let app_of ctx = ctx.handle.Tea.app

(* A hand-built transition bypassing dispatch: spend the state UTxO
   with [msg], then apply [pays] as pay.ToContract calls in order. *)
let hand_built ctx utxo msg pays : Lucid.tx_builder =
  let opening =
    Lucid.new_tx ctx.lucid
    |> Lucid.collect_from ~utxos:[ utxo ]
         ~redeemer:((app_of ctx).Tea.msg_to_data msg)
    |> Lucid.attach_spending_validator
         ~validator:(app_of ctx).Tea.validator
  in
  List.fold_left
    (fun builder (datum, lovelace) ->
      Lucid.pay_to_contract ~address:ctx.handle.Tea.address ~datum ~lovelace
        builder)
    opening pays

let tests app =
  [ ( "deploys the initial model",
      fun () ->
        Promise_js.bind_ok (setup app) (fun ctx -> expect_confirmed ctx 0) );
    ( "confirms each optimistic update on-chain",
      fun () ->
        Promise_js.bind_ok (setup app) (fun ctx ->
          Promise_js.bind_ok (step_expect ctx Counter.Increment 1) (fun () ->
            Promise_js.bind_ok (expect_confirmed ctx 1) (fun () ->
              Promise_js.bind_ok (step_expect ctx Counter.Increment 2)
                (fun () ->
                  Promise_js.bind_ok (step_expect ctx Counter.Decrement 1)
                    (fun () -> expect_confirmed ctx 1))))) );
    ( "resets to zero",
      fun () ->
        Promise_js.bind_ok (setup app) (fun ctx ->
          Promise_js.bind_ok (step ctx Counter.Increment) (fun (_ : Counter.model) ->
            Promise_js.bind_ok (step ctx Counter.Reset) (fun (_ : Counter.model) ->
              expect_confirmed ctx 0))) );
    ( "rejects a transition whose datum is not update(msg, model)",
      fun () ->
        Promise_js.bind_ok (setup app) (fun ctx ->
          Promise_js.bind_ok (Tea.state_utxo ctx.handle) (fun utxo ->
            let forged = (app_of ctx).Tea.model_to_data { Counter.count = 7 } in
            Harness.expect_reject ~probe:"script"
              (hand_built ctx utxo Counter.Increment
                 [ (forged, Lucid.bigint state_lovelace) ]))) );
    ( "rejects a state split into two script outputs",
      fun () ->
        Promise_js.bind_ok (setup app) (fun ctx ->
          Promise_js.bind_ok (Tea.state_utxo ctx.handle) (fun utxo ->
            let next =
              (app_of ctx).Tea.model_to_data
                (Counter.update Counter.Increment { Counter.count = 0 })
            in
            Harness.expect_reject
              (hand_built ctx utxo Counter.Increment
                 [ (next, Lucid.bigint state_lovelace);
                   (next, Lucid.bigint "2000000")
                 ]))) );
    ( "rejects a transition that drains lovelace from the state",
      fun () ->
        Promise_js.bind_ok (setup app) (fun ctx ->
          Promise_js.bind_ok (Tea.state_utxo ctx.handle) (fun utxo ->
            let next =
              (app_of ctx).Tea.model_to_data
                (Counter.update Counter.Increment { Counter.count = 0 })
            in
            Harness.expect_reject
              (hand_built ctx utxo Counter.Increment
                 [ (next, Lucid.bigint "2000000") ]))) )
  ]

let () =
  Harness.compiled_code ~title:"counter.counter.spend"
  |> Result.fold
       ~ok:(fun code -> Harness.run (tests (Counter_app.app code)))
       ~error:(fun message ->
         print_endline ("setup: " ^ message);
         Lucid.set_exit_code 1)

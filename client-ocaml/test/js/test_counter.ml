(* Emulator suite for the OCaml TEA client runtime; the port of
   client/test/counter.test.ts. The acceptance tests are also the
   mirror-parity oracle: every dispatched transition is re-verified
   on-chain by the Aiken validator, so an OCaml/Aiken `update`
   divergence fails the test. The reject tests hand-build transactions
   that bypass `dispatch` to prove the validator (not the client) is
   what enforces the protocol.

   Run via test/js/run.mjs, which injects @lucid-evolution/lucid as
   globalThis.LucidLib and the parsed plutus.json as
   globalThis.Blueprint before importing this bundle. *)

open Js_of_ocaml
open Tea_pure
open Tea_client

let state_lovelace = "5000000"

let compiled_code () : (string, string) result =
  let blueprint = Js.Unsafe.get Js.Unsafe.global (Js.string "Blueprint") in
  if Lucid.nullish blueprint then Error "globalThis.Blueprint missing"
  else
    Js.to_array (Js.Unsafe.coerce (Js.Unsafe.get blueprint (Js.string "validators")))
    |> Array.to_list
    |> List.find_opt (fun entry ->
         Js.to_string (Js.Unsafe.get entry (Js.string "title"))
         = "counter.counter.spend")
    |> Option.map (fun entry ->
         Js.to_string (Js.Unsafe.get entry (Js.string "compiledCode")))
    |> Option.to_result ~none:"counter.counter.spend missing from plutus.json"

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

let check message condition = if condition then Ok () else Error message

let expect_confirmed ctx expected : (unit, string) result Promise_js.t =
  Promise_js.bind_ok (Tea.current_model ctx.handle) (fun model ->
    Promise_js.return
      (check
         (Printf.sprintf "confirmed count = %d, expected %d" model.Counter.count
            expected)
         (model.Counter.count = expected)))

let expect_predicted expected model : (unit, string) result =
  check
    (Printf.sprintf "predicted count = %d, expected %d" model.Counter.count
       expected)
    (model.Counter.count = expected)

(* Dispatch [msg] and require the optimistic model to hit [expected]. *)
let step_expect ctx msg expected : (unit, string) result Promise_js.t =
  Promise_js.bind_ok (step ctx msg) (fun model ->
    Promise_js.return (expect_predicted expected model))

(* A transaction build that must be rejected by the validator when the
   emulator evaluates the script at complete(). *)
let expect_reject ?probe (builder : Lucid.tx_builder) :
    (unit, string) result Promise_js.t =
  Promise_js.map
    (fun outcome ->
      Result.fold
        ~ok:(fun (_ : Lucid.tx) -> Error "transaction completed, expected a script rejection")
        ~error:(fun message ->
          probe
          |> Option.fold ~none:(Ok ()) ~some:(fun fragment ->
               check
                 (Printf.sprintf "rejection %S does not mention %S" message
                    fragment)
                 (Js.Unsafe.meth_call
                    (Js.string message)
                    "includes"
                    [| Js.Unsafe.inject (Js.string fragment) |]
                 |> Js.to_bool)))
        outcome)
    (Promise_js.attempt (Lucid.complete builder))

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
            expect_reject ~probe:"script"
              (hand_built ctx utxo Counter.Increment
                 [ (forged, Lucid.bigint state_lovelace) ]))) );
    ( "rejects a state split into two script outputs",
      fun () ->
        Promise_js.bind_ok (setup app) (fun ctx ->
          Promise_js.bind_ok (Tea.state_utxo ctx.handle) (fun utxo ->
            let next =
              (app_of ctx).Tea.model_to_data
                ((app_of ctx).Tea.update Counter.Increment { Counter.count = 0 })
            in
            expect_reject
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
                ((app_of ctx).Tea.update Counter.Increment { Counter.count = 0 })
            in
            expect_reject
              (hand_built ctx utxo Counter.Increment
                 [ (next, Lucid.bigint "2000000") ]))) )
  ]

let run_tests app =
  let suite = tests app in
  Promise_js.map
    (fun failures ->
      Printf.printf "%d tests, %d failures\n%!" (List.length suite) failures;
      if failures > 0 then Lucid.set_exit_code 1)
    (List.fold_left
       (fun acc (name, body) ->
         Promise_js.bind acc (fun failures ->
           Promise_js.map
             (Result.fold
                ~ok:(fun () ->
                  Printf.printf "ok  - %s\n%!" name;
                  failures)
                ~error:(fun message ->
                  Printf.printf "FAIL- %s: %s\n%!" name message;
                  failures + 1))
             (Promise_js.run (body ()))))
       (Promise_js.return 0) suite)

let () =
  compiled_code ()
  |> Result.fold
       ~ok:(fun code -> ignore (run_tests (Counter_app.app code)))
       ~error:(fun message ->
         print_endline ("setup: " ^ message);
         Lucid.set_exit_code 1)

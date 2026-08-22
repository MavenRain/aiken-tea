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
  policy_id : string;
}

let account () = Lucid.generate_emulator_account ~lovelace:"100000000000"

(* Fresh emulator and funded wallets; the first wallet's first UTxO is
   the seed the validator parameter pins. Stops short of genesis so
   the genesis tests can build their own. *)
let prepare_with ((compiled, exported) : string * string) accounts :
    (ctx * Lucid.utxo, string) result Promise_js.t =
  let emulator = Lucid.emulator accounts in
  match accounts with
  | [] -> Promise_js.return (Error "no emulator accounts")
  | first :: _ ->
    Promise_js.bind (Lucid.connect_custom emulator) (fun lucid ->
      Lucid.select_wallet_from_seed lucid (Lucid.account_seed first);
      Promise_js.bind (Lucid.wallet_utxos lucid) (fun utxos ->
        match utxos with
        | [] -> Promise_js.return (Error "wallet has no seed UTxO")
        | seed :: _ ->
          Counter_app.app ~compiled_code:compiled ~exported_update:exported
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
                          policy_id
                        },
                        seed )))))

let prepare code = prepare_with code [ account () ]

(* Genesis with the zero model: the deployment every test builds on. *)
let setup (code : string * string) : (ctx, string) result Promise_js.t =
  Promise_js.bind_ok (prepare code) (fun (ctx, seed) ->
    Promise_js.bind_ok
      (Counter_app.genesis ctx.handle ~policy_id:ctx.policy_id ~seed
         ~initial:{ Counter.count = 0 }
         ~lovelace:(Lucid.bigint state_lovelace))
      (fun (_ : string) ->
        Lucid.await_block ctx.emulator 1;
        Promise_js.return (Ok ctx)))

(* The same ctx with a deliberately wrong Increment mirror, for the
   divergence tests. Genesis stays honest: only dispatch diverges. *)
let divergent_ctx ctx =
  { ctx with
    handle =
      { ctx.handle with
        Tea.app =
          { ctx.handle.Tea.app with
            Tea.update =
              (fun msg model ->
                Some
                  (match msg with
                  | Counter.Increment ->
                    { Counter.count = model.Counter.count + 2 }
                  | Counter.Decrement | Counter.Reset ->
                    Counter.update msg model))
          }
      }
  }

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
let unit_of ctx = Counter_app.reference_unit ~policy_id:ctx.policy_id

let lovelace_assets amount =
  Lucid.assets_of [ ("lovelace", Lucid.bigint amount) ]

(* Full value of the continuing state output: the pinned lovelace plus
   the reference NFT. *)
let state_assets ctx =
  Lucid.assets_of
    [ ("lovelace", Lucid.bigint state_lovelace);
      (unit_of ctx, Lucid.bigint "1")
    ]

(* A drained continuing output that still keeps the NFT, so the
   lovelace check is the only violation. *)
let drained_assets ctx =
  Lucid.assets_of
    [ ("lovelace", Lucid.bigint "2000000"); (unit_of ctx, Lucid.bigint "1") ]

(* A hand-built transition bypassing dispatch: spend the state UTxO
   with [msg], then apply [pays] as (datum, assets) contract outputs
   in order. *)
let hand_built ctx utxo msg pays : Lucid.tx_builder =
  let opening =
    Lucid.new_tx ctx.lucid
    |> Lucid.collect_from ~utxos:[ utxo ]
         ~redeemer:((app_of ctx).Tea.msg_to_redeemer msg)
    |> Lucid.attach_spending_validator ~validator:(app_of ctx).Tea.validator
  in
  List.fold_left
    (fun builder (datum, assets) ->
      Lucid.pay_to_contract_assets ~address:ctx.handle.Tea.address ~datum
        ~assets builder)
    opening pays

(* --- Step 8 fixtures: a queue bound to the counter, two wallets --- *)

type qctx = { ctx : ctx; seed_a : string; seed_b : string; queue : Tea.queue }

(* Two funded accounts; wallet A runs the genesis and stays selected.
   The queue script gets the counter's script hash as its parameter. *)
let setup_queue (code : string * string) ~queue_code :
    (qctx, string) result Promise_js.t =
  let account_a = account () in
  let account_b = account () in
  Promise_js.bind_ok (prepare_with code [ account_a; account_b ])
    (fun (ctx, seed) ->
      Promise_js.bind_ok
        (Counter_app.genesis ctx.handle ~policy_id:ctx.policy_id ~seed
           ~initial:{ Counter.count = 0 }
           ~lovelace:(Lucid.bigint state_lovelace))
        (fun (_ : string) ->
          Lucid.await_block ctx.emulator 1;
          Tea.queue_for ctx.handle ~compiled_code:queue_code
          |> Result.fold
               ~error:(fun message -> Promise_js.return (Error message))
               ~ok:(fun queue ->
                 Promise_js.return
                   (Ok
                      {
                        ctx;
                        seed_a = Lucid.account_seed account_a;
                        seed_b = Lucid.account_seed account_b;
                        queue;
                      }))))

(* Enqueue [msg] as the wallet behind [seed], then confirm the block. *)
let enqueue_as q seed msg : (unit, string) result Promise_js.t =
  Lucid.select_wallet_from_seed q.ctx.lucid seed;
  Promise_js.bind_ok
    (Tea.enqueue q.ctx.handle q.queue msg ~lovelace:(Lucid.bigint "2000000"))
    (fun (_ : string) ->
      Lucid.await_block q.ctx.emulator 1;
      Promise_js.return (Ok ()))

let expect_entry_count q expected : (unit, string) result Promise_js.t =
  Promise_js.map
    (fun entries ->
      Harness.check
        (Printf.sprintf "queue holds %d entries, expected %d"
           (List.length entries) expected)
        (List.length entries = expected))
    (Tea.queue_entries q.ctx.handle q.queue)

let tests code queue_code =
  [ ( "genesis deploys the zero model with the reference NFT",
      fun () ->
        Promise_js.bind_ok (setup code) (fun ctx -> expect_confirmed ctx 0) );
    ( "genesis refuses a nonzero initial model",
      fun () ->
        Promise_js.bind_ok (prepare code) (fun (ctx, seed) ->
          Promise_js.bind
            (Counter_app.genesis ctx.handle ~policy_id:ctx.policy_id ~seed
               ~initial:{ Counter.count = 5 }
               ~lovelace:(Lucid.bigint state_lovelace))
            (fun outcome ->
              Result.fold
                ~ok:(fun (_ : string) ->
                  Promise_js.return (Error "genesis accepted a nonzero model"))
                ~error:(fun (_ : string) -> Promise_js.return (Ok ()))
                outcome)) );
    ( "confirms each optimistic update on-chain",
      fun () ->
        Promise_js.bind_ok (setup code) (fun ctx ->
          Promise_js.bind_ok (step_expect ctx Counter.Increment 1) (fun () ->
            Promise_js.bind_ok (expect_confirmed ctx 1) (fun () ->
              Promise_js.bind_ok (step_expect ctx Counter.Increment 2)
                (fun () ->
                  Promise_js.bind_ok (step_expect ctx Counter.Decrement 1)
                    (fun () -> expect_confirmed ctx 1))))) );
    ( "resets to zero",
      fun () ->
        Promise_js.bind_ok (setup code) (fun ctx ->
          Promise_js.bind_ok (step ctx Counter.Increment) (fun (_ : Counter.model) ->
            Promise_js.bind_ok (step ctx Counter.Reset) (fun (_ : Counter.model) ->
              expect_confirmed ctx 0))) );
    ( "rejects a transition whose datum is not update(msg, model)",
      fun () ->
        Promise_js.bind_ok (setup code) (fun ctx ->
          Promise_js.bind_ok (Tea.state_utxo ctx.handle) (fun utxo ->
            let forged = (app_of ctx).Tea.model_to_data { Counter.count = 7 } in
            Harness.expect_reject ~probe:"script"
              (hand_built ctx utxo Counter.Increment
                 [ (forged, state_assets ctx) ]))) );
    ( "rejects a state split into two script outputs",
      fun () ->
        Promise_js.bind_ok (setup code) (fun ctx ->
          Promise_js.bind_ok (Tea.state_utxo ctx.handle) (fun utxo ->
            let next =
              (app_of ctx).Tea.model_to_data
                (Counter.update Counter.Increment { Counter.count = 0 })
            in
            Harness.expect_reject
              (hand_built ctx utxo Counter.Increment
                 [ (next, state_assets ctx); (next, lovelace_assets "2000000") ]))) );
    ( "rejects a transition that drains lovelace from the state",
      fun () ->
        Promise_js.bind_ok (setup code) (fun ctx ->
          Promise_js.bind_ok (Tea.state_utxo ctx.handle) (fun utxo ->
            let next =
              (app_of ctx).Tea.model_to_data
                (Counter.update Counter.Increment { Counter.count = 0 })
            in
            Harness.expect_reject
              (hand_built ctx utxo Counter.Increment
                 [ (next, drained_assets ctx) ]))) );
    ( "rejects a transition that strips the reference NFT",
      fun () ->
        Promise_js.bind_ok (setup code) (fun ctx ->
          Promise_js.bind_ok (Tea.state_utxo ctx.handle) (fun utxo ->
            let next =
              (app_of ctx).Tea.model_to_data
                (Counter.update Counter.Increment { Counter.count = 0 })
            in
            Harness.expect_reject ~probe:"script"
              (hand_built ctx utxo Counter.Increment
                 [ (next, lovelace_assets state_lovelace) ]))) );
    ( "uplc step matches the mirror on every message",
      fun () ->
        Promise_js.return
          (let compiled, exported = code in
           Counter_app.app ~compiled_code:compiled ~exported_update:exported
             ~seed_tx_hash:(String.make 64 '0') ~seed_index:0
           |> Result.fold
                ~error:(fun message -> Error message)
                ~ok:(fun (app, (_ : string)) ->
                  List.fold_left
                    (fun acc (msg, count) ->
                      Result.bind acc (fun () ->
                        Result.bind
                          (Option.to_result
                             ~none:"counter app carries no uplc step"
                             app.Tea.uplc_step)
                          (fun uplc_step ->
                            Result.bind
                              (uplc_step
                                 ~msg:(Counter.msg_to_data msg)
                                 ~model:
                                   (Counter.model_to_data { Counter.count }))
                              (fun on_chain ->
                                let mirrored =
                                  "d8799f"
                                  ^ Counter.model_to_data
                                      (Counter.update msg { Counter.count })
                                  ^ "ff"
                                in
                                Harness.check
                                  (Printf.sprintf "%s on %d: uplc %s, mirror %s"
                                     (Counter.msg_to_string msg) count on_chain
                                     mirrored)
                                  (String.equal on_chain mirrored)))))
                    (Ok ())
                    [ (Counter.Increment, 0);
                      (Counter.Increment, 41);
                      (Counter.Decrement, 5);
                      (Counter.Decrement, 0);
                      (Counter.Reset, 7)
                    ])) );
    ( "a divergent mirror is caught before submission",
      fun () ->
        Promise_js.bind_ok (setup code) (fun honest ->
          let ctx = divergent_ctx honest in
          Promise_js.bind (Tea.dispatch ctx.handle Counter.Increment)
            (fun outcome ->
              Result.fold
                ~ok:(fun ((_ : Counter.model), (_ : string)) ->
                  Promise_js.return
                    (Error "dispatch succeeded on a divergent mirror"))
                ~error:(fun message ->
                  Harness.check ("divergence not reported: " ^ message)
                    (Harness.mentions "diverges" message)
                  |> Result.fold
                       ~error:(fun inner ->
                         Promise_js.return (Error inner))
                       ~ok:(fun () -> expect_confirmed ctx 0))
                outcome)) );
    ( "a malformed exported program reports an error, not a crash",
      fun () ->
        let broken = Uplc.of_compiled_code ~compiled_code:"deadbeef" in
        Promise_js.return
          (Result.fold
             ~ok:(fun (hex : string) ->
               Error ("malformed program evaluated to " ^ hex))
             ~error:(fun (_ : string) -> Ok ())
             (Uplc.step broken ~msg:"d87980" ~model:"d8799f00ff")) );
    ( "a non-constructor message is rejected by the machine",
      fun () ->
        Promise_js.return
          (Result.bind (Harness.exported_update ~app:"counter")
             (fun exported ->
               let program =
                 Uplc.of_compiled_code ~compiled_code:exported
               in
               Result.fold
                 ~ok:(fun (hex : string) ->
                   Error ("machine accepted an integer message: " ^ hex))
                 ~error:(fun (_ : string) -> Ok ())
                 (Uplc.step program ~msg:"00" ~model:"d8799f00ff"))) );
    ( "batches two users' queued messages into one transition",
      fun () ->
        Promise_js.bind_ok (setup_queue code ~queue_code) (fun q ->
          Promise_js.bind_ok (enqueue_as q q.seed_a Counter.Increment)
            (fun () ->
              Promise_js.bind_ok (enqueue_as q q.seed_b Counter.Increment)
                (fun () ->
                  Lucid.select_wallet_from_seed q.ctx.lucid q.seed_a;
                  Promise_js.bind_ok (Tea.process_queue q.ctx.handle q.queue)
                    (fun (final, (_ : string)) ->
                      Lucid.await_block q.ctx.emulator 1;
                      Result.fold
                        ~error:(fun message ->
                          Promise_js.return (Error message))
                        ~ok:(fun () ->
                          Promise_js.bind_ok (expect_confirmed q.ctx 2)
                            (fun () -> expect_entry_count q 0))
                        (expect_predicted 2 final))))) );
    ( "an empty queue refuses to batch",
      fun () ->
        Promise_js.bind_ok (setup_queue code ~queue_code) (fun q ->
          Promise_js.bind (Tea.process_queue q.ctx.handle q.queue)
            (fun outcome ->
              Result.fold
                ~ok:(fun ((_ : Counter.model), (_ : string)) ->
                  Promise_js.return (Error "batched an empty queue"))
                ~error:(fun message ->
                  Promise_js.return
                    (Harness.check
                       ("emptiness not reported: " ^ message)
                       (Harness.mentions "empty" message)))
                outcome)) );
    ( "reclaims a queued entry with the author's signature",
      fun () ->
        Promise_js.bind_ok (setup_queue code ~queue_code) (fun q ->
          Promise_js.bind_ok (enqueue_as q q.seed_b Counter.Increment)
            (fun () ->
              Promise_js.bind (Tea.queue_entries q.ctx.handle q.queue)
                (fun entries ->
                  match entries with
                  | [ (utxo, Counter.Increment) ] ->
                    Promise_js.bind_ok (Tea.reclaim q.ctx.handle q.queue utxo)
                      (fun (_ : string) ->
                        Lucid.await_block q.ctx.emulator 1;
                        Promise_js.bind_ok (expect_entry_count q 0) (fun () ->
                          expect_confirmed q.ctx 0))
                  | (_ : (Lucid.utxo * Counter.msg) list) ->
                    Promise_js.return
                      (Error "expected exactly the enqueued Increment")))) );
    ( "a stranger cannot reclaim a queued entry",
      fun () ->
        Promise_js.bind_ok (setup_queue code ~queue_code) (fun q ->
          Promise_js.bind_ok (enqueue_as q q.seed_b Counter.Increment)
            (fun () ->
              Lucid.select_wallet_from_seed q.ctx.lucid q.seed_a;
              Promise_js.bind (Tea.queue_entries q.ctx.handle q.queue)
                (fun entries ->
                  match entries with
                  | [ (utxo, (_ : Counter.msg)) ] ->
                    Promise_js.bind (Tea.reclaim q.ctx.handle q.queue utxo)
                      (fun outcome ->
                        Result.fold
                          ~ok:(fun (_ : string) ->
                            Promise_js.return
                              (Error "a stranger reclaimed the entry"))
                          ~error:(fun (_ : string) -> expect_entry_count q 1)
                          outcome)
                  | (_ : (Lucid.utxo * Counter.msg) list) ->
                    Promise_js.return
                      (Error "expected exactly the enqueued Increment")))) );
    ( "a divergent mirror halts the batch before submission",
      fun () ->
        Promise_js.bind_ok (setup_queue code ~queue_code) (fun honest ->
          let q = { honest with ctx = divergent_ctx honest.ctx } in
          Promise_js.bind_ok (enqueue_as q q.seed_a Counter.Increment)
            (fun () ->
              Promise_js.bind (Tea.process_queue q.ctx.handle q.queue)
                (fun outcome ->
                  Result.fold
                    ~ok:(fun ((_ : Counter.model), (_ : string)) ->
                      Promise_js.return
                        (Error "batch submitted on a divergent mirror"))
                    ~error:(fun message ->
                      Harness.check ("divergence not reported: " ^ message)
                        (Harness.mentions "diverges" message)
                      |> Result.fold
                           ~error:(fun inner ->
                             Promise_js.return (Error inner))
                           ~ok:(fun () ->
                             Promise_js.bind_ok (expect_confirmed q.ctx 0)
                               (fun () -> expect_entry_count q 1)))
                    outcome))) );
    ( "a look-alike state UTxO cannot drain the queue",
      fun () ->
        (* The closed step-8 limit: wallet B stages a state-shaped UTxO
           at the counter address (no NFT: only the one-shot genesis can
           mint it) and tries to drain a queue entry into it. The queue's
           Process check and the counter's NFT-keeps check both refuse. *)
        Promise_js.bind_ok (setup_queue code ~queue_code) (fun q ->
          Lucid.select_wallet_from_seed q.ctx.lucid q.seed_b;
          let fake_datum =
            (app_of q.ctx).Tea.model_to_data { Counter.count = 7 }
          in
          Promise_js.bind_ok
            (Tea.deploy q.ctx.handle ~initial:{ Counter.count = 7 }
               ~lovelace:(Lucid.bigint "2000000"))
            (fun (_ : string) ->
              Lucid.await_block q.ctx.emulator 1;
              Promise_js.bind_ok (enqueue_as q q.seed_b Counter.Increment)
                (fun () ->
                  Promise_js.bind (Tea.queue_entries q.ctx.handle q.queue)
                    (fun entries ->
                      Promise_js.bind
                        (Lucid.utxos_at q.ctx.lucid q.ctx.handle.Tea.address)
                        (fun at_state ->
                          let fakes =
                            List.filter
                              (fun u ->
                                Option.fold ~none:false
                                  ~some:(String.equal fake_datum)
                                  (Lucid.utxo_datum u))
                              at_state
                          in
                          match (fakes, entries) with
                          | [ fake ], [ (entry, (_ : Counter.msg)) ] ->
                            let claimed =
                              (app_of q.ctx).Tea.model_to_data
                                { Counter.count = 8 }
                            in
                            Harness.expect_reject ~probe:"script"
                              (Lucid.new_tx q.ctx.lucid
                              |> Lucid.collect_from ~utxos:[ fake ]
                                   ~redeemer:
                                     ((app_of q.ctx).Tea.msg_to_redeemer
                                        Counter.Increment)
                              |> Lucid.attach_spending_validator
                                   ~validator:(app_of q.ctx).Tea.validator
                              |> Lucid.collect_from ~utxos:[ entry ]
                                   ~redeemer:Tea_pure.Queue.process_redeemer
                              |> Lucid.attach_spending_validator
                                   ~validator:q.queue.Tea.queue_validator
                              |> Lucid.pay_to_contract
                                   ~address:q.ctx.handle.Tea.address
                                   ~datum:claimed
                                   ~lovelace:(Lucid.bigint "2000000"))
                          | ((_ : Lucid.utxo list),
                             (_ : (Lucid.utxo * Counter.msg) list)) ->
                            Promise_js.return
                              (Error
                                 "expected exactly one look-alike and one entry"))))))
    )
  ]

let () =
  Result.bind (Harness.compiled_code ~title:"counter.counter.spend")
    (fun compiled ->
      Result.bind (Harness.exported_update ~app:"counter") (fun exported ->
        Result.map
          (fun queue_code -> ((compiled, exported), queue_code))
          (Harness.compiled_code ~title:"queue.queue.spend")))
  |> Result.fold
       ~ok:(fun (code, queue_code) -> Harness.run (tests code queue_code))
       ~error:(fun message ->
         print_endline ("setup: " ^ message);
         Lucid.set_exit_code 1)

(* Generic Elm-Architecture (TEA) client runtime for Cardano.
   The on-chain half (lib/tea.ak)
   verifies one TEA step per transaction; this half produces those
   transactions. [dispatch] computes the next model locally with the
   mirrored pure [update] (the optimistic update), checks that model
   against the exported on-chain step evaluated off-chain (see Uplc),
   then submits a transaction the validator re-checks by recomputing
   the same step on-chain. [subscribe] is the Sub side: it polls the
   script address
   and reports the confirmed model. Every fallible operation resolves
   to a [result]; nothing raises. *)

open Js_of_ocaml

(* A TEA application: the pure step function plus the Data codecs and
   the compiled validator that enforces that step on-chain. *)
type ('model, 'msg) app = {
  (* The pure TEA step function. [None] marks a terminal message: the
     application ends and no state UTxO continues (see [halt]). *)
  update : 'msg -> 'model -> 'model option;
  model_to_data : 'model -> string;
  model_of_data : string -> ('model, string) result;
  msg_to_data : 'msg -> string;
  (* Decode one queued Data payload back into the message it carries:
     the batch fold (see [process_queue]) mirrors the on-chain
     `decode` the same way [update] mirrors the on-chain step. *)
  msg_of_data : string -> ('msg, string) result;
  (* The full redeemer for one dispatched message. A batch-capable app
     wraps the message in `Single` (tea.Action); an app without a
     queue sends the bare message. *)
  msg_to_redeemer : 'msg -> string;
  (* The compiled on-chain step, exported by `aiken export` and
     evaluated off-chain (see Uplc). Before submission the runtime
     requires its verdict to equal the mirrored [update]'s, byte for
     byte at the Data level: Constr 0 [model'] for a next model,
     Constr 1 [] for a terminal verdict. [None] skips the check. *)
  uplc_step : (msg:string -> model:string -> (string, string) result) option;
  (* Per-app decoration of every build, keyed by the message: extra
     required signers, the terminal token burn, and similar policy
     inputs the generic runtime cannot know. [fun _ builder -> builder]
     when the app needs none. *)
  finish : 'msg -> Lucid.tx_builder -> Lucid.tx_builder;
  validator : Lucid.validator;
}

(* A connected app: Lucid instance plus the derived script address. *)
type ('model, 'msg) handle = {
  lucid : Lucid.t;
  app : ('model, 'msg) app;
  address : string;
}

let connect lucid app =
  {
    lucid;
    app;
    address = Lucid.validator_to_address (Lucid.network lucid) app.validator;
  }

(* Create the state UTxO carrying the initial model as an inline
   datum. Resolves to the transaction hash. *)
let deploy handle ~initial ~lovelace =
  Promise_js.attempt
    (Promise_js.bind
       (Lucid.new_tx handle.lucid
       |> Lucid.pay_to_contract ~address:handle.address
            ~datum:(handle.app.model_to_data initial) ~lovelace
       |> Lucid.complete)
       Lucid.sign_and_submit)

(* The single state UTxO at the script address. More or fewer than one
   datum-carrying UTxO means the app state is broken (or not
   deployed). *)
let state_utxo handle =
  Promise_js.run
    (Promise_js.map
       (fun utxos ->
         let with_datum =
           List.filter
             (fun utxo -> Option.is_some (Lucid.utxo_datum utxo))
             utxos
         in
         match with_datum with
         | [ only ] -> Ok only
         | none_or_many ->
           Error
             (Printf.sprintf "expected exactly one state UTxO at %s, found %d"
                handle.address
                (List.length none_or_many)))
       (Lucid.utxos_at handle.lucid handle.address))

let utxo_model handle utxo =
  Result.bind
    (Option.to_result ~none:"state UTxO carries no datum"
       (Lucid.utxo_datum utxo))
    handle.app.model_of_data

(* The Data encoding of an update verdict: Some model' is Constr 0
   [model'], None is Constr 1 []. CBOR composes, so wrapping the
   already-encoded model hex reproduces exactly what Tea_data.encode
   would emit for the same constructor. *)
let verdict_to_data model_to_data verdict =
  verdict
  |> Option.fold ~none:"d87a80" ~some:(fun model ->
       "d8799f" ^ model_to_data model ^ "ff")

(* The optimistic-update gate: when the app carries the exported
   on-chain step, evaluate it on the same (msg, model) and require
   byte equality with the mirror's verdict. Divergence aborts before
   any transaction is built. *)
let mirror_check app msg model verdict =
  app.uplc_step
  |> Option.fold ~none:(Ok ()) ~some:(fun step ->
       Result.bind
         (step ~msg:(app.msg_to_data msg) ~model:(app.model_to_data model))
         (fun on_chain ->
           let mirrored = verdict_to_data app.model_to_data verdict in
           if String.equal on_chain mirrored then Ok ()
           else
             Error
               (Printf.sprintf
                  "mirror diverges from the on-chain step: uplc %s, mirror %s"
                  on_chain mirrored)))

(* The confirmed on-chain model. *)
let current_model handle =
  Promise_js.bind_ok (state_utxo handle) (fun utxo ->
    Promise_js.return (utxo_model handle utxo))

(* One TEA step: read the confirmed model, compute the next model
   locally (the optimistic update, returned immediately alongside the
   hash), and submit the transition transaction with the message as
   redeemer. *)
let dispatch handle msg =
  Promise_js.bind_ok (state_utxo handle) (fun utxo ->
    utxo_model handle utxo
    |> Result.fold
         ~error:(fun message -> Promise_js.return (Error message))
         ~ok:(fun model ->
           handle.app.update msg model
           |> Option.fold
                ~none:
                  (Promise_js.return
                     (Error "terminal message: there is no next model, use halt"))
                ~some:(fun predicted ->
                  mirror_check handle.app msg model (Some predicted)
                  |> Result.fold
                       ~error:(fun message ->
                         Promise_js.return (Error message))
                       ~ok:(fun () ->
                  Promise_js.run
                    (Promise_js.map
                       (fun tx_hash -> Ok (predicted, tx_hash))
                       (Promise_js.bind
                          (Lucid.new_tx handle.lucid
                          |> Lucid.collect_from ~utxos:[ utxo ]
                               ~redeemer:(handle.app.msg_to_redeemer msg)
                          |> Lucid.attach_spending_validator
                               ~validator:handle.app.validator
                          |> Lucid.pay_to_contract_assets
                               ~address:handle.address
                               ~datum:(handle.app.model_to_data predicted)
                               ~assets:(Lucid.utxo_assets utxo)
                          |> handle.app.finish msg
                          |> Lucid.complete)
                          Lucid.sign_and_submit))))))

(* One terminal TEA step: the message's update verdict must be [None].
   Spends the state UTxO with the message as redeemer and recreates
   nothing; the app's [finish] adds the teardown policy the generic
   runtime cannot know (the state-token burn, the owner signature).
   Resolves to the transaction hash. *)
let halt handle msg =
  Promise_js.bind_ok (state_utxo handle) (fun utxo ->
    utxo_model handle utxo
    |> Result.fold
         ~error:(fun message -> Promise_js.return (Error message))
         ~ok:(fun model ->
           if Option.is_some (handle.app.update msg model) then
             Promise_js.return
               (Error "not a terminal message: a next model exists, use dispatch")
           else
             mirror_check handle.app msg model None
             |> Result.fold
                  ~error:(fun message -> Promise_js.return (Error message))
                  ~ok:(fun () ->
                    Promise_js.attempt
                      (Promise_js.bind
                         (Lucid.new_tx handle.lucid
                         |> Lucid.collect_from ~utxos:[ utxo ]
                              ~redeemer:(handle.app.msg_to_redeemer msg)
                         |> Lucid.attach_spending_validator
                              ~validator:handle.app.validator
                         |> handle.app.finish msg
                         |> Lucid.complete)
                         Lucid.sign_and_submit))))

(* The Sub side: poll the script address and report each confirmed
   model. Returns the unsubscribe function. Poll errors are dropped,
   like the TS original. *)
let subscribe handle ~on_model ~interval_ms =
  let tick () =
    ignore
      (Promise_js.map
         (Result.fold ~ok:on_model ~error:(fun (_ : string) -> ()))
         (current_model handle))
  in
  let timer =
    Js.Unsafe.fun_call
      (Js.Unsafe.get Js.Unsafe.global (Js.string "setInterval"))
      [| Js.Unsafe.inject (Js.wrap_callback tick);
         Js.Unsafe.inject interval_ms
      |]
  in
  fun () ->
    ignore
      (Js.Unsafe.fun_call
         (Js.Unsafe.get Js.Unsafe.global (Js.string "clearInterval"))
         [| timer |])

(* --- Step 8: the message queue, batched dispatch --- *)

(* A queue script bound to one app: the compiled queue validator with
   the app's script hash applied as its parameter, the derived address
   entries are locked at, and the queue's own script hash (what the
   Batch redeemer names). *)
type queue = {
  queue_validator : Lucid.validator;
  queue_address : string;
  queue_hash : string;
}

let queue_for handle ~compiled_code =
  Result.map
    (fun validator ->
      {
        queue_validator = validator;
        queue_address =
          Lucid.validator_to_address (Lucid.network handle.lucid) validator;
        queue_hash = Lucid.minting_policy_to_id validator;
      })
    (Result.map
       (fun state_hash ->
         Lucid.plutus_v3_validator
           (Lucid.apply_params_to_script
              ~params:
                [
                  Lucid.data_of_cbor_hex
                    (Tea_pure.Tea_data.encode (Bytes state_hash));
                ]
              compiled_code))
       (Result.map_error Tea_pure.Tea_data.error_to_string
          (Tea_pure.Tea_data.string_of_hex
             (Lucid.payment_credential_hash handle.address))))

(* Lock one message at the queue address as a Queued inline datum,
   authored by the connected wallet. The entry's lovelace is the
   batcher's fee, or comes back on reclaim. Resolves to the
   transaction hash. *)
let enqueue handle queue msg ~lovelace =
  Promise_js.bind (Lucid.wallet_address handle.lucid) (fun author_address ->
    Tea_pure.Queue.queued_datum
      ~author_hash:(Lucid.payment_credential_hash author_address)
      ~msg_hex:(handle.app.msg_to_data msg)
    |> Result.fold
         ~error:(fun message -> Promise_js.return (Error message))
         ~ok:(fun datum ->
           Promise_js.attempt
             (Promise_js.bind
                (Lucid.new_tx handle.lucid
                |> Lucid.pay_to_contract ~address:queue.queue_address ~datum
                     ~lovelace
                |> Lucid.complete)
                Lucid.sign_and_submit)))

(* The pending entries at the queue address, paired with their decoded
   messages, in ledger input order (output reference: transaction id
   lexicographically, then index) - the order the on-chain fold will
   apply. An entry whose datum or message fails to decode is skipped:
   junk parked at the queue address must not jam the batcher. *)
let queue_entries handle queue =
  Promise_js.map
    (fun utxos ->
      List.filter_map
        (fun utxo ->
          Option.bind (Lucid.utxo_datum utxo) (fun datum ->
            Result.to_option
              (Result.bind
                 (Tea_pure.Queue.queued_msg_hex datum)
                 handle.app.msg_of_data)
            |> Option.map (fun msg -> (utxo, msg))))
        utxos
      |> List.sort (fun (a, (_ : 'msg)) (b, (_ : 'msg)) ->
           let by_hash =
             String.compare (Lucid.utxo_tx_hash a) (Lucid.utxo_tx_hash b)
           in
           if by_hash = 0 then
             compare (Lucid.utxo_output_index a) (Lucid.utxo_output_index b)
           else by_hash))
    (Lucid.utxos_at handle.lucid queue.queue_address)

(* One batched TEA step: drain every decodable queue entry into a
   single transition. The mirror folds the messages in ledger input
   order, gating each step through the optimistic-eval check; the
   transaction then consumes the state UTxO (Batch redeemer, naming
   the queue script) plus the entries (Process redeemer), and the
   validator re-folds on-chain. The entries' lovelace, less the fee,
   stays with the submitting wallet: the batcher's payment. Resolves
   to the final model and the transaction hash. *)
let process_queue handle queue =
  Promise_js.bind_ok (state_utxo handle) (fun state ->
    Promise_js.bind (queue_entries handle queue) (fun entries ->
      Result.bind (utxo_model handle state) (fun model ->
        match entries with
        | [] -> Error "the queue is empty: nothing to batch"
        | _ :: _ ->
          List.fold_left
            (fun acc ((_ : Lucid.utxo), msg) ->
              Result.bind acc (fun current ->
                handle.app.update msg current
                |> Option.fold
                     ~none:
                       (Error "terminal message in a batch: a halt travels alone")
                     ~some:(fun next ->
                       Result.map
                         (fun () -> next)
                         (mirror_check handle.app msg current (Some next)))))
            (Ok model) entries)
      |> Result.fold
           ~error:(fun message -> Promise_js.return (Error message))
           ~ok:(fun final ->
             Tea_pure.Queue.batch_redeemer ~queue_hash:queue.queue_hash
             |> Result.fold
                  ~error:(fun message -> Promise_js.return (Error message))
                  ~ok:(fun batch ->
                    Promise_js.run
                      (Promise_js.map
                         (fun tx_hash -> Ok (final, tx_hash))
                         (Promise_js.bind
                            (Lucid.new_tx handle.lucid
                            |> Lucid.collect_from ~utxos:[ state ]
                                 ~redeemer:batch
                            |> Lucid.attach_spending_validator
                                 ~validator:handle.app.validator
                            |> Lucid.collect_from
                                 ~utxos:(List.map fst entries)
                                 ~redeemer:Tea_pure.Queue.process_redeemer
                            |> Lucid.attach_spending_validator
                                 ~validator:queue.queue_validator
                            |> Lucid.pay_to_contract_assets
                                 ~address:handle.address
                                 ~datum:(handle.app.model_to_data final)
                                 ~assets:(Lucid.utxo_assets state)
                            |> Lucid.complete)
                            Lucid.sign_and_submit))))))

(* Take one queued entry back: the connected wallet must be the
   entry's author (the validator checks the signature). Resolves to
   the transaction hash. *)
let reclaim handle queue utxo =
  Promise_js.bind (Lucid.wallet_address handle.lucid) (fun author_address ->
    Promise_js.attempt
      (Promise_js.bind
         (Lucid.new_tx handle.lucid
         |> Lucid.collect_from ~utxos:[ utxo ]
              ~redeemer:Tea_pure.Queue.reclaim_redeemer
         |> Lucid.attach_spending_validator ~validator:queue.queue_validator
         |> Lucid.add_signer ~address:author_address
         |> Lucid.complete)
         Lucid.sign_and_submit))

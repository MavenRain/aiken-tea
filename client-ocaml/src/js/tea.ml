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
                               ~redeemer:(handle.app.msg_to_data msg)
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
                              ~redeemer:(handle.app.msg_to_data msg)
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

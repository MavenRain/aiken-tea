(* FFI bindings for the slice of @lucid-evolution/lucid the TEA runtime
   uses. The library object itself is injected by the node driver
   (test/js/run.mjs) as globalThis.LucidLib, because the package is
   ESM-only and a js_of_ocaml bundle cannot import it directly.
   Lovelace amounts stay opaque JS bigints end to end; OCaml ints are
   32-bit under js_of_ocaml and must never hold an amount. *)

open Js_of_ocaml

type t
type emulator
type account
type utxo
type tx_builder
type tx
type signed_tx
type validator = Js.Unsafe.any
type lovelace = Js.Unsafe.any
type network = Js.Unsafe.any

let global name = Js.Unsafe.get Js.Unsafe.global (Js.string name)
let lib () : Js.Unsafe.any = global "LucidLib"
let lib_get name = Js.Unsafe.get (lib ()) (Js.string name)

let is_nullish_js =
  Js.Unsafe.eval_string "(function (value) { return value == null; })"

let nullish (value : Js.Unsafe.any) : bool =
  Js.to_bool (Js.Unsafe.fun_call is_nullish_js [| value |])

let bigint (text : string) : lovelace =
  Js.Unsafe.fun_call (global "BigInt") [| Js.Unsafe.inject (Js.string text) |]

let generate_emulator_account ~(lovelace : string) : account =
  Js.Unsafe.fun_call
    (lib_get "generateEmulatorAccount")
    [| Js.Unsafe.obj [| ("lovelace", bigint lovelace) |] |]

let account_seed (account : account) : string =
  Js.to_string (Js.Unsafe.get account (Js.string "seedPhrase"))

let emulator (accounts : account list) : emulator =
  Js.Unsafe.new_obj (lib_get "Emulator")
    [| Js.Unsafe.inject (Js.array (Array.of_list accounts)) |]

let await_block (emulator : emulator) (height : int) : unit =
  ignore
    (Js.Unsafe.meth_call emulator "awaitBlock" [| Js.Unsafe.inject height |])

(* Lucid(emulator, "Custom") *)
let connect_custom (emulator : emulator) : t Promise_js.t =
  Promise_js.of_any
    (Js.Unsafe.fun_call (lib_get "Lucid")
       [| Js.Unsafe.inject emulator; Js.Unsafe.inject (Js.string "Custom") |])

let select_wallet_from_seed (lucid : t) (seed : string) : unit =
  ignore
    (Js.Unsafe.meth_call
       (Js.Unsafe.get lucid (Js.string "selectWallet"))
       "fromSeed"
       [| Js.Unsafe.inject (Js.string seed) |])

let network (lucid : t) : network =
  let config = Js.Unsafe.meth_call lucid "config" [||] in
  let value = Js.Unsafe.get config (Js.string "network") in
  if nullish value then Js.Unsafe.inject (Js.string "Custom") else value

let validator_to_address (network : network) (validator : validator) : string =
  Js.to_string
    (Js.Unsafe.fun_call
       (lib_get "validatorToAddress")
       [| network; Js.Unsafe.inject validator |])

let plutus_v3_validator (compiled_code : string) : validator =
  let script =
    Js.Unsafe.fun_call
      (lib_get "applyDoubleCborEncoding")
      [| Js.Unsafe.inject (Js.string compiled_code) |]
  in
  Js.Unsafe.obj
    [| ("type", Js.Unsafe.inject (Js.string "PlutusV3")); ("script", script) |]

let utxos_at (lucid : t) (address : string) : utxo list Promise_js.t =
  Promise_js.map
    (fun utxos -> Array.to_list (Js.to_array (Js.Unsafe.coerce utxos)))
    (Promise_js.of_any
       (Js.Unsafe.meth_call lucid "utxosAt"
          [| Js.Unsafe.inject (Js.string address) |]))

let utxo_datum (utxo : utxo) : string option =
  let datum = Js.Unsafe.get utxo (Js.string "datum") in
  if nullish datum then None else Some (Js.to_string datum)

let utxo_lovelace (utxo : utxo) : lovelace =
  let assets = Js.Unsafe.get utxo (Js.string "assets") in
  let amount = Js.Unsafe.get assets (Js.string "lovelace") in
  if nullish amount then bigint "0" else amount

let new_tx (lucid : t) : tx_builder = Js.Unsafe.meth_call lucid "newTx" [||]

let pay_to_contract ~(address : string) ~(datum : string)
    ~(lovelace : lovelace) (builder : tx_builder) : tx_builder =
  Js.Unsafe.meth_call
    (Js.Unsafe.get builder (Js.string "pay"))
    "ToContract"
    [| Js.Unsafe.inject (Js.string address);
       Js.Unsafe.obj
         [| ("kind", Js.Unsafe.inject (Js.string "inline"));
            ("value", Js.Unsafe.inject (Js.string datum))
         |];
       Js.Unsafe.obj [| ("lovelace", lovelace) |]
    |]

let collect_from ~(utxos : utxo list) ~(redeemer : string)
    (builder : tx_builder) : tx_builder =
  Js.Unsafe.meth_call builder "collectFrom"
    [| Js.Unsafe.inject (Js.array (Array.of_list utxos));
       Js.Unsafe.inject (Js.string redeemer)
    |]

let attach_spending_validator ~(validator : validator) (builder : tx_builder)
    : tx_builder =
  Js.Unsafe.meth_call
    (Js.Unsafe.get builder (Js.string "attach"))
    "SpendingValidator"
    [| Js.Unsafe.inject validator |]

let complete (builder : tx_builder) : tx Promise_js.t =
  Promise_js.of_any (Js.Unsafe.meth_call builder "complete" [||])

(* tx.sign.withWallet().complete() then submit() *)
let sign_and_submit (tx : tx) : string Promise_js.t =
  let signing =
    Js.Unsafe.meth_call (Js.Unsafe.get tx (Js.string "sign")) "withWallet" [||]
  in
  Promise_js.bind
    (Promise_js.of_any (Js.Unsafe.meth_call signing "complete" [||]))
    (fun (signed : signed_tx) ->
      Promise_js.map Js.to_string
        (Promise_js.of_any (Js.Unsafe.meth_call signed "submit" [||])))

let set_exit_code (code : int) : unit =
  Js.Unsafe.set (global "process") (Js.string "exitCode") code

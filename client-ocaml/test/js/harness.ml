(* Shared plumbing for the js emulator suites: blueprint lookup, the
   script-rejection oracle, and the sequential runner. *)

open Js_of_ocaml
open Tea_client

(* The compiled code of the blueprint validator named [title], from the
   plutus.json the node driver injects as globalThis.Blueprint. *)
let compiled_code ~(title : string) : (string, string) result =
  let blueprint = Js.Unsafe.get Js.Unsafe.global (Js.string "Blueprint") in
  if Lucid.nullish blueprint then Error "globalThis.Blueprint missing"
  else
    Js.to_array
      (Js.Unsafe.coerce (Js.Unsafe.get blueprint (Js.string "validators")))
    |> Array.to_list
    |> List.find_opt (fun entry ->
         Js.to_string (Js.Unsafe.get entry (Js.string "title")) = title)
    |> Option.map (fun entry ->
         Js.to_string (Js.Unsafe.get entry (Js.string "compiledCode")))
    |> Option.to_result ~none:(title ^ " missing from plutus.json")

(* The compiledCode of an exported update program, from the JSON
   files the node driver injects as globalThis.UplcExports. *)
let exported_update ~(app : string) : (string, string) result =
  let exports = Js.Unsafe.get Js.Unsafe.global (Js.string "UplcExports") in
  if Lucid.nullish exports then Error "globalThis.UplcExports missing"
  else
    let entry = Js.Unsafe.get exports (Js.string app) in
    if Lucid.nullish entry then Error (app ^ " missing from UplcExports")
    else Ok (Js.to_string (Js.Unsafe.get entry (Js.string "compiledCode")))

(* True when [message] contains [fragment] (JS String.includes). *)
let mentions (fragment : string) (message : string) : bool =
  Js.Unsafe.meth_call (Js.string message) "includes"
    [| Js.Unsafe.inject (Js.string fragment) |]
  |> Js.to_bool

let check message condition = if condition then Ok () else Error message

(* A transaction build that must be rejected by the validator when the
   emulator evaluates the script at complete(). *)
let expect_reject ?probe (builder : Lucid.tx_builder) :
    (unit, string) result Promise_js.t =
  Promise_js.map
    (fun outcome ->
      Result.fold
        ~ok:(fun (_ : Lucid.tx) ->
          Error "transaction completed, expected a script rejection")
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

(* Run the suite sequentially, report per test, and set a failing exit
   code when anything fails. *)
let run (suite : (string * (unit -> (unit, string) result Promise_js.t)) list)
    : unit =
  ignore
    (Promise_js.map
       (fun failures ->
         Printf.printf "%d tests, %d failures\n%!" (List.length suite)
           failures;
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
          (Promise_js.return 0) suite))

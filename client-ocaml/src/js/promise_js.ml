open Js_of_ocaml

type 'a t = Js.Unsafe.any

let of_any any = any
let to_any promise = promise

let promise_constructor () =
  Js.Unsafe.get Js.Unsafe.global (Js.string "Promise")

let return value =
  Js.Unsafe.meth_call (promise_constructor ()) "resolve"
    [| Js.Unsafe.inject value |]

let bind promise continue =
  Js.Unsafe.meth_call promise "then"
    [| Js.Unsafe.inject (Js.wrap_callback continue) |]

let map transform promise =
  Js.Unsafe.meth_call promise "then"
    [| Js.Unsafe.inject (Js.wrap_callback transform) |]

(* String(reason): total on any rejection value, Error or not. *)
let reason_to_string reason =
  Js.to_string
    (Js.Unsafe.fun_call
       (Js.Unsafe.get Js.Unsafe.global (Js.string "String"))
       [| reason |])

let attempt promise =
  Js.Unsafe.meth_call promise "then"
    [| Js.Unsafe.inject (Js.wrap_callback (fun value -> Ok value));
       Js.Unsafe.inject
         (Js.wrap_callback (fun reason -> Error (reason_to_string reason)))
    |]

let run promise = map Result.join (attempt promise)

let bind_ok promise continue =
  bind promise
    (Result.fold ~ok:continue ~error:(fun message -> return (Error message)))

(* Typed view of a JS promise. The phantom parameter keeps distinct
   payloads from unifying; rejections only ever surface through
   [attempt]/[run], as a [result], never as an exception. *)

type 'a t

val return : 'a -> 'a t
val bind : 'a t -> ('a -> 'b t) -> 'b t
val map : ('a -> 'b) -> 'a t -> 'b t

(* Resolve to [Ok value], or [Error message] if the chain rejected. *)
val attempt : 'a t -> ('a, string) result t

(* [attempt] for chains that already carry a result: rejection and a
   domain error collapse into the same [Error]. *)
val run : ('a, string) result t -> ('a, string) result t

(* Monadic bind over the result layer: short-circuits on [Error]. *)
val bind_ok :
  ('a, string) result t ->
  ('a -> ('b, string) result t) ->
  ('b, string) result t

(* FFI boundary: view an untyped JS promise as a typed one and back. *)
val of_any : Js_of_ocaml.Js.Unsafe.any -> 'a t
val to_any : 'a t -> Js_of_ocaml.Js.Unsafe.any

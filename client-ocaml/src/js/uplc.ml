(* Off-chain evaluation of the compiled on-chain step. `aiken export`
   turns an application's `update` function into a standalone UPLC
   program; this module runs that program on the harmoniclabs CEK
   machine (pure JS, so it runs in a browser as well as under node)
   with the message and model as Plutus Data arguments. The evaluated
   result is the authority the mirrored OCaml `update` is checked
   against before any transaction is submitted: the optimistic update
   cannot silently diverge from what the validator will recompute. *)

open Js_of_ocaml

(* An exported program: the hex `compiledCode` field of an
   `aiken export -m <module> -n update` JSON. *)
type program = Program of string

let of_compiled_code ~(compiled_code : string) : program =
  Program compiled_code

(* globalThis.[name], injected by the node driver (run.mjs) or the
   embedding page. *)
let library (name : string) : (Js.Unsafe.any, string) result =
  let value = Js.Unsafe.get Js.Unsafe.global (Js.string name) in
  if Lucid.nullish value then Error ("globalThis." ^ name ^ " missing")
  else Ok value

(* One JS closure runs the whole pipeline inside its own try: unwrap
   the CBOR bytestring around the flat program, decode it, apply the
   two Data arguments, run the CEK machine, and hex the resulting Data
   back out. Every failure (bad hex, malformed program, machine
   error, non-Data result) becomes { err }, never an exception. *)
let eval_js =
  Js.Unsafe.eval_string
    {|
(function (uplc, machine, pdata, compiledHex, msgHex, modelHex) {
  try {
    const bytes = (hex) => {
      if (typeof hex !== "string" || hex.length % 2 !== 0
          || /[^0-9a-f]/.test(hex))
        throw new Error("not lowercase hex: " + String(hex).slice(0, 32));
      return Uint8Array.from(hex.match(/../g) ?? [],
        (pair) => parseInt(pair, 16));
    };
    const wrapped = bytes(compiledHex);
    const skip =
      wrapped[0] === 0x58 ? 2
      : wrapped[0] === 0x59 ? 3
      : wrapped[0] === 0x5a ? 5
      : 0;
    if (skip === 0)
      throw new Error("compiledCode is not a CBOR bytestring");
    const program = uplc.UPLCDecoder.parse(wrapped.subarray(skip), "flat");
    const applied = new uplc.Application(
      new uplc.Application(
        program.body,
        uplc.UPLCConst.data(pdata.dataFromCbor(msgHex))),
      uplc.UPLCConst.data(pdata.dataFromCbor(modelHex)));
    const result = machine.Machine.evalSimple(applied);
    if (machine.CEKError !== undefined && result instanceof machine.CEKError)
      throw new Error("the machine rejected the step: "
        + (result.msg ?? "evaluation error"));
    const out = pdata.dataToCbor(result.value);
    const u8 = typeof out.toBuffer === "function" ? out.toBuffer() : out;
    return {
      ok: Array.from(u8, (b) => b.toString(16).padStart(2, "0")).join(""),
    };
  } catch (error) {
    return { err: String(error) };
  }
})
|}

(* Evaluate update(msg, model) at the Data level: hex CBOR in, hex
   CBOR out. *)
let step (Program compiled_code) ~(msg : string) ~(model : string) :
    (string, string) result =
  Result.bind (library "UplcLib") (fun uplc ->
    Result.bind (library "PlutusMachine") (fun machine ->
      Result.bind (library "PlutusData") (fun pdata ->
        let outcome =
          Js.Unsafe.fun_call eval_js
            [| uplc;
               machine;
               pdata;
               Js.Unsafe.inject (Js.string compiled_code);
               Js.Unsafe.inject (Js.string msg);
               Js.Unsafe.inject (Js.string model)
            |]
        in
        let ok = Js.Unsafe.get outcome (Js.string "ok") in
        if Lucid.nullish ok then
          Error
            ("uplc step failed: "
            ^ Js.to_string (Js.Unsafe.get outcome (Js.string "err")))
        else Ok (Js.to_string (Js.Unsafe.coerce ok)))))

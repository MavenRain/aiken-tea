(* Native gate for the pure half: CBOR vectors pinned to what Lucid's
   Data.to produces for the same values, plus round-trips and decoder
   rejection cases. Runs under `dune runtest` with no JS toolchain. *)

open Tea_pure

let roundtrips value =
  Tea_data.decode (Tea_data.encode value) = Ok value

let checks =
  [ (* Vectors pinned against @lucid-evolution Data.to output. *)
    ("model {count=0} encodes as d8799f00ff",
     Counter.model_to_data { count = 0 } = "d8799f00ff");
    ("model {count=1} encodes as d8799f01ff",
     Counter.model_to_data { count = 1 } = "d8799f01ff");
    ("msg Increment encodes as d87980",
     Counter.msg_to_data Counter.Increment = "d87980");
    ("msg Decrement encodes as d87a80",
     Counter.msg_to_data Counter.Decrement = "d87a80");
    ("msg Reset encodes as d87b80",
     Counter.msg_to_data Counter.Reset = "d87b80");
    ("model decode inverts encode",
     Counter.model_of_data "d8799f00ff" = Ok { Counter.count = 0 });
    ("model decode accepts a definite-length field list",
     Counter.model_of_data "d87981182a" = Ok { Counter.count = 42 });
    ("model decode rejects a bare integer",
     Result.is_error (Counter.model_of_data "00"));
    ("model decode rejects the wrong constructor",
     Result.is_error (Counter.model_of_data "d87a9f00ff"));
    (* Codec round-trips across the integer forms. *)
    ("int 0", roundtrips (Int 0));
    ("int 23", roundtrips (Int 23));
    ("int 24", roundtrips (Int 24));
    ("int 255", roundtrips (Int 255));
    ("int 256", roundtrips (Int 256));
    ("int 65535", roundtrips (Int 65535));
    ("int 100000", roundtrips (Int 100000));
    ("int 2^40", roundtrips (Int (1 lsl 40)));
    ("int -1", roundtrips (Int (-1)));
    ("int -1000000", roundtrips (Int (-1000000)));
    (* Constructor index ranges: compact, extended, general. *)
    ("constr 0 empty", roundtrips (Constr (0, [])));
    ("constr 6", roundtrips (Constr (6, [ Int 5 ])));
    ("constr 7 uses tag 1280", roundtrips (Constr (7, [ Int 5 ])));
    ("constr 127", roundtrips (Constr (127, [])));
    ("constr 200 uses tag 102", roundtrips (Constr (200, [ Int 1; Int 2 ])));
    ("nested constrs",
     roundtrips (Constr (1, [ Constr (0, [ Int (-7) ]); Int 9 ])));
    (* Decoder rejections. *)
    ("odd-length hex rejected", Result.is_error (Tea_data.decode "d87"));
    ("non-hex rejected", Result.is_error (Tea_data.decode "zz"));
    ("truncated rejected", Result.is_error (Tea_data.decode "d8799f00"));
    ("trailing bytes rejected", Result.is_error (Tea_data.decode "d8798000"));
    ("map rejected", Result.is_error (Tea_data.decode "a0"));
    (* Byte strings (added with the registry app). *)
    ("empty bytes encodes as 40", Tea_data.encode (Bytes "") = "40");
    ("bytes round-trip", roundtrips (Bytes "abc"));
    ("bytes 64-byte round-trip", roundtrips (Bytes (String.make 64 'x')));
    ("chunked indefinite bytes decode",
     Tea_data.decode "5f43616263426465ff" = Ok (Tea_data.Bytes "abcde"));
    ("hex helper inverts", Tea_data.string_of_hex "6162" = Ok "ab");
    (* Registry vectors pinned against @lucid-evolution Data.to. *)
    ("registry model v0 encodes as Lucid Data.to",
     Registry.model_to_data
       { cid = "bafy-genesis"; version = 0; frontend_hash = String.make 32 '\017' }
     = "d8799f4c626166792d67656e6573697300" ^ "5820" ^ String.make 64 '1' ^ "ff");
    ("registry publish encodes as Lucid Data.to",
     Registry.msg_to_data
       (Registry.Publish
          { cid = "bafy-upgrade"; frontend_hash = String.make 32 '\017' })
     = "d8799f4c626166792d75706772616465" ^ "5820" ^ String.make 64 '1' ^ "ff");
    ("registry model decode inverts encode",
     Registry.model_of_data
       (Registry.model_to_data
          { cid = "c"; version = 3; frontend_hash = String.make 32 'h' })
     = Ok { Registry.cid = "c"; version = 3; frontend_hash = String.make 32 'h' });
    ("registry update bumps the version",
     Registry.update
       (Registry.Publish { cid = "b"; frontend_hash = "h" })
       { cid = "a"; version = 4; frontend_hash = "g" }
     = { Registry.cid = "b"; version = 5; frontend_hash = "h" });
    ("registry well_formed rejects an empty cid",
     not (Registry.well_formed "" (String.make 32 'h')));
    ("registry well_formed rejects a short hash",
     not (Registry.well_formed "cid" "short"));
    (* The step function, pinned. *)
    ("update Increment",
     Counter.update Counter.Increment { count = 4 } = { Counter.count = 5 });
    ("update Decrement",
     Counter.update Counter.Decrement { count = 4 } = { Counter.count = 3 });
    ("update Reset",
     Counter.update Counter.Reset { count = 4 } = { Counter.count = 0 })
  ]

let () =
  let failed =
    List.fold_left
      (fun failed (name, ok) ->
        Printf.printf "%s %s\n" (if ok then "ok  -" else "FAIL-") name;
        if ok then failed else failed + 1)
      0 checks
  in
  Printf.printf "%d checks, %d failures\n" (List.length checks) failed;
  if failed > 0 then exit 1

(* Native gate for the pure half: CBOR vectors pinned to what Lucid's
   Data.to produces for the same values, plus round-trips and decoder
   rejection cases. Runs under `dune runtest` with no JS toolchain. *)

open Tea_pure

let roundtrips value =
  Tea_data.decode (Tea_data.encode value) = Ok value

let hex = Tea_data.hex_of_string

(* The n-byte ramp i land 0xff: no two chunks are equal, so a chunk-
   or link-ordering mutation cannot hide behind repeated blocks. *)
let ramp n = Tea_data.string_of_byte_list (List.init n (fun i -> i land 0xff))

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
     = Some { Registry.cid = "b"; version = 5; frontend_hash = "h" });
    ("registry retire encodes as Lucid Data.to",
     Registry.msg_to_data Registry.Retire = "d87a80");
    ("registry update retire ends the application",
     Registry.update Registry.Retire
       { Registry.cid = "a"; version = 4; frontend_hash = "g" }
     = None);
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
     Counter.update Counter.Reset { count = 4 } = { Counter.count = 0 });
    (* Hash vectors, pinned to python3 hashlib and cross-checked with
       shasum -a 256 / b2sum -l 256. The 64/128/200-byte inputs cover
       the multi-block fold and both padding parities. *)
    ("sha256 of empty",
     hex (Hashes.sha256 "")
     = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855");
    ("sha256 of abc",
     hex (Hashes.sha256 "abc")
     = "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad");
    ("sha256 of the fixture greeting",
     hex (Hashes.sha256 "hello aiken-tea\n")
     = "9c4347cb168b05a614b721635cfe5dc94d6b63440b6b5f83ab0ce3cfc47a68aa");
    ("sha256 of one full block",
     hex (Hashes.sha256 (String.make 64 'a'))
     = "ffe054fe7ae0cb6dc65c3af9b61d5209f439851db43d0ba5997337df154668eb");
    ("sha256 of 200 bytes",
     hex (Hashes.sha256 (String.make 200 'a'))
     = "c2a908d98f5df987ade41b5fce213067efbcc21ef2240212a41e54b5e7c28ae5");
    ("blake2b-256 of empty",
     hex (Hashes.blake2b_256 "")
     = "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8");
    ("blake2b-256 of abc",
     hex (Hashes.blake2b_256 "abc")
     = "bddd813c634239723171ef3fee98579b94964e3bb1cb3e427262c8c068d52319");
    ("blake2b-256 of the fixture greeting",
     hex (Hashes.blake2b_256 "hello aiken-tea\n")
     = "bfc98bca86ea2d3a92edcf3c8c0195e255c5da90f3b9be157141759db5c1179e");
    ("blake2b-256 of one full block",
     hex (Hashes.blake2b_256 (String.make 128 'a'))
     = "ae2aa48507885c4c950fb809b2076f959cde9f8ea6da260d9a3587df33dac450");
    ("blake2b-256 of 200 bytes",
     hex (Hashes.blake2b_256 (String.make 200 'a'))
     = "6b6e59aaf00eb730cf93de53560846722184bbd92f8368c21ffa95380c2f9fe6");
    (* Byte ramps make every message word distinct, so a wrong sigma
       permutation or word order cannot cancel out, unlike the uniform
       'aaa...' blocks above. *)
    ("sha256 of the 0..127 byte ramp",
     hex (Hashes.sha256 (Tea_data.string_of_byte_list (List.init 128 Fun.id)))
     = "471fb943aa23c511f6f72f8d1652d9c880cfa392ad80503120547703e56a2be5");
    ("blake2b-256 of the 0..127 byte ramp",
     hex
       (Hashes.blake2b_256
          (Tea_data.string_of_byte_list (List.init 128 Fun.id)))
     = "c3582f71ebb2be66fa5dd750f80baae97554f3b015663c8be377cfcb2488c1d1");
    ("blake2b-256 of the 0..199 byte ramp",
     hex
       (Hashes.blake2b_256
          (Tea_data.string_of_byte_list (List.init 200 Fun.id)))
     = "63c3d97a9f8894d5e043a707b0fee7f7ec4c049a23bbf1079df20b4165f9e22d");
    (* Base32 (RFC 4648 lowercase, no padding) and LEB128, pinned to
       python3 base64.b32encode and hand LEB128. *)
    ("base32 of empty", Cid.base32 "" = "");
    ("base32 of fo", Cid.base32 "fo" = "mzxq");
    ("base32 of foob", Cid.base32 "foob" = "mzxw6yq");
    ("base32 of foobar", Cid.base32 "foobar" = "mzxw6ytboi");
    ("varint 0", hex (Cid.varint 0) = "00");
    ("varint 127", hex (Cid.varint 127) = "7f");
    ("varint 128", hex (Cid.varint 128) = "8001");
    ("varint 300", hex (Cid.varint 300) = "ac02");
    ("varint 262144", hex (Cid.varint 262144) = "808010");
    (* CIDv1 raw-leaf vectors; the empty one is the well-known public
       empty-block CID, confirming the version/codec/multihash prefix. *)
    ("cid of empty",
     Cid.of_raw_block ""
     = Ok "bafkreihdwdcefgh4dqkjv67uzcmw7ojee6xedzdetojuzjevtenxquvyku");
    ("cid of abc",
     Cid.of_raw_block "abc"
     = Ok "bafkreif2pall7dybz7vecqka3zo24irdwabwdi4wc55jznaq75q7eaavvu");
    ("cid of the fixture greeting",
     Cid.of_raw_block "hello aiken-tea\n"
     = Ok "bafkreie4ind4wfulawtbjnzbmnop4xojjvvwgralnnpyhkym4ph4i6tivi");
    ("cid accepts a full raw block",
     Result.is_ok (Cid.of_raw_block (String.make 262144 'x')));
    ("cid rejects past one raw block",
     Result.is_error (Cid.of_raw_block (String.make 262145 'x')));
    (* Chunked dag-pb vectors, pinned to ipfs-unixfs-importer (the
       Helia/kubo reference implementation; regenerate with
       test/js/oracle_cids.mjs). Tiny parameters exercise the
       multi-level tree; production parameters cross the raw-block
       boundary for real. *)
    ("chunked cid: one tiny chunk stays a raw block",
     Cid.of_bundle_with ~chunk_size:4 ~fanout:2 (ramp 4)
     = "bafkreiafj3pmdubbd5re73imxsu5j6kabmheshcdoqvpfrnqvpv7bsmq3a");
    ("chunked cid: two chunks, one level",
     Cid.of_bundle_with ~chunk_size:4 ~fanout:2 (ramp 8)
     = "bafybeidi3txkchykjupk3j4wfbwbzgj3skzhk2ey6hbmkgviaj7vrx4p64");
    ("chunked cid: a trailing lone leaf still gets its own parent",
     Cid.of_bundle_with ~chunk_size:4 ~fanout:2 (ramp 9)
     = "bafybeib5lmbpvzcdgw65yiwyhjskzt2bs7gkgu5k72dbflnqsmmuo5qa7q");
    ("chunked cid: four chunks fill two levels",
     Cid.of_bundle_with ~chunk_size:4 ~fanout:2 (ramp 16)
     = "bafybeiasle3qmuobd22vlfmyxjheaa5pqsu4byawpbednrcbwuihqrz2fe");
    ("chunked cid: five chunks need three levels",
     Cid.of_bundle_with ~chunk_size:4 ~fanout:2 (ramp 20)
     = "bafybeiaypvylxcrpdril3jpir3omidycuihxgaalmrjfcvdpeoh5cgk6ny");
    ("chunked cid: production parameters, one byte past a raw block",
     Cid.of_bundle (ramp 262145)
     = "bafybeibp4affrl5svfd2wwp3l2srpu76awzmvyc37zmrtap5iqydfxffuy");
    ("chunked cid: production parameters, an exact three-chunk bundle",
     Cid.of_bundle (ramp 786432)
     = "bafybeiepg5bjyssjj6qquajjo6djc7tr3altcf7le3n3bqyen2f2gj7cf4");
    ("of_bundle agrees with of_raw_block under one chunk",
     Ok (Cid.of_bundle "hello aiken-tea\n")
     = Cid.of_raw_block "hello aiken-tea\n");
    (* The pinning pipeline composes both digests into the message,
       chunking an oversize bundle instead of rejecting it. *)
    ("bundle publish_msg pairs cid with blake2b hash",
     Bundle.publish_msg "hello aiken-tea\n"
     = Registry.Publish
         { cid = "bafkreie4ind4wfulawtbjnzbmnop4xojjvvwgralnnpyhkym4ph4i6tivi";
           frontend_hash =
             Tea_data.string_of_hex
               "bfc98bca86ea2d3a92edcf3c8c0195e255c5da90f3b9be157141759db5c1179e"
             |> Result.value ~default:""
         });
    ("bundle publish_msg chunks an oversize bundle",
     Bundle.publish_msg (ramp 262145)
     = Registry.Publish
         { cid = "bafybeibp4affrl5svfd2wwp3l2srpu76awzmvyc37zmrtap5iqydfxffuy";
           frontend_hash =
             Tea_data.string_of_hex
               "7b60b9f741bb79a38689b7ebc454a6d285e9ceed9f349d29ea785a1dd2723968"
             |> Result.value ~default:""
         });
    ("bundle frontend_hash is 32 bytes, as well_formed requires",
     String.length (Bundle.frontend_hash "anything") = 32);
    (* Step 8: batched-dispatch codecs (tea.Action, tea.Queued,
       queue.Msg), vectors pinned by CBOR composition against the
       constructor scheme Lucid's Data.to uses. *)
    ("action Single(Increment) wraps the message as constructor 0",
     Queue.single_redeemer (Counter.msg_to_data Counter.Increment)
     = "d8799fd87980ff");
    ("action Batch names the queue script hash as constructor 1",
     Queue.batch_redeemer
       ~queue_hash:"11111111111111111111111111111111111111111111111111111111"
     = Ok
         "d87a9f581c11111111111111111111111111111111111111111111111111111111ff");
    ("action Batch rejects a non-hex queue hash",
     Result.is_error (Queue.batch_redeemer ~queue_hash:"zz"));
    ("queue Process encodes as d87980", Queue.process_redeemer = "d87980");
    ("queue Reclaim encodes as d87a80", Queue.reclaim_redeemer = "d87a80");
    ("queued datum pairs the author hash with the message",
     Queue.queued_datum
       ~author_hash:"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
       ~msg_hex:(Counter.msg_to_data Counter.Reset)
     = Ok
         "d8799f581caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaad87b80ff");
    ("queued datum rejects a non-hex author hash",
     Result.is_error (Queue.queued_datum ~author_hash:"nope" ~msg_hex:"d87980"));
    ("queued_msg_hex extracts the message payload back out",
     Result.bind
       (Queue.queued_datum
          ~author_hash:"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          ~msg_hex:(Counter.msg_to_data Counter.Decrement))
       Queue.queued_msg_hex
     = Ok "d87a80");
    ("queued_msg_hex rejects a payload that is not a queued entry",
     Result.is_error (Queue.queued_msg_hex "d87980"));
    ("counter msg_of_data inverts msg_to_data on every message",
     List.for_all
       (fun msg -> Counter.msg_of_data (Counter.msg_to_data msg) = Ok msg)
       [ Counter.Increment; Counter.Decrement; Counter.Reset ]);
    ("counter msg_of_data rejects an integer payload",
     Result.is_error (Counter.msg_of_data "00"));
    ("registry msg_of_data inverts msg_to_data on publish and retire",
     List.for_all
       (fun msg -> Registry.msg_of_data (Registry.msg_to_data msg) = Ok msg)
       [ Registry.Publish
           { cid = "bafy-v1"; frontend_hash = "0123456789abcdef0123456789abcdef" };
         Registry.Retire
       ]);
    ("registry msg_of_data rejects a malformed constructor",
     Result.is_error (Registry.msg_of_data "d87c80"))
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

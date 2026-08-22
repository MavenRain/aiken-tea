(* Pure hashing for the pinning pipeline: SHA-256 (the multihash inside
   a CIDv1) and BLAKE2b-256 (the registry's frontend_hash, matching
   Aiken's blake2b_256 builtin). Word arithmetic lives in Int32/Int64
   so every operation is exact under 32-bit js_of_ocaml ints; state
   lives in tuples and message schedules in lists read through
   List.nth_opt, whose out-of-range zero would break the pinned
   vectors, so the totality costs no silent-wrong risk. *)

(* The bytes of [text] in [size]-byte chunks; callers pad first, so a
   short trailing chunk never arises here. *)
let byte_chunks ~size text =
  let step (chunk, count, chunks) ch =
    let widened = Char.code ch :: chunk in
    if count + 1 = size then ([], 0, List.rev widened :: chunks)
    else (widened, count + 1, chunks)
  in
  let trailing, _, chunks = String.fold_left step ([], 0, []) text in
  List.rev
    (if trailing = [] then chunks else List.rev trailing :: chunks)

(* Truncating zip: total, unlike List.combine. *)
let rec zip left right =
  match (left, right) with
  | left_head :: left_tail, right_head :: right_tail ->
    (left_head, right_head) :: zip left_tail right_tail
  | [], _ | _, [] -> []

(* Big-endian bytes of a host int that may exceed 32 bits; the double
   16-bit shift keeps every shift count legal on 32-bit ints, as in
   Tea_data.header. *)
let be64_bytes value =
  [ ((value asr 28) asr 28) land 0xff;
    ((value asr 24) asr 24) land 0xff;
    ((value asr 24) asr 16) land 0xff;
    ((value asr 16) asr 16) land 0xff;
    (value lsr 24) land 0xff;
    (value lsr 16) land 0xff;
    (value lsr 8) land 0xff;
    value land 0xff
  ]

(* --- SHA-256 (FIPS 180-4) --- *)

let rotr32 count word =
  Int32.logor
    (Int32.shift_right_logical word count)
    (Int32.shift_left word (32 - count))

let xor3_32 a b c = Int32.logxor (Int32.logxor a b) c

let sha256_k =
  [ 0x428a2f98l; 0x71374491l; 0xb5c0fbcfl; 0xe9b5dba5l;
    0x3956c25bl; 0x59f111f1l; 0x923f82a4l; 0xab1c5ed5l;
    0xd807aa98l; 0x12835b01l; 0x243185bel; 0x550c7dc3l;
    0x72be5d74l; 0x80deb1fel; 0x9bdc06a7l; 0xc19bf174l;
    0xe49b69c1l; 0xefbe4786l; 0x0fc19dc6l; 0x240ca1ccl;
    0x2de92c6fl; 0x4a7484aal; 0x5cb0a9dcl; 0x76f988dal;
    0x983e5152l; 0xa831c66dl; 0xb00327c8l; 0xbf597fc7l;
    0xc6e00bf3l; 0xd5a79147l; 0x06ca6351l; 0x14292967l;
    0x27b70a85l; 0x2e1b2138l; 0x4d2c6dfcl; 0x53380d13l;
    0x650a7354l; 0x766a0abbl; 0x81c2c92el; 0x92722c85l;
    0xa2bfe8a1l; 0xa81a664bl; 0xc24b8b70l; 0xc76c51a3l;
    0xd192e819l; 0xd6990624l; 0xf40e3585l; 0x106aa070l;
    0x19a4c116l; 0x1e376c08l; 0x2748774cl; 0x34b0bcb5l;
    0x391c0cb3l; 0x4ed8aa4al; 0x5b9cca4fl; 0x682e6ff3l;
    0x748f82eel; 0x78a5636fl; 0x84c87814l; 0x8cc70208l;
    0x90befffal; 0xa4506cebl; 0xbef9a3f7l; 0xc67178f2l
  ]

(* Big-endian 32-bit words of one 64-byte block. *)
let words_be block =
  let step (acc, count, words) byte =
    let acc = Int32.logor (Int32.shift_left acc 8) (Int32.of_int byte) in
    if count = 3 then (0l, 0, acc :: words) else (acc, count + 1, words)
  in
  let _, _, words = List.fold_left step (0l, 0, []) block in
  List.rev words

let small_sigma0 word =
  xor3_32 (rotr32 7 word) (rotr32 18 word)
    (Int32.shift_right_logical word 3)

let small_sigma1 word =
  xor3_32 (rotr32 17 word) (rotr32 19 word)
    (Int32.shift_right_logical word 10)

(* W[16..63]. The list is newest-first, so W[t-2] sits at index 1,
   W[t-7] at 6, W[t-15] at 14 and W[t-16] at 15. *)
let rec extend_schedule count newest_first =
  if count = 0 then List.rev newest_first
  else
    let at index =
      List.nth_opt newest_first index |> Option.value ~default:0l
    in
    let word =
      Int32.add
        (Int32.add (small_sigma1 (at 1)) (at 6))
        (Int32.add (small_sigma0 (at 14)) (at 15))
    in
    extend_schedule (count - 1) (word :: newest_first)

let big_sigma0 a = xor3_32 (rotr32 2 a) (rotr32 13 a) (rotr32 22 a)
let big_sigma1 e = xor3_32 (rotr32 6 e) (rotr32 11 e) (rotr32 25 e)

let sha256_round (a, b, c, d, e, f, g, h) (k, w) =
  let choose =
    Int32.logxor (Int32.logand e f) (Int32.logand (Int32.lognot e) g)
  in
  let majority =
    xor3_32 (Int32.logand a b) (Int32.logand a c) (Int32.logand b c)
  in
  let t1 =
    Int32.add
      (Int32.add (Int32.add h (big_sigma1 e)) (Int32.add choose k))
      w
  in
  let t2 = Int32.add (big_sigma0 a) majority in
  (Int32.add t1 t2, a, b, c, Int32.add d t1, e, f, g)

let sha256_block (a0, b0, c0, d0, e0, f0, g0, h0) block =
  let schedule = extend_schedule 48 (List.rev (words_be block)) in
  let a, b, c, d, e, f, g, h =
    List.fold_left sha256_round
      (a0, b0, c0, d0, e0, f0, g0, h0)
      (zip sha256_k schedule)
  in
  ( Int32.add a0 a, Int32.add b0 b, Int32.add c0 c, Int32.add d0 d,
    Int32.add e0 e, Int32.add f0 f, Int32.add g0 g, Int32.add h0 h )

let word_bytes_be word =
  List.map
    (fun shift ->
      Int32.to_int (Int32.shift_right_logical word shift) land 0xff)
    [ 24; 16; 8; 0 ]

(* The 32-byte SHA-256 digest of [text], as a raw byte string. *)
let sha256 text =
  let length = String.length text in
  let padded =
    text ^ "\x80"
    ^ String.make ((55 - length) land 63) '\000'
    ^ Tea_data.string_of_byte_list (be64_bytes (length * 8))
  in
  let a, b, c, d, e, f, g, h =
    List.fold_left sha256_block
      ( 0x6a09e667l, 0xbb67ae85l, 0x3c6ef372l, 0xa54ff53al,
        0x510e527fl, 0x9b05688cl, 0x1f83d9abl, 0x5be0cd19l )
      (byte_chunks ~size:64 padded)
  in
  Tea_data.string_of_byte_list
    (List.concat_map word_bytes_be [ a; b; c; d; e; f; g; h ])

(* --- BLAKE2b-256 (RFC 7693), unkeyed --- *)

let rotr64 count word =
  Int64.logor
    (Int64.shift_right_logical word count)
    (Int64.shift_left word (64 - count))

let iv0 = 0x6a09e667f3bcc908L
let iv1 = 0xbb67ae8584caa73bL
let iv2 = 0x3c6ef372fe94f82bL
let iv3 = 0xa54ff53a5f1d36f1L
let iv4 = 0x510e527fade682d1L
let iv5 = 0x9b05688c2b3e6c1fL
let iv6 = 0x1f83d9abfb41bd6bL
let iv7 = 0x5be0cd19137e2179L

(* The ten message-word permutations; rounds 10 and 11 reuse rows 0
   and 1. *)
let sigma_rounds =
  [ (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15);
    (14, 10, 4, 8, 9, 15, 13, 6, 1, 12, 0, 2, 11, 7, 5, 3);
    (11, 8, 12, 0, 5, 2, 15, 13, 10, 14, 3, 6, 7, 1, 9, 4);
    (7, 9, 3, 1, 13, 12, 11, 14, 2, 6, 5, 10, 4, 0, 15, 8);
    (9, 0, 5, 7, 2, 4, 10, 15, 14, 1, 11, 12, 6, 8, 3, 13);
    (2, 12, 6, 10, 0, 11, 8, 3, 4, 13, 7, 5, 15, 14, 1, 9);
    (12, 5, 1, 15, 14, 13, 4, 10, 0, 7, 6, 3, 9, 2, 8, 11);
    (13, 11, 7, 14, 12, 1, 3, 9, 5, 0, 15, 4, 8, 6, 2, 10);
    (6, 15, 14, 9, 11, 3, 0, 8, 12, 2, 13, 7, 1, 4, 10, 5);
    (10, 2, 8, 4, 7, 6, 1, 5, 15, 11, 9, 14, 3, 12, 13, 0);
    (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15);
    (14, 10, 4, 8, 9, 15, 13, 6, 1, 12, 0, 2, 11, 7, 5, 3)
  ]

let mix (a, b, c, d) x y =
  let a = Int64.add (Int64.add a b) x in
  let d = rotr64 32 (Int64.logxor d a) in
  let c = Int64.add c d in
  let b = rotr64 24 (Int64.logxor b c) in
  let a = Int64.add (Int64.add a b) y in
  let d = rotr64 16 (Int64.logxor d a) in
  let c = Int64.add c d in
  let b = rotr64 63 (Int64.logxor b c) in
  (a, b, c, d)

let blake2b_round message
    (s0, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15)
    (v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15) =
  let at index = List.nth_opt message index |> Option.value ~default:0L in
  let v0, v4, v8, v12 = mix (v0, v4, v8, v12) (at s0) (at s1) in
  let v1, v5, v9, v13 = mix (v1, v5, v9, v13) (at s2) (at s3) in
  let v2, v6, v10, v14 = mix (v2, v6, v10, v14) (at s4) (at s5) in
  let v3, v7, v11, v15 = mix (v3, v7, v11, v15) (at s6) (at s7) in
  let v0, v5, v10, v15 = mix (v0, v5, v10, v15) (at s8) (at s9) in
  let v1, v6, v11, v12 = mix (v1, v6, v11, v12) (at s10) (at s11) in
  let v2, v7, v8, v13 = mix (v2, v7, v8, v13) (at s12) (at s13) in
  let v3, v4, v9, v14 = mix (v3, v4, v9, v14) (at s14) (at s15) in
  (v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15)

(* Little-endian 64-bit words of one 128-byte block. *)
let words_le block =
  let step (acc, shift, words) byte =
    let acc = Int64.logor acc (Int64.shift_left (Int64.of_int byte) shift) in
    if shift = 56 then (0L, 0, acc :: words) else (acc, shift + 8, words)
  in
  let _, _, words = List.fold_left step (0L, 0, []) block in
  List.rev words

let blake2b_compress (h0, h1, h2, h3, h4, h5, h6, h7) message ~offset ~final =
  let start =
    ( h0, h1, h2, h3, h4, h5, h6, h7, iv0, iv1, iv2, iv3,
      Int64.logxor iv4 (Int64.of_int offset), iv5,
      (if final then Int64.lognot iv6 else iv6), iv7 )
  in
  let v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15 =
    List.fold_left
      (fun state row -> blake2b_round message row state)
      start sigma_rounds
  in
  ( Int64.logxor h0 (Int64.logxor v0 v8),
    Int64.logxor h1 (Int64.logxor v1 v9),
    Int64.logxor h2 (Int64.logxor v2 v10),
    Int64.logxor h3 (Int64.logxor v3 v11),
    Int64.logxor h4 (Int64.logxor v4 v12),
    Int64.logxor h5 (Int64.logxor v5 v13),
    Int64.logxor h6 (Int64.logxor v6 v14),
    Int64.logxor h7 (Int64.logxor v7 v15) )

let word_bytes_le word =
  List.map
    (fun shift ->
      Int64.to_int (Int64.shift_right_logical word shift) land 0xff)
    [ 0; 8; 16; 24; 32; 40; 48; 56 ]

(* The 32-byte BLAKE2b-256 digest of [text], as a raw byte string. *)
let blake2b_256 text =
  let length = String.length text in
  let remainder = length land 127 in
  let pad = if length > 0 && remainder = 0 then 0 else 128 - remainder in
  let blocks = byte_chunks ~size:128 (text ^ String.make pad '\000') in
  let block_count = List.length blocks in
  let step (digest, index) block =
    let final = index + 1 = block_count in
    let offset = if final then length else (index + 1) * 128 in
    (blake2b_compress digest (words_le block) ~offset ~final, index + 1)
  in
  let (h0, h1, h2, h3, _, _, _, _), _ =
    List.fold_left step
      ( ( Int64.logxor iv0 0x01010020L, iv1, iv2, iv3, iv4, iv5, iv6, iv7 ),
        0 )
      blocks
  in
  Tea_data.string_of_byte_list
    (List.concat_map word_bytes_le [ h0; h1; h2; h3 ])

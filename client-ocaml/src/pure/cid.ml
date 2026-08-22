(* CIDv1 for a raw block: multibase base32 (lowercase, unpadded) over
   the CID bytes: varint cid-version 1, varint raw codec 0x55, then
   the sha2-256 multihash (code 0x12, length 0x20, digest). One raw
   leaf covers a bundle up to the IPFS chunk size; a bigger bundle
   needs a chunked dag-pb tree, which this probe defers. *)

(* The kubo default chunk size: the largest bundle one raw leaf holds. *)
let max_raw_block = 262144

(* Unsigned LEB128. *)
let varint value =
  let rec bytes value =
    let low = value land 0x7f in
    let rest = value lsr 7 in
    if rest = 0 then [ low ] else (low lor 0x80) :: bytes rest
  in
  Tea_data.string_of_byte_list (bytes value)

let base32_alphabet =
  List.of_seq (String.to_seq "abcdefghijklmnopqrstuvwxyz234567")

(* RFC 4648 base32, lowercase, no padding: fold the bytes through a
   bit buffer, draining five bits per output symbol. An alphabet miss
   cannot happen under the [land 0x1f] masks and would surface as '?'
   in the pinned vectors. *)
let base32 bytes =
  let symbol index =
    List.nth_opt base32_alphabet index |> Option.value ~default:'?'
  in
  let rec drain (acc, bits, out) =
    if bits < 5 then (acc, bits, out)
    else
      let bits = bits - 5 in
      drain
        ( acc land ((1 lsl bits) - 1),
          bits,
          symbol ((acc lsr bits) land 0x1f) :: out )
  in
  let step (acc, bits, out) ch =
    drain (((acc lsl 8) lor Char.code ch, bits + 8, out))
  in
  let acc, bits, out = String.fold_left step (0, 0, []) bytes in
  let out =
    if bits = 0 then out else symbol ((acc lsl (5 - bits)) land 0x1f) :: out
  in
  List.rev out |> List.to_seq |> String.of_seq

(* The CIDv1 a pinning service derives for [bytes] added with
   --cid-version 1 --raw-leaves, provided it fits one raw block. *)
let of_raw_block bytes =
  if String.length bytes > max_raw_block then
    Error
      (Printf.sprintf
         "bundle is %d bytes; one raw block holds at most %d (chunked \
          dag-pb is out of scope)"
         (String.length bytes) max_raw_block)
  else
    Ok
      ("b"
      ^ base32
          (String.concat ""
             [ varint 0x01; varint 0x55; varint 0x12; varint 0x20;
               Hashes.sha256 bytes
             ]))

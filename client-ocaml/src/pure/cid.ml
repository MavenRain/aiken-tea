(* CIDv1 derivation matching `ipfs add --cid-version 1 --raw-leaves`:
   a bundle that fits one chunk is a single raw block (codec 0x55,
   multibase base32 over varint cid-version 1, the codec, and the
   sha2-256 multihash); a bigger bundle chunks into raw leaves joined
   by a balanced dag-pb tree (codec 0x70) whose nodes are UnixFS File
   messages, the layout kubo and the Helia importer share. *)

(* The kubo default chunk size: the largest bundle one raw leaf holds. *)
let max_raw_block = 262144

(* kubo's DefaultLinksPerBlock: the balanced layout's fanout. *)
let dagpb_fanout = 174

(* Unsigned LEB128, which is also the protobuf varint. *)
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

(* The sha2-256 multihash: code 0x12, length 0x20, then the digest. *)
let sha256_multihash bytes =
  String.concat "" [ varint 0x12; varint 0x20; Hashes.sha256 bytes ]

(* A subtree of the file DAG: its binary CIDv1, the encoded size of
   the whole subtree (what dag-pb links call Tsize), and the file
   bytes it covers (what UnixFS calls filesize). At a raw leaf the two
   sizes coincide; above one they diverge, so confusing them breaks
   the two-level pinned vectors. *)
type node = { cid_bytes : string; tsize : int; filesize : int }

let raw_leaf bytes =
  let size = String.length bytes in
  { cid_bytes =
      String.concat "" [ varint 0x01; varint 0x55; sha256_multihash bytes ];
    tsize = size;
    filesize = size
  }

(* Protobuf wire helpers: a key is the field number shifted over the
   wire type (0 varint, 2 length-delimited). *)
let pb_uint field value = String.concat "" [ varint (field lsl 3); varint value ]

let pb_bytes field payload =
  String.concat ""
    [ varint ((field lsl 3) lor 2); varint (String.length payload); payload ]

(* The UnixFS Data message of an intermediate node: Type=File, the
   total filesize, and one blocksize per link, fields in number
   order. *)
let unixfs_file ~filesize ~blocksizes =
  String.concat ""
    (pb_uint 1 2 :: pb_uint 3 filesize :: List.map (pb_uint 4) blocksizes)

(* A PBLink: Hash (the child's binary CID), an empty Name, and Tsize,
   the three fields the reference importers emit for file DAGs. *)
let pb_link child =
  String.concat ""
    [ pb_bytes 1 child.cid_bytes; pb_bytes 2 ""; pb_uint 3 child.tsize ]

(* One dag-pb parent over [children]: links (field 2) precede Data
   (field 1), the order the dag-pb spec fixes despite the field
   numbers. Tsize adds this node's own encoded bytes to its
   children's. *)
let dagpb_parent children =
  let blocksizes = List.map (fun child -> child.filesize) children in
  let filesize = List.fold_left ( + ) 0 blocksizes in
  let encoded =
    String.concat ""
      (List.map (fun child -> pb_bytes 2 (pb_link child)) children
      @ [ pb_bytes 1 (unixfs_file ~filesize ~blocksizes) ])
  in
  { cid_bytes =
      String.concat "" [ varint 0x01; varint 0x70; sha256_multihash encoded ];
    tsize =
      List.fold_left
        (fun acc child -> acc + child.tsize)
        (String.length encoded) children;
    filesize
  }

(* [text] in [size]-byte string chunks, in order, the trailing chunk
   short: the fold shape of Hashes.byte_chunks. Callers clamp [size]
   to at least 1, so the flush comparison always fires. *)
let string_chunks ~size text =
  let step (chunk, count, chunks) ch =
    let widened = ch :: chunk in
    if count + 1 = size then ([], 0, List.rev widened :: chunks)
    else (widened, count + 1, chunks)
  in
  let trailing, _, chunks = String.fold_left step ([], 0, []) text in
  List.rev_map
    (fun chars -> List.to_seq chars |> String.of_seq)
    (if trailing = [] then chunks else List.rev trailing :: chunks)

(* [items] in groups of at most [size], order kept. Callers clamp
   [size] to at least 2, which makes each pass strictly shrink any
   list of two or more. *)
let grouped ~size items =
  let step (group, count, groups) item =
    let widened = item :: group in
    if count + 1 = size then ([], 0, List.rev widened :: groups)
    else (widened, count + 1, groups)
  in
  let trailing, _, groups = List.fold_left step ([], 0, []) items in
  List.rev (if trailing = [] then groups else List.rev trailing :: groups)

(* The balanced layout: leaves fill parents of at most [fanout]
   children per level until one root remains, every leaf at the same
   depth (a trailing lone leaf still gets its own parent, as the
   ramp-9 pinned vector confirms). *)
let rec balanced ~fanout nodes =
  match nodes with
  | [] -> raw_leaf ""
  | [ root ] -> root
  | _ :: _ :: _ ->
    balanced ~fanout (List.map dagpb_parent (grouped ~size:fanout nodes))

(* The CIDv1 a pinning service derives for [bytes] at the given
   parameters: total over any size, unlike [of_raw_block]. The
   parameters are exposed so tests can pin multi-level trees against
   the ipfs-unixfs-importer oracle without megabyte inputs. *)
let of_bundle_with ~chunk_size ~fanout bytes =
  let chunk_size = max 1 chunk_size in
  let root =
    if String.length bytes <= chunk_size then raw_leaf bytes
    else
      balanced ~fanout:(max 2 fanout)
        (List.map raw_leaf (string_chunks ~size:chunk_size bytes))
  in
  "b" ^ base32 root.cid_bytes

(* The CIDv1 for [bytes] added with --cid-version 1 --raw-leaves under
   the kubo defaults, whatever the bundle size. *)
let of_bundle bytes =
  of_bundle_with ~chunk_size:max_raw_block ~fanout:dagpb_fanout bytes

(* The single-raw-block special case, kept as a Result so callers that
   promise one block (and the pinned boundary tests) still have the
   explicit rejection; [of_bundle] chunks instead of rejecting. *)
let of_raw_block bytes =
  if String.length bytes > max_raw_block then
    Error
      (Printf.sprintf
         "bundle is %d bytes; one raw block holds at most %d (of_bundle \
          chunks it instead)"
         (String.length bytes) max_raw_block)
  else Ok ("b" ^ base32 (raw_leaf bytes).cid_bytes)

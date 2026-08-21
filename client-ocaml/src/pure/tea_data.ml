(* Plutus Data: the subset the TEA client needs (constructors and
   integers), with the CBOR wire codec Lucid and the ledger use.
   Constructor tags follow the ledger alternatives: 121+i for indices
   0-6, 1280+(i-7) for 7-127, the general tag-102 pair otherwise.
   Empty field lists are definite-length, non-empty ones indefinite,
   matching the serialization the on-chain world produces. *)

type t =
  | Int of int
  | Constr of int * t list

type error =
  | Bad_hex of string
  | Truncated
  | Unsupported of string
  | Overflow
  | Trailing of int

let error_to_string error =
  match error with
  | Bad_hex what -> "bad hex: " ^ what
  | Truncated -> "truncated CBOR"
  | Unsupported what -> "unsupported CBOR: " ^ what
  | Overflow -> "integer too large for this host"
  | Trailing count -> Printf.sprintf "%d trailing bytes after value" count

let rec to_string value =
  match value with
  | Int n -> string_of_int n
  | Constr (index, fields) ->
    Printf.sprintf "Constr(%d, [%s])" index
      (String.concat "; " (List.map to_string fields))

(* CBOR header for one major type and unsigned argument. The 8-byte
   form only arises for values past 32 bits, which exist on 63-bit
   hosts only; the double 16-bit shift keeps every shift count legal
   on 32-bit js_of_ocaml ints too. *)
let header major value =
  let tag info = (major lsl 5) lor info in
  if value < 24 then [ tag value ]
  else if value < 0x100 then [ tag 24; value ]
  else if value < 0x10000 then [ tag 25; value lsr 8; value land 0xff ]
  else if (value asr 16) asr 16 = 0 then
    [ tag 26;
      (value lsr 24) land 0xff;
      (value lsr 16) land 0xff;
      (value lsr 8) land 0xff;
      value land 0xff ]
  else
    [ tag 27;
      ((value asr 28) asr 28) land 0xff;
      ((value asr 24) asr 24) land 0xff;
      ((value asr 24) asr 16) land 0xff;
      ((value asr 16) asr 16) land 0xff;
      (value lsr 24) land 0xff;
      (value lsr 16) land 0xff;
      (value lsr 8) land 0xff;
      value land 0xff ]

let fields_bytes encoded_fields =
  match encoded_fields with
  | [] -> [ 0x80 ]
  | _ :: _ -> (0x9f :: List.concat encoded_fields) @ [ 0xff ]

let rec to_bytes value =
  match value with
  | Int n -> if n >= 0 then header 0 n else header 1 (-1 - n)
  | Constr (index, fields) ->
    let encoded = List.map to_bytes fields in
    if 0 <= index && index <= 6 then
      header 6 (121 + index) @ fields_bytes encoded
    else if 7 <= index && index <= 127 then
      header 6 (1280 + (index - 7)) @ fields_bytes encoded
    else
      header 6 102 @ (0x82 :: to_bytes (Int index)) @ fields_bytes encoded

let encode value =
  to_bytes value |> List.map (Printf.sprintf "%02x") |> String.concat ""

let nibble ch =
  match ch with
  | '0' .. '9' -> Some (Char.code ch - Char.code '0')
  | 'a' .. 'f' -> Some (Char.code ch - Char.code 'a' + 10)
  | 'A' .. 'F' -> Some (Char.code ch - Char.code 'A' + 10)
  | _ -> None

let bytes_of_hex text =
  let step acc ch =
    Result.bind acc (fun (rev_bytes, pending) ->
      nibble ch
      |> Option.fold
           ~none:(Error (Bad_hex (String.make 1 ch)))
           ~some:(fun low ->
             pending
             |> Option.fold
                  ~none:(Ok (rev_bytes, Some low))
                  ~some:(fun high ->
                    Ok (((high lsl 4) lor low) :: rev_bytes, None))))
  in
  Result.bind
    (String.fold_left step (Ok ([], None)) text)
    (fun (rev_bytes, pending) ->
      pending
      |> Option.fold
           ~none:(Ok (List.rev rev_bytes))
           ~some:(fun _ -> Error (Bad_hex "odd length")))

(* Read [count] bytes as a big-endian unsigned int, refusing values the
   host int cannot hold. *)
let rec take_uint count acc bytes =
  if count = 0 then Ok (acc, bytes)
  else
    match bytes with
    | [] -> Error Truncated
    | byte :: rest ->
      if acc > max_int asr 8 then Error Overflow
      else take_uint (count - 1) ((acc lsl 8) lor byte) rest

(* CBOR argument for a header byte: Ok (Some value, rest) for definite
   forms, Ok (None, rest) for the indefinite marker. *)
let argument info bytes =
  if info < 24 then Ok (Some info, bytes)
  else if info = 24 then
    Result.map (fun (v, rest) -> (Some v, rest)) (take_uint 1 0 bytes)
  else if info = 25 then
    Result.map (fun (v, rest) -> (Some v, rest)) (take_uint 2 0 bytes)
  else if info = 26 then
    Result.map (fun (v, rest) -> (Some v, rest)) (take_uint 4 0 bytes)
  else if info = 27 then
    Result.map (fun (v, rest) -> (Some v, rest)) (take_uint 8 0 bytes)
  else if info = 31 then Ok (None, bytes)
  else Error (Unsupported (Printf.sprintf "additional info %d" info))

let rec parse bytes =
  match bytes with
  | [] -> Error Truncated
  | byte :: rest ->
    let major = byte lsr 5 in
    let info = byte land 0x1f in
    Result.bind (argument info rest) (fun (arg, rest) ->
      match (major, arg) with
      | 0, Some value -> Ok (Int value, rest)
      | 1, Some value -> Ok (Int (-1 - value), rest)
      | 6, Some tag -> parse_tagged tag rest
      | 0, None | 1, None | 6, None ->
        Error (Unsupported "indefinite integer or tag")
      | _, _ -> Error (Unsupported (Printf.sprintf "major type %d" major)))

and parse_tagged tag bytes =
  if 121 <= tag && tag <= 127 then
    Result.map
      (fun (fields, rest) -> (Constr (tag - 121, fields), rest))
      (parse_array bytes)
  else if 1280 <= tag && tag <= 1400 then
    Result.map
      (fun (fields, rest) -> (Constr (tag - 1280 + 7, fields), rest))
      (parse_array bytes)
  else if tag = 102 then
    match bytes with
    | [] -> Error Truncated
    | byte :: rest ->
      if byte <> 0x82 then Error (Unsupported "tag 102 shape")
      else
        Result.bind (parse rest) (fun (index_item, rest) ->
          match index_item with
          | Int index ->
            Result.map
              (fun (fields, rest) -> (Constr (index, fields), rest))
              (parse_array rest)
          | Constr (_, _) -> Error (Unsupported "tag 102 index"))
  else Error (Unsupported (Printf.sprintf "tag %d" tag))

(* An array of Data items: definite or indefinite length. *)
and parse_array bytes =
  match bytes with
  | [] -> Error Truncated
  | byte :: rest ->
    let major = byte lsr 5 in
    let info = byte land 0x1f in
    if major <> 4 then Error (Unsupported "expected array")
    else
      Result.bind (argument info rest) (fun (arg, rest) ->
        arg
        |> Option.fold
             ~none:(parse_indefinite [] rest)
             ~some:(fun count -> parse_definite count [] rest))

and parse_definite remaining acc bytes =
  if remaining = 0 then Ok (List.rev acc, bytes)
  else
    Result.bind (parse bytes) (fun (item, rest) ->
      parse_definite (remaining - 1) (item :: acc) rest)

and parse_indefinite acc bytes =
  match bytes with
  | [] -> Error Truncated
  | 0xff :: rest -> Ok (List.rev acc, rest)
  | _ :: _ ->
    Result.bind (parse bytes) (fun (item, rest) ->
      parse_indefinite (item :: acc) rest)

let decode text =
  Result.bind (bytes_of_hex text) (fun bytes ->
    Result.bind (parse bytes) (fun (value, rest) ->
      match rest with
      | [] -> Ok value
      | _ :: _ -> Error (Trailing (List.length rest))))

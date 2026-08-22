(* The pinning pipeline, pure half: a frontend bundle's raw bytes
   determine both payloads of a Publish message. The CID is what the
   bundle pins under on IPFS; the frontend_hash is the blake2b-256
   digest the validator length-checks on-chain and a browser client
   verifies the fetched bundle against. *)

let frontend_hash bytes = Hashes.blake2b_256 bytes
let cid bytes = Cid.of_raw_block bytes

let publish_msg bytes =
  Result.map
    (fun cid -> Registry.Publish { cid; frontend_hash = frontend_hash bytes })
    (cid bytes)

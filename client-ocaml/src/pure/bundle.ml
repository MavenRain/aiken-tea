(* The pinning pipeline, pure half: a frontend bundle's raw bytes
   determine both payloads of a Publish message, at any bundle size.
   The CID is what the bundle pins under on IPFS (one raw block when
   it fits a chunk, a balanced dag-pb tree past that); the
   frontend_hash is the blake2b-256 digest the validator
   length-checks on-chain and a browser client verifies the fetched
   bundle against. *)

let frontend_hash bytes = Hashes.blake2b_256 bytes
let cid bytes = Cid.of_bundle bytes

let publish_msg bytes =
  Registry.Publish { cid = cid bytes; frontend_hash = frontend_hash bytes }

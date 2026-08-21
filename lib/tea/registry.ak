//// IPFS deployment registry: a TEA application whose model is the
//// deployed frontend. `cid` is the IPFS content id of the bundle,
//// `frontend_hash` is the blake2b-256 hash of the bundle, and `version`
//// counts upgrades. A CIP-68-style reference NFT marks the state UTxO;
//// the spend policy is the generic TEA transition plus an owner
//// signature and NFT preservation.

use aiken/collection/dict
use aiken/collection/list
use aiken/crypto.{VerificationKeyHash}
use aiken/primitive/bytearray
use cardano/address.{Script}
use cardano/assets.{PolicyId}
use cardano/transaction.{
  InlineDatum, Input, Output, OutputReference, Transaction,
}
use tea

/// CIP-67 label (100) prefix, then "aiken-tea": the reference-token name
/// that marks the registry state UTxO.
pub const reference_token_name: ByteArray = #"000643b061696b656e2d746561"

pub type Model {
  cid: ByteArray,
  version: Int,
  frontend_hash: ByteArray,
}

pub type Msg {
  Publish { cid: ByteArray, frontend_hash: ByteArray }
}

/// The pure TEA step function: a publish replaces the deployment and
/// bumps the version by exactly one.
pub fn update(msg: Msg, model: Model) -> Model {
  when msg is {
    Publish { cid, frontend_hash } ->
      Model { cid, version: model.version + 1, frontend_hash }
  }
}

/// A publishable payload: a non-empty CID and a 32-byte blake2b-256
/// bundle hash.
pub fn well_formed(cid: ByteArray, frontend_hash: ByteArray) -> Bool {
  and {
    bytearray.length(cid) > 0,
    bytearray.length(frontend_hash) == 32,
  }
}

/// Upgrade policy: the transaction is signed by the registry owner, the
/// payload is well formed, and the generic TEA transition holds with the
/// reference NFT staying on the state UTxO.
pub fn registry_spend(
  owner: VerificationKeyHash,
  datum: Option<Model>,
  redeemer: Msg,
  own_ref: OutputReference,
  self: Transaction,
) -> Bool {
  expect Some(model) = datum
  let Publish { cid, frontend_hash } = redeemer
  let next: Data = update(redeemer, model)
  and {
    list.has(self.extra_signatories, owner),
    well_formed(cid, frontend_hash),
    tea.transition_with(next, own_ref, self, keeps_reference_token),
  }
}

/// The continuing state output must carry the reference NFT minted under
/// the registry's own script hash.
fn keeps_reference_token(own_input: Input, own_output: Output) -> Bool {
  expect Script(own_hash) = own_input.output.address.payment_credential
  assets.quantity_of(own_output.value, own_hash, reference_token_name) == 1
}

/// Genesis: a one-shot mint of the reference NFT. The transaction must
/// consume the `seed` UTxO (so the mint can never repeat), mint exactly
/// one token named `reference_token_name` under this policy, and lock it
/// at the registry script with a well-formed version-0 inline datum.
pub fn registry_mint(
  seed: OutputReference,
  policy_id: PolicyId,
  self: Transaction,
) -> Bool {
  expect [Pair(minted_name, minted_quantity)] = assets.tokens(
    self.mint,
    policy_id,
  )
    |> dict.to_pairs
  expect [genesis] = list.filter(
    self.outputs,
    fn(out) { out.address.payment_credential == Script(policy_id) },
  )
  expect InlineDatum(genesis_data) = genesis.datum
  expect genesis_model: Model = genesis_data
  and {
    list.any(self.inputs, fn(input) { input.output_reference == seed }),
    minted_name == reference_token_name,
    minted_quantity == 1,
    assets.quantity_of(genesis.value, policy_id, reference_token_name) == 1,
    genesis_model.version == 0,
    well_formed(genesis_model.cid, genesis_model.frontend_hash),
  }
}

test update_publish_bumps_version() {
  let model = Model { cid: "bafy-v1", version: 4, frontend_hash: "old-hash" }
  update(Publish { cid: "bafy-v2", frontend_hash: "new-hash" }, model) == Model {
    cid: "bafy-v2",
    version: 5,
    frontend_hash: "new-hash",
  }
}

test well_formed_accepts_32_byte_hash() {
  well_formed(
    "bafy-v1",
    #"0000000000000000000000000000000000000000000000000000000000000000",
  )
}

test well_formed_rejects_empty_cid() {
  !well_formed(
    "",
    #"0000000000000000000000000000000000000000000000000000000000000000",
  )
}

test well_formed_rejects_short_hash() {
  !well_formed("bafy-v1", #"00")
}

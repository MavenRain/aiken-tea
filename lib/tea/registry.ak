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
use cardano/transaction.{Input, Output, OutputReference, Transaction}
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
  Retire
}

/// The pure TEA step function: a publish replaces the deployment and
/// bumps the version by exactly one; a retire ends the application, so
/// there is no next model.
pub fn update(msg: Msg, model: Model) -> Option<Model> {
  when msg is {
    Publish { cid, frontend_hash } ->
      Some(Model { cid, version: model.version + 1, frontend_hash })
    Retire -> None
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

/// Upgrade or retirement, decided by recomputing `update`. Either way
/// the transaction must be signed by the registry owner. A `Some` step
/// is a publish: the payload must be well formed and the generic TEA
/// transition must hold with the reference NFT staying on the state
/// UTxO. A `None` step is the retirement: the generic halt must hold
/// (no continuing state output) and the reference NFT must be burned.
pub fn registry_spend(
  owner: VerificationKeyHash,
  datum: Option<Model>,
  redeemer: Msg,
  own_ref: OutputReference,
  self: Transaction,
) -> Bool {
  expect Some(model) = datum
  and {
    list.has(self.extra_signatories, owner),
    when update(redeemer, model) is {
      Some(next) -> {
        expect Publish { cid, frontend_hash } = redeemer
        let next_data: Data = next
        and {
          well_formed(cid, frontend_hash),
          tea.transition_with(next_data, own_ref, self, keeps_reference_token),
        }
      }
      None ->
        tea.halt(
          own_ref,
          self,
          fn(own_input) { burns_reference_token(own_input, self) },
        )
    },
  }
}

/// The continuing state output must carry the reference NFT minted under
/// the registry's own script hash.
fn keeps_reference_token(own_input: Input, own_output: Output) -> Bool {
  expect Script(own_hash) = own_input.output.address.payment_credential
  assets.quantity_of(own_output.value, own_hash, reference_token_name) == 1
}

/// The retirement burn: the transaction must burn exactly the one
/// reference NFT minted under the registry's own script hash.
fn burns_reference_token(own_input: Input, self: Transaction) -> Bool {
  expect Script(own_hash) = own_input.output.address.payment_credential
  assets.quantity_of(self.mint, own_hash, reference_token_name) == -1
}

/// Genesis or retirement, decided by the minted quantity. Genesis is
/// the generic one-shot `tea.genesis` (seed consumed, one token named
/// `reference_token_name`, locked at the registry script) with the
/// registry's datum policy: a well-formed version-0 model. The
/// retirement burn only checks the token name: the token lives on the
/// state UTxO, so a burn always spends that UTxO, and the spend handler
/// carries the retirement policy.
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
  when minted_quantity is {
    -1 -> minted_name == reference_token_name
    _ -> tea.genesis(seed, policy_id, reference_token_name, version_zero, self)
  }
}

/// The genesis datum policy: a well-formed version-0 model.
fn version_zero(state_data: Data) -> Bool {
  expect model: Model = state_data
  and {
    model.version == 0,
    well_formed(model.cid, model.frontend_hash),
  }
}

test update_publish_bumps_version() {
  let model = Model { cid: "bafy-v1", version: 4, frontend_hash: "old-hash" }
  update(Publish { cid: "bafy-v2", frontend_hash: "new-hash" }, model) == Some(
    Model { cid: "bafy-v2", version: 5, frontend_hash: "new-hash" },
  )
}

test update_retire_ends_the_application() {
  let model = Model { cid: "bafy-v1", version: 4, frontend_hash: "old-hash" }
  update(Retire, model) == None
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

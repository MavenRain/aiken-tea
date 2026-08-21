use aiken/crypto.{VerificationKeyHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use tea/registry.{Model, Msg, registry_mint, registry_spend} as registry_app

/// One script, two purposes: the mint handler creates the reference NFT
/// once (the policy id is this script's own hash), the spend handler
/// enforces the upgrade policy on the state UTxO the NFT marks.
validator registry(owner: VerificationKeyHash, seed: OutputReference) {
  mint(_redeemer: Data, policy_id: PolicyId, self: Transaction) {
    registry_mint(seed, policy_id, self)
  }

  spend(
    datum: Option<Model>,
    redeemer: Msg,
    own_ref: OutputReference,
    self: Transaction,
  ) {
    registry_spend(owner, datum, redeemer, own_ref, self)
  }

  else(_) {
    fail
  }
}

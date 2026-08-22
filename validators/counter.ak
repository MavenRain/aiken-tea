use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use tea.{Action, Batch, Single}
use tea/counter.{Model,
  Msg, counter_batch, counter_mint, counter_spend} as counter_app

/// One script, two purposes: the mint handler creates the reference NFT
/// once (the policy id is this script's own hash), the spend handler
/// verifies one TEA step (or one batched fold) and keeps the NFT on the
/// continuing state UTxO.
validator counter(seed: OutputReference) {
  mint(_redeemer: Data, policy_id: PolicyId, self: Transaction) {
    counter_mint(seed, policy_id, self)
  }

  spend(
    datum: Option<Model>,
    redeemer: Action<Msg>,
    own_ref: OutputReference,
    self: Transaction,
  ) {
    when redeemer is {
      Single(msg) -> counter_spend(datum, msg, own_ref, self)
      Batch { queue } -> counter_batch(datum, queue, own_ref, self)
    }
  }

  else(_) {
    fail
  }
}

//// Message queue for batched TEA dispatch. A user who would otherwise
//// race for the state UTxO locks the message here instead, as a
//// `tea.Queued` inline datum. A batcher consumes entries together with
//// the state UTxO (`Process`); the state validator, not this one,
//// enforces that the folded messages produce the new state, so this
//// script only has to pin each entry to a state transition or give it
//// back to its author (`Reclaim`). The entry's min-ada is the batcher's
//// fee.
////
//// `Process` demands a state input that carries a token minted under
//// the state script's own policy, not just one at the state address:
//// anyone can pay a look-alike UTxO to that address, but only the state
//// script's mint handler can create such a token, and both shipped apps
//// mint it exactly once at a one-shot genesis (`tea.genesis`) and pin
//// it to the state on every transition (`tea.spend_with`,
//// `tea.batch_with`). A batch-capable app must mark its state this way,
//// or no entry of its queue can ever be processed.

use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/address.{Script}
use cardano/assets
use cardano/transaction.{Transaction}
use tea.{Queued}

pub type Msg {
  /// Spend the entry into a batched state transition: the transaction
  /// must also consume an input at the state script this queue serves.
  Process
  /// The author takes the entry (and its min-ada) back with a signature.
  Reclaim
}

/// Spend logic for one queue entry. Kept in the library so tests can
/// call it directly.
pub fn queue_spend(
  state: ScriptHash,
  datum: Option<Queued>,
  redeemer: Msg,
  self: Transaction,
) -> Bool {
  when redeemer is {
    Process ->
      list.any(
        self.inputs,
        fn(input) {
          and {
            input.output.address.payment_credential == Script(state),
            list.has(assets.policies(input.output.value), state),
          }
        },
      )
    Reclaim -> {
      expect Some(queued) = datum
      list.has(self.extra_signatories, queued.author)
    }
  }
}

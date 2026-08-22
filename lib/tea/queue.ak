//// Message queue for batched TEA dispatch. A user who would otherwise
//// race for the state UTxO locks the message here instead, as a
//// `tea.Queued` inline datum. A batcher consumes entries together with
//// the state UTxO (`Process`); the state validator, not this one,
//// enforces that the folded messages produce the new state, so this
//// script only has to pin each entry to a state transition or give it
//// back to its author (`Reclaim`). The entry's min-ada is the batcher's
//// fee.
////
//// Known limit of the probe: `Process` proves an input at the state
//// script exists, but a bare app like the counter has no genesis NFT,
//// so anyone can stage a look-alike state UTxO at the state address and
//// drain entries into it (censorship plus min-ada capture, never a
//// forged state: the real state UTxO is untouched). An app that marks
//// its state with a reference NFT (the registry pattern) closes this by
//// checking the token in `keeps`.

use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/address.{Script}
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
        fn(input) { input.output.address.payment_credential == Script(state) },
      )
    Reclaim -> {
      expect Some(queued) = datum
      list.has(self.extra_signatories, queued.author)
    }
  }
}

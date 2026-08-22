use aiken/crypto.{ScriptHash}
use cardano/transaction.{OutputReference, Transaction}
use tea.{Queued}
use tea/queue.{Msg, queue_spend} as queue_app

/// One queue per state script: `state` is the script hash of the TEA
/// app whose messages this queue holds.
validator queue(state: ScriptHash) {
  spend(
    datum: Option<Queued>,
    redeemer: Msg,
    _own_ref: OutputReference,
    self: Transaction,
  ) {
    queue_spend(state, datum, redeemer, self)
  }

  else(_) {
    fail
  }
}

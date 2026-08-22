use cardano/transaction.{OutputReference, Transaction}
use tea.{Action, Batch, Single}
use tea/counter.{Model, Msg, counter_batch, counter_spend} as counter_app

validator counter {
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

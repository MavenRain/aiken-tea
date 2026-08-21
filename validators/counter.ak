use cardano/transaction.{OutputReference, Transaction}
use tea/counter.{Model, Msg, counter_spend} as counter_app

validator counter {
  spend(
    datum: Option<Model>,
    redeemer: Msg,
    own_ref: OutputReference,
    self: Transaction,
  ) {
    counter_spend(datum, redeemer, own_ref, self)
  }

  else(_) {
    fail
  }
}

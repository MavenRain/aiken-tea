//// Counter: the smallest TEA application. `Model` is a single count,
//// `Msg` is increment / decrement / reset.

use cardano/transaction.{OutputReference, Transaction}
use tea

pub type Model {
  count: Int,
}

pub type Msg {
  Increment
  Decrement
  Reset
}

/// The pure TEA step function. This is the whole application.
pub fn update(msg: Msg, model: Model) -> Model {
  when msg is {
    Increment -> Model { count: model.count + 1 }
    Decrement -> Model { count: model.count - 1 }
    Reset -> Model { count: 0 }
  }
}

/// Spend logic for the counter, delegated to the generic TEA check.
/// Kept in the library so tests can call it directly.
pub fn counter_spend(
  datum: Option<Model>,
  redeemer: Msg,
  own_ref: OutputReference,
  self: Transaction,
) -> Bool {
  tea.spend(step, datum, redeemer, own_ref, self)
}

/// `update` with its result upcast to `Data`, the shape `tea.spend` needs.
fn step(msg: Msg, model: Model) -> Data {
  let next: Data = update(msg, model)
  next
}

test update_increment() {
  update(Increment, Model { count: 41 }) == Model { count: 42 }
}

test update_decrement() {
  update(Decrement, Model { count: 42 }) == Model { count: 41 }
}

test update_reset() {
  update(Reset, Model { count: 42 }) == Model { count: 0 }
}

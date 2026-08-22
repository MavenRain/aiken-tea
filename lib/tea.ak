//// Generic Elm-Architecture (TEA) transition check for spend validators.
////
//// The validator does not run the application. It verifies one step of it:
//// the transaction that spends the state UTxO must recreate it at the same
//// address with datum `update(msg, model)` and with no lovelace removed.
//// `update` is the pure TEA step function, `Msg` is the redeemer and
//// `Model` is the inline datum.

use aiken/collection/list
use cardano/assets
use cardano/transaction.{
  InlineDatum, Input, Output, OutputReference, Transaction, find_input,
}

/// Full TEA spend check, generic in the application's `model` and `msg`
/// types. `step` is the application's `update` with its result upcast to
/// `Data` at the concrete call site (the compiler rejects an upcast of an
/// unbound generic here). Fails (invalid transaction) when the datum is
/// missing, the state output is absent or duplicated, or the proposed
/// datum is not the one `step` computes.
pub fn spend(
  step: fn(msg, model) -> Data,
  datum: Option<model>,
  redeemer: msg,
  own_ref: OutputReference,
  self: Transaction,
) -> Bool {
  expect Some(model) = datum
  transition(step(redeemer, model), own_ref, self)
}

/// Core check, over an already-computed expected datum. Enforces:
/// exactly one continuing output at the script's own address, carrying
/// `expected` as an inline datum, and at least as much lovelace as the
/// input held.
pub fn transition(
  expected: Data,
  own_ref: OutputReference,
  self: Transaction,
) -> Bool {
  transition_with(expected, own_ref, self, fn(_own_input, _own_output) { True })
}

/// `transition` with an application-specific guard over the state input
/// and its continuing output, for policies the generic check cannot know
/// (a state token that must stay with the UTxO, for example). The guard
/// runs on top of the datum and lovelace checks, never instead of them.
pub fn transition_with(
  expected: Data,
  own_ref: OutputReference,
  self: Transaction,
  keeps: fn(Input, Output) -> Bool,
) -> Bool {
  expect Some(own_input) = find_input(self.inputs, own_ref)
  let own_address = own_input.output.address
  expect [own_output] = list.filter(
    self.outputs,
    fn(out) { out.address == own_address },
  )
  expect InlineDatum(out_data) = own_output.datum
  and {
    out_data == expected,
    assets.lovelace_of(own_output.value) >= assets.lovelace_of(
      own_input.output.value,
    ),
    keeps(own_input, own_output),
  }
}

/// Terminal TEA step: `update` returned `None`, so the application ends.
/// The spending transaction must leave no output at the script's own
/// address. `releases` is the application guard over the state input,
/// for teardown policy the generic check cannot know (a state-token
/// burn, for example).
pub fn halt(
  own_ref: OutputReference,
  self: Transaction,
  releases: fn(Input) -> Bool,
) -> Bool {
  expect Some(own_input) = find_input(self.inputs, own_ref)
  let own_address = own_input.output.address
  and {
    !list.any(self.outputs, fn(out) { out.address == own_address }),
    releases(own_input),
  }
}

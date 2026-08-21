//// Transaction-level tests for the counter app driven through the generic
//// `tea.transition` check (via `counter_spend`).

use cardano/address.{from_script}
use cardano/assets.{from_lovelace}
use cardano/transaction.{
  InlineDatum, Input, NoDatum, Output, OutputReference, Transaction, placeholder,
}
use tea/counter.{Increment, Model, Reset, counter_spend}

const own_ref =
  OutputReference {
    transaction_id: #"0000000000000000000000000000000000000000000000000000000000000000",
    output_index: 0,
  }

fn script_address() {
  from_script(#"00000000000000000000000000000000000000000000000000000000")
}

fn state_output(count: Int, lovelace: Int) -> Output {
  let model_data: Data = Model { count }
  Output {
    address: script_address(),
    value: from_lovelace(lovelace),
    datum: InlineDatum(model_data),
    reference_script: None,
  }
}

fn state_input(count: Int) -> Input {
  Input { output_reference: own_ref, output: state_output(count, 2_000_000) }
}

fn spend_tx(outputs: List<Output>) -> Transaction {
  Transaction { ..placeholder, inputs: [state_input(41)], outputs }
}

test increment_accepted() {
  counter_spend(
    Some(Model { count: 41 }),
    Increment,
    own_ref,
    spend_tx([state_output(42, 2_000_000)]),
  )
}

test reset_accepted() {
  counter_spend(
    Some(Model { count: 41 }),
    Reset,
    own_ref,
    spend_tx([state_output(0, 2_000_000)]),
  )
}

test extra_lovelace_accepted() {
  counter_spend(
    Some(Model { count: 41 }),
    Increment,
    own_ref,
    spend_tx([state_output(42, 3_000_000)]),
  )
}

test wrong_datum_rejected() {
  !counter_spend(
    Some(Model { count: 41 }),
    Increment,
    own_ref,
    spend_tx([state_output(43, 2_000_000)]),
  )
}

test lovelace_drain_rejected() {
  !counter_spend(
    Some(Model { count: 41 }),
    Increment,
    own_ref,
    spend_tx([state_output(42, 1_500_000)]),
  )
}

test missing_datum_rejected() fail {
  counter_spend(
    None,
    Increment,
    own_ref,
    spend_tx([state_output(42, 2_000_000)]),
  )
}

test split_state_rejected() fail {
  counter_spend(
    Some(Model { count: 41 }),
    Increment,
    own_ref,
    spend_tx([state_output(42, 2_000_000), state_output(42, 2_000_000)]),
  )
}

test vanished_state_rejected() fail {
  counter_spend(Some(Model { count: 41 }), Increment, own_ref, spend_tx([]))
}

test non_inline_datum_rejected() fail {
  let bare = Output {
    address: script_address(),
    value: from_lovelace(2_000_000),
    datum: NoDatum,
    reference_script: None,
  }
  counter_spend(Some(Model { count: 41 }), Increment, own_ref, spend_tx([bare]))
}

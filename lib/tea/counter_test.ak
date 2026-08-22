//// Transaction-level tests for the counter app driven through the generic
//// `tea.transition_with` check (via `counter_spend` / `counter_batch`)
//// and the one-shot genesis mint (`counter_mint`).

use cardano/address.{from_script, from_verification_key}
use cardano/assets.{add, from_asset, from_lovelace}
use cardano/transaction.{
  InlineDatum, Input, NoDatum, Output, OutputReference, Transaction, placeholder,
}
use tea.{Queued}
use tea/counter.{
  Decrement, Increment, Model, Msg, Reset, counter_batch, counter_mint,
  counter_spend, reference_token_name, update,
}

const script_hash = #"00000000000000000000000000000000000000000000000000000000"

const own_ref =
  OutputReference {
    transaction_id: #"0000000000000000000000000000000000000000000000000000000000000000",
    output_index: 0,
  }

fn script_address() {
  from_script(script_hash)
}

/// A state output carrying the reference NFT, the shape every honest
/// transition recreates.
fn state_output(count: Int, lovelace: Int) -> Output {
  let model_data: Data = Model { count }
  Output {
    address: script_address(),
    value: from_lovelace(lovelace)
      |> add(script_hash, reference_token_name, 1),
    datum: InlineDatum(model_data),
    reference_script: None,
  }
}

/// A state-shaped output without the reference NFT.
fn bare_output(count: Int, lovelace: Int) -> Output {
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

test token_dropped_rejected() {
  // The continuing output must keep the reference NFT: a transition
  // that strips it could seed a look-alike state elsewhere.
  !counter_spend(
    Some(Model { count: 41 }),
    Increment,
    own_ref,
    spend_tx([bare_output(42, 2_000_000)]),
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

// --- Batched dispatch (step 8): queue entries folded into the state ---

const queue_hash = #"11111111111111111111111111111111111111111111111111111111"

const author = #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

fn queue_ref(index: Int) -> OutputReference {
  OutputReference {
    transaction_id: #"1111111111111111111111111111111111111111111111111111111111111111",
    output_index: index,
  }
}

fn entry_at(script: ByteArray, index: Int, msg: Msg) -> Input {
  let payload: Data = msg
  let queued: Data = Queued { author, msg: payload }
  Input {
    output_reference: queue_ref(index),
    output: Output {
      address: from_script(script),
      value: from_lovelace(2_000_000),
      datum: InlineDatum(queued),
      reference_script: None,
    },
  }
}

fn queue_entry(index: Int, msg: Msg) -> Input {
  entry_at(queue_hash, index, msg)
}

fn batch_tx(entries: List<Input>, outputs: List<Output>) -> Transaction {
  Transaction { ..placeholder, inputs: [state_input(41), ..entries], outputs }
}

test batch_fold_accepted() {
  counter_batch(
    Some(Model { count: 41 }),
    queue_hash,
    own_ref,
    batch_tx(
      [
        queue_entry(0, Increment),
        queue_entry(1, Increment),
        queue_entry(2, Decrement),
      ],
      [state_output(42, 2_000_000)],
    ),
  )
}

test batch_input_order_applied_accepted() {
  counter_batch(
    Some(Model { count: 41 }),
    queue_hash,
    own_ref,
    batch_tx(
      [queue_entry(0, Reset), queue_entry(1, Increment)],
      [state_output(1, 2_000_000)],
    ),
  )
}

test batch_reversed_order_fold_rejected() {
  // The same two entries folded in the opposite order would give 0;
  // the validator must apply input order, so 0 is a wrong claim.
  !counter_batch(
    Some(Model { count: 41 }),
    queue_hash,
    own_ref,
    batch_tx(
      [queue_entry(0, Reset), queue_entry(1, Increment)],
      [state_output(0, 2_000_000)],
    ),
  )
}

test batch_wrong_datum_rejected() {
  !counter_batch(
    Some(Model { count: 41 }),
    queue_hash,
    own_ref,
    batch_tx([queue_entry(0, Increment)], [state_output(45, 2_000_000)]),
  )
}

test batch_lovelace_drain_rejected() {
  !counter_batch(
    Some(Model { count: 41 }),
    queue_hash,
    own_ref,
    batch_tx([queue_entry(0, Increment)], [state_output(42, 1_500_000)]),
  )
}

test batch_token_dropped_rejected() {
  !counter_batch(
    Some(Model { count: 41 }),
    queue_hash,
    own_ref,
    batch_tx([queue_entry(0, Increment)], [bare_output(42, 2_000_000)]),
  )
}

test batch_empty_rejected() fail {
  counter_batch(
    Some(Model { count: 41 }),
    queue_hash,
    own_ref,
    batch_tx([], [state_output(41, 2_000_000)]),
  )
}

test batch_malformed_entry_rejected() fail {
  let not_queued: Data = 7
  let junk = Input {
    output_reference: queue_ref(0),
    output: Output {
      address: from_script(queue_hash),
      value: from_lovelace(2_000_000),
      datum: InlineDatum(not_queued),
      reference_script: None,
    },
  }
  counter_batch(
    Some(Model { count: 41 }),
    queue_hash,
    own_ref,
    batch_tx([junk], [state_output(41, 2_000_000)]),
  )
}

test batch_foreign_script_entries_ignored() {
  // A Queued-shaped datum on some other script's input is not part of
  // this queue: only the Increment counts, so 42 is the right claim
  // (a fold that included the foreign Decrement would demand 41).
  let foreign = entry_at(
    #"22222222222222222222222222222222222222222222222222222222",
    3,
    Decrement,
  )
  counter_batch(
    Some(Model { count: 41 }),
    queue_hash,
    own_ref,
    batch_tx(
      [queue_entry(0, Increment), foreign],
      [state_output(42, 2_000_000)],
    ),
  )
}

test batch_terminal_step_rejected() fail {
  // A step that can halt must never do so inside a batch: the fold has
  // no continuing model past a None.
  tea.batch(
    fn(msg, model) {
      when msg is {
        Reset -> None
        Increment -> Some(update(msg, model))
        Decrement -> Some(update(msg, model))
      }
    },
    fn(payload) {
      expect msg: Msg = payload
      msg
    },
    fn(model) {
      let data: Data = model
      data
    },
    Some(Model { count: 41 }),
    queue_hash,
    own_ref,
    batch_tx(
      [queue_entry(0, Increment), queue_entry(1, Reset)],
      [state_output(42, 2_000_000)],
    ),
  )
}

// --- Genesis (step 9): the one-shot mint that marks the state ---

const seed_ref =
  OutputReference {
    transaction_id: #"3333333333333333333333333333333333333333333333333333333333333333",
    output_index: 1,
  }

const founder = #"cccccccccccccccccccccccccccccccccccccccccccccccccccccccc"

fn seed_input() -> Input {
  Input {
    output_reference: seed_ref,
    output: Output {
      address: from_verification_key(founder),
      value: from_lovelace(5_000_000),
      datum: NoDatum,
      reference_script: None,
    },
  }
}

fn mint_tx(
  inputs: List<Input>,
  outputs: List<Output>,
  quantity: Int,
) -> Transaction {
  Transaction {
    ..placeholder,
    inputs,
    outputs,
    mint: from_asset(script_hash, reference_token_name, quantity),
  }
}

test genesis_mint_accepted() {
  counter_mint(
    seed_ref,
    script_hash,
    mint_tx([seed_input()], [state_output(0, 2_000_000)], 1),
  )
}

test genesis_without_seed_rejected() {
  !counter_mint(
    seed_ref,
    script_hash,
    mint_tx([state_input(0)], [state_output(0, 2_000_000)], 1),
  )
}

test genesis_double_mint_rejected() {
  !counter_mint(
    seed_ref,
    script_hash,
    mint_tx([seed_input()], [state_output(0, 2_000_000)], 2),
  )
}

test genesis_nonzero_count_rejected() {
  !counter_mint(
    seed_ref,
    script_hash,
    mint_tx([seed_input()], [state_output(7, 2_000_000)], 1),
  )
}

test genesis_wrong_name_rejected() {
  let tx = Transaction {
    ..placeholder,
    inputs: [seed_input()],
    outputs: [state_output(0, 2_000_000)],
    mint: from_asset(script_hash, "wrong", 1),
  }
  !counter_mint(seed_ref, script_hash, tx)
}

test genesis_split_state_rejected() fail {
  counter_mint(
    seed_ref,
    script_hash,
    mint_tx(
      [seed_input()],
      [state_output(0, 2_000_000), state_output(0, 2_000_000)],
      1,
    ),
  )
}

test genesis_token_elsewhere_rejected() {
  let strayed = Output {
    address: from_verification_key(founder),
    value: from_lovelace(2_000_000)
      |> add(script_hash, reference_token_name, 1),
    datum: NoDatum,
    reference_script: None,
  }
  !counter_mint(
    seed_ref,
    script_hash,
    mint_tx([seed_input()], [bare_output(0, 2_000_000), strayed], 1),
  )
}

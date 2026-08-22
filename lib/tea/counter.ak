//// Counter: the smallest TEA application. `Model` is a single count,
//// `Msg` is increment / decrement / reset. A genesis reference NFT
//// (minted once by `counter_mint`) marks the state UTxO, and every
//// transition keeps it there: that is what lets the message queue tell
//// the genuine state from a look-alike (see `tea/queue`).

use aiken/crypto.{ScriptHash}
use cardano/address.{Script}
use cardano/assets.{PolicyId}
use cardano/transaction.{Input, Output, OutputReference, Transaction}
use tea

/// CIP-67 label (100) prefix, then "counter": the reference-token name
/// that marks the counter state UTxO.
pub const reference_token_name: ByteArray = #"000643b0636f756e746572"

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

/// One-shot genesis, TEA's `init`: mint the reference NFT and lock it at
/// the script with the zero model (see `tea.genesis`).
pub fn counter_mint(
  seed: OutputReference,
  policy_id: PolicyId,
  self: Transaction,
) -> Bool {
  tea.genesis(seed, policy_id, reference_token_name, count_zero, self)
}

/// The genesis datum policy: the count starts at zero.
fn count_zero(state_data: Data) -> Bool {
  expect model: Model = state_data
  model.count == 0
}

/// Spend logic for the counter, delegated to the generic TEA check.
/// Kept in the library so tests can call it directly.
pub fn counter_spend(
  datum: Option<Model>,
  redeemer: Msg,
  own_ref: OutputReference,
  self: Transaction,
) -> Bool {
  tea.spend_with(step, datum, redeemer, own_ref, self, keeps_reference_token)
}

/// `update` with its result upcast to `Data`, the shape `tea.spend_with`
/// needs.
fn step(msg: Msg, model: Model) -> Data {
  let next: Data = update(msg, model)
  next
}

/// Batched spend logic: fold every message consumed from `queue` into
/// the model (see `tea.batch_with`). Kept in the library so tests can
/// call it directly.
pub fn counter_batch(
  datum: Option<Model>,
  queue: ScriptHash,
  own_ref: OutputReference,
  self: Transaction,
) -> Bool {
  tea.batch_with(
    fn(msg, model) { Some(update(msg, model)) },
    decode_msg,
    to_data,
    datum,
    queue,
    own_ref,
    self,
    keeps_reference_token,
  )
}

/// The continuing state output must carry the reference NFT minted
/// under the counter's own script hash: what tells the queue's
/// `Process` check (and every later transition) the genuine state from
/// a look-alike.
fn keeps_reference_token(own_input: Input, own_output: Output) -> Bool {
  expect Script(own_hash) = own_input.output.address.payment_credential
  assets.quantity_of(own_output.value, own_hash, reference_token_name) == 1
}

/// Downcast a queued `Data` payload at the counter's message type, the
/// concrete site `tea.batch_with` cannot have.
fn decode_msg(payload: Data) -> Msg {
  expect msg: Msg = payload
  msg
}

fn to_data(model: Model) -> Data {
  let data: Data = model
  data
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

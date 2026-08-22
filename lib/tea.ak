//// Generic Elm-Architecture (TEA) transition check for spend validators.
////
//// The validator does not run the application. It verifies one step of it:
//// the transaction that spends the state UTxO must recreate it at the same
//// address with datum `update(msg, model)` and with no lovelace removed.
//// `update` is the pure TEA step function, `Msg` is the redeemer and
//// `Model` is the inline datum.

use aiken/collection/dict
use aiken/collection/list
use aiken/crypto.{ScriptHash, VerificationKeyHash}
use aiken/option
use cardano/address.{Script}
use cardano/assets.{PolicyId}
use cardano/transaction.{
  InlineDatum, Input, Output, OutputReference, Transaction, find_input,
}

/// Redeemer wrapper for a batch-capable app. `Single` carries one
/// message, the ordinary TEA step. `Batch` names the queue script whose
/// consumed entries carry the messages (see `batch`): the state
/// validator reads them from its own transaction, so the batcher cannot
/// apply messages it does not consume.
pub type Action<msg> {
  Single(msg)
  Batch { queue: ScriptHash }
}

/// One queued message: the inline datum of a UTxO at the queue script
/// address. `author` alone may reclaim the entry (with a signature);
/// `msg` is the application message, upcast to `Data` by the enqueuer.
/// Locking the entry is the author's authorization of the message, so
/// processing needs no further signature from them.
pub type Queued {
  author: VerificationKeyHash,
  msg: Data,
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
  spend_with(
    step,
    datum,
    redeemer,
    own_ref,
    self,
    fn(_own_input, _own_output) { True },
  )
}

/// `spend` with an application guard over the state input and its
/// continuing output (see `transition_with`): the shape for an app whose
/// state UTxO carries a marker token that must stay with it.
pub fn spend_with(
  step: fn(msg, model) -> Data,
  datum: Option<model>,
  redeemer: msg,
  own_ref: OutputReference,
  self: Transaction,
  keeps: fn(Input, Output) -> Bool,
) -> Bool {
  expect Some(model) = datum
  transition_with(step(redeemer, model), own_ref, self, keeps)
}

/// The messages carried by the transaction's queue inputs, in ledger
/// input order. The ledger sorts inputs by output reference, so the
/// order is deterministic and no party picks it after the entries
/// exist. `decode` downcasts each `Data` payload at the application's
/// concrete message type; a payload of any other shape fails the
/// transaction.
pub fn queued_msgs(
  queue: ScriptHash,
  self: Transaction,
  decode: fn(Data) -> msg,
) -> List<msg> {
  self.inputs
    |> list.filter(
        fn(input) { input.output.address.payment_credential == Script(queue) },
      )
    |> list.map(
        fn(input) {
          expect InlineDatum(payload) = input.output.datum
          expect queued: Queued = payload
          decode(queued.msg)
        },
      )
}

/// Batched TEA transition: the write-contention relief. Users lock
/// messages at the queue script instead of racing for the state UTxO;
/// one transaction then consumes the state UTxO plus any number of
/// queue entries, and the new state datum must equal the fold of `step`
/// over the queued messages, applied to the current model in ledger
/// input order. The batch must be non-empty (an empty batch is a
/// pointless state rewrite) and no step inside it may be terminal: a
/// halt has no continuing model to fold into, so it travels alone
/// through the single-message path.
pub fn batch(
  step: fn(msg, model) -> Option<model>,
  decode: fn(Data) -> msg,
  to_data: fn(model) -> Data,
  datum: Option<model>,
  queue: ScriptHash,
  own_ref: OutputReference,
  self: Transaction,
) -> Bool {
  batch_with(
    step,
    decode,
    to_data,
    datum,
    queue,
    own_ref,
    self,
    fn(_own_input, _own_output) { True },
  )
}

/// `batch` with an application guard over the state input and its
/// continuing output (see `transition_with`): the batched counterpart
/// of `spend_with`.
pub fn batch_with(
  step: fn(msg, model) -> Option<model>,
  decode: fn(Data) -> msg,
  to_data: fn(model) -> Data,
  datum: Option<model>,
  queue: ScriptHash,
  own_ref: OutputReference,
  self: Transaction,
  keeps: fn(Input, Output) -> Bool,
) -> Bool {
  expect Some(model) = datum
  let msgs = queued_msgs(queue, self, decode)
  expect [_, ..] = msgs
  expect Some(final) = list.foldl(
    msgs,
    Some(model),
    fn(msg, acc) { option.and_then(acc, fn(current) { step(msg, current) }) },
  )
  transition_with(to_data(final), own_ref, self, keeps)
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

/// One-shot genesis: TEA's `init` as a mint policy. The transaction
/// must consume `seed` (so the mint can never repeat), mint exactly one
/// `token_name` under `policy_id`, and lock the token at the script
/// whose hash is `policy_id`, on a single output whose inline datum
/// `init_ok` accepts. The token is what later tells the genuine state
/// UTxO from a look-alike someone pays to the same address (see
/// `tea/queue`): only this mint can create it, and a `keeps` guard on
/// the spend side (`spend_with`, `batch_with`) pins it to the state
/// forever after.
pub fn genesis(
  seed: OutputReference,
  policy_id: PolicyId,
  token_name: ByteArray,
  init_ok: fn(Data) -> Bool,
  self: Transaction,
) -> Bool {
  expect [Pair(minted_name, minted_quantity)] = assets.tokens(
    self.mint,
    policy_id,
  )
    |> dict.to_pairs
  expect [state] = list.filter(
    self.outputs,
    fn(out) { out.address.payment_credential == Script(policy_id) },
  )
  expect InlineDatum(state_data) = state.datum
  and {
    list.any(self.inputs, fn(input) { input.output_reference == seed }),
    minted_name == token_name,
    minted_quantity == 1,
    assets.quantity_of(state.value, policy_id, token_name) == 1,
    init_ok(state_data),
  }
}

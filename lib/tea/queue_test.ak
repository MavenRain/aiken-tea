//// Transaction-level tests for the message-queue spend policy
//// (`queue_spend`): an entry leaves the queue either into a batched
//// state transition (`Process`, a state input carrying the state
//// policy's marker token must be present) or back to its author
//// (`Reclaim`, the author must sign).

use cardano/address.{from_script, from_verification_key}
use cardano/assets.{add, from_lovelace}
use cardano/transaction.{
  Input, NoDatum, Output, OutputReference, Transaction, placeholder,
}
use tea.{Queued}
use tea/queue.{Process, Reclaim, queue_spend}

const state_hash = #"00000000000000000000000000000000000000000000000000000000"

const other_hash = #"22222222222222222222222222222222222222222222222222222222"

const author = #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

const stranger = #"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

fn entry_datum() -> Option<Queued> {
  let payload: Data = 0
  Some(Queued { author, msg: payload })
}

fn input_holding(address, value) -> Input {
  Input {
    output_reference: OutputReference {
      transaction_id: #"0000000000000000000000000000000000000000000000000000000000000000",
      output_index: 0,
    },
    output: Output { address, value, datum: NoDatum, reference_script: None },
  }
}

fn tx_with_input(address, value) -> Transaction {
  Transaction { ..placeholder, inputs: [input_holding(address, value)] }
}

/// The genuine state value: min-ada plus a marker token minted under
/// the state script's own policy.
fn marked(policy) {
  from_lovelace(2_000_000) |> add(policy, "marker", 1)
}

fn tx_signed_by(signer: ByteArray) -> Transaction {
  Transaction { ..placeholder, extra_signatories: [signer] }
}

test process_with_marked_state_input_accepted() {
  queue_spend(
    state_hash,
    entry_datum(),
    Process,
    tx_with_input(from_script(state_hash), marked(state_hash)),
  )
}

test process_lookalike_without_token_rejected() {
  // Anyone can pay a UTxO to the state address; without a token minted
  // under the state's own policy it must not count as the state.
  !queue_spend(
    state_hash,
    entry_datum(),
    Process,
    tx_with_input(from_script(state_hash), from_lovelace(2_000_000)),
  )
}

test process_token_at_foreign_credential_rejected() {
  !queue_spend(
    state_hash,
    entry_datum(),
    Process,
    tx_with_input(from_verification_key(stranger), marked(state_hash)),
  )
}

test process_without_state_input_rejected() {
  !queue_spend(
    state_hash,
    entry_datum(),
    Process,
    tx_with_input(from_verification_key(stranger), from_lovelace(2_000_000)),
  )
}

test process_wrong_script_input_rejected() {
  !queue_spend(
    state_hash,
    entry_datum(),
    Process,
    tx_with_input(from_script(other_hash), marked(other_hash)),
  )
}

test reclaim_by_author_accepted() {
  queue_spend(state_hash, entry_datum(), Reclaim, tx_signed_by(author))
}

test reclaim_by_stranger_rejected() {
  !queue_spend(state_hash, entry_datum(), Reclaim, tx_signed_by(stranger))
}

test reclaim_unsigned_rejected() {
  !queue_spend(
    state_hash,
    entry_datum(),
    Reclaim,
    Transaction { ..placeholder, extra_signatories: [] },
  )
}

test reclaim_missing_datum_rejected() fail {
  queue_spend(state_hash, None, Reclaim, tx_signed_by(author))
}

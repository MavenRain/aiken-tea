//// Transaction-level tests for the registry app: the upgrade policy
//// (owner signature, payload shape, NFT preservation on top of the
//// generic transition) and the one-shot genesis mint.

use cardano/address.{from_script, from_verification_key}
use cardano/assets.{add, from_asset, from_lovelace}
use cardano/transaction.{
  InlineDatum, Input, NoDatum, Output, OutputReference, Transaction, placeholder,
}
use tea/registry.{
  Model, Publish, Retire, reference_token_name, registry_mint, registry_spend,
}

const script_hash = #"00000000000000000000000000000000000000000000000000000000"

const owner = #"11111111111111111111111111111111111111111111111111111111"

const stranger = #"22222222222222222222222222222222222222222222222222222222"

const hash_v1 =
  #"0101010101010101010101010101010101010101010101010101010101010101"

const hash_v2 =
  #"0202020202020202020202020202020202020202020202020202020202020202"

const own_ref =
  OutputReference {
    transaction_id: #"0000000000000000000000000000000000000000000000000000000000000000",
    output_index: 0,
  }

const seed_ref =
  OutputReference {
    transaction_id: #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
    output_index: 1,
  }

fn model_v1() -> Model {
  Model { cid: "bafy-v1", version: 0, frontend_hash: hash_v1 }
}

fn publish_v2() {
  Publish { cid: "bafy-v2", frontend_hash: hash_v2 }
}

fn state_output(model: Model, with_token: Bool) -> Output {
  let model_data: Data = model
  let value = when with_token is {
    True ->
      from_lovelace(2_000_000) |> add(script_hash, reference_token_name, 1)
    False -> from_lovelace(2_000_000)
  }
  Output {
    address: from_script(script_hash),
    value,
    datum: InlineDatum(model_data),
    reference_script: None,
  }
}

fn state_input() -> Input {
  Input { output_reference: own_ref, output: state_output(model_v1(), True) }
}

fn spend_tx(outputs: List<Output>, signers: List<ByteArray>) -> Transaction {
  Transaction {
    ..placeholder,
    inputs: [state_input()],
    outputs,
    extra_signatories: signers,
  }
}

fn next_model() -> Model {
  Model { cid: "bafy-v2", version: 1, frontend_hash: hash_v2 }
}

test publish_accepted() {
  registry_spend(
    owner,
    Some(model_v1()),
    publish_v2(),
    own_ref,
    spend_tx([state_output(next_model(), True)], [owner]),
  )
}

test unsigned_rejected() {
  !registry_spend(
    owner,
    Some(model_v1()),
    publish_v2(),
    own_ref,
    spend_tx([state_output(next_model(), True)], []),
  )
}

test stranger_signature_rejected() {
  !registry_spend(
    owner,
    Some(model_v1()),
    publish_v2(),
    own_ref,
    spend_tx([state_output(next_model(), True)], [stranger]),
  )
}

test nft_dropped_rejected() {
  !registry_spend(
    owner,
    Some(model_v1()),
    publish_v2(),
    own_ref,
    spend_tx([state_output(next_model(), False)], [owner]),
  )
}

test version_skip_rejected() {
  let skipped = Model { cid: "bafy-v2", version: 2, frontend_hash: hash_v2 }
  !registry_spend(
    owner,
    Some(model_v1()),
    publish_v2(),
    own_ref,
    spend_tx([state_output(skipped, True)], [owner]),
  )
}

test empty_cid_rejected() {
  let msg = Publish { cid: "", frontend_hash: hash_v2 }
  let bad = Model { cid: "", version: 1, frontend_hash: hash_v2 }
  !registry_spend(
    owner,
    Some(model_v1()),
    msg,
    own_ref,
    spend_tx([state_output(bad, True)], [owner]),
  )
}

test short_hash_rejected() {
  let msg = Publish { cid: "bafy-v2", frontend_hash: #"0202" }
  let bad = Model { cid: "bafy-v2", version: 1, frontend_hash: #"0202" }
  !registry_spend(
    owner,
    Some(model_v1()),
    msg,
    own_ref,
    spend_tx([state_output(bad, True)], [owner]),
  )
}

fn seed_input() -> Input {
  Input {
    output_reference: seed_ref,
    output: Output {
      address: from_verification_key(owner),
      value: from_lovelace(5_000_000),
      datum: NoDatum,
      reference_script: None,
    },
  }
}

fn genesis_output(model: Model) -> Output {
  state_output(model, True)
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
  registry_mint(
    seed_ref,
    script_hash,
    mint_tx([seed_input()], [genesis_output(model_v1())], 1),
  )
}

test genesis_without_seed_rejected() {
  !registry_mint(
    seed_ref,
    script_hash,
    mint_tx([state_input()], [genesis_output(model_v1())], 1),
  )
}

test genesis_double_mint_rejected() {
  !registry_mint(
    seed_ref,
    script_hash,
    mint_tx([seed_input()], [genesis_output(model_v1())], 2),
  )
}

test genesis_nonzero_version_rejected() {
  let late = Model { cid: "bafy-v1", version: 1, frontend_hash: hash_v1 }
  !registry_mint(
    seed_ref,
    script_hash,
    mint_tx([seed_input()], [genesis_output(late)], 1),
  )
}

test genesis_malformed_datum_rejected() {
  let bare = Model { cid: "", version: 0, frontend_hash: hash_v1 }
  !registry_mint(
    seed_ref,
    script_hash,
    mint_tx([seed_input()], [genesis_output(bare)], 1),
  )
}

test genesis_split_state_rejected() fail {
  registry_mint(
    seed_ref,
    script_hash,
    mint_tx(
      [seed_input()],
      [genesis_output(model_v1()), genesis_output(model_v1())],
      1,
    ),
  )
}

test genesis_token_elsewhere_rejected() {
  let strayed = Output {
    address: from_verification_key(owner),
    value: from_lovelace(2_000_000)
      |> add(script_hash, reference_token_name, 1),
    datum: NoDatum,
    reference_script: None,
  }
  let bare_state = state_output(model_v1(), False)
  !registry_mint(
    seed_ref,
    script_hash,
    mint_tx([seed_input()], [bare_state, strayed], 1),
  )
}

fn retire_tx(
  outputs: List<Output>,
  signers: List<ByteArray>,
  quantity: Int,
) -> Transaction {
  Transaction {
    ..placeholder,
    inputs: [state_input()],
    outputs,
    extra_signatories: signers,
    mint: from_asset(script_hash, reference_token_name, quantity),
  }
}

test retire_accepted() {
  registry_spend(
    owner,
    Some(model_v1()),
    Retire,
    own_ref,
    retire_tx([], [owner], -1),
  )
}

test retire_unsigned_rejected() {
  !registry_spend(
    owner,
    Some(model_v1()),
    Retire,
    own_ref,
    retire_tx([], [], -1),
  )
}

test retire_stranger_rejected() {
  !registry_spend(
    owner,
    Some(model_v1()),
    Retire,
    own_ref,
    retire_tx([], [stranger], -1),
  )
}

test retire_keeping_state_rejected() {
  !registry_spend(
    owner,
    Some(model_v1()),
    Retire,
    own_ref,
    retire_tx([state_output(model_v1(), False)], [owner], -1),
  )
}

test retire_without_burn_rejected() {
  !registry_spend(
    owner,
    Some(model_v1()),
    Retire,
    own_ref,
    spend_tx([], [owner]),
  )
}

test retire_double_burn_rejected() {
  !registry_spend(
    owner,
    Some(model_v1()),
    Retire,
    own_ref,
    retire_tx([], [owner], -2),
  )
}

test burn_mint_accepted() {
  registry_mint(seed_ref, script_hash, mint_tx([state_input()], [], -1))
}

test burn_wrong_name_rejected() {
  !registry_mint(
    seed_ref,
    script_hash,
    Transaction {
      ..placeholder,
      inputs: [state_input()],
      outputs: [],
      mint: from_asset(script_hash, "stray-token", -1),
    },
  )
}

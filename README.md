# aiken-tea

The Elm Architecture (TEA) on Cardano, in Aiken. First probe of a
TEA-shaped web framework: the on-chain state UTxO is the `Model`, the
redeemer is the `Msg`, and the validator verifies (not runs) one step
of the pure `update` function per transaction.

## Layout

- `lib/tea.ak` - the framework: a generic `spend` check. Give it the
  application's `update` (as a `Data`-returning `step`) and it enforces
  that the spending transaction recreates exactly one state UTxO at the
  script address, with inline datum `update(msg, model)` and no
  lovelace removed. `tea.halt` is the terminal check: when `update`
  returns `None`, the transaction must consume the state UTxO and leave
  no output at the script address.
- `lib/tea/counter.ak` - the smallest application: `Model` = a count,
  `Msg` = `Increment | Decrement | Reset`.
- `lib/tea/counter_test.ak` - transaction-level tests: 3 accepted
  transitions, 6 rejected shapes (wrong datum, lovelace drain, missing
  datum, split state, vanished state, non-inline datum).
- `validators/counter.ak` - the deployable validator, a one-line
  delegation to the library.
- `lib/tea/registry.ak` - the IPFS deployment registry app (step 3):
  `Model` = `{cid, version, frontend_hash}`, `Msg` = `Publish |
  Retire`. The spend policy stacks an owner signature, payload shape
  checks and reference-NFT preservation on the generic transition
  (`tea.transition_with`); a one-shot `mint` creates the CIP-68-style
  reference NFT at genesis. `Retire` is the terminal message (step 5):
  `update` returns `None`, so the spend requires the generic halt plus
  a burn of the reference NFT, and the mint handler accepts that burn.
- `lib/tea/registry_test.ak` - transaction-level tests for the upgrade
  policy, the genesis mint and the retirement path.
- `validators/registry.ak` - one script, two purposes: the mint policy
  id is the script's own hash, so the spend handler recovers it from
  its own address with no extra parameter.

## Checks

```
aiken check   # 39/39
aiken build
```

## Design notes

- The validator recomputes `update(msg, model)` and requires the
  proposed output datum to equal it, so the transition is exact, not
  attested. The same compiled UPLC can be evaluated in a browser (WASM)
  for optimistic UI.
- The generic wrapper takes `step: fn(msg, model) -> Data` because
  Aiken cannot upcast an unbound generic to `Data`; the application
  upcasts at its concrete call site.
- Exactly-one continuing output blocks double-satisfaction and state
  splitting. Lovelace is monotone non-decreasing so min-ada can grow
  with the datum.
- The registry needs policy the generic check cannot know (who may
  publish, the NFT staying put), so `tea.transition_with` takes a guard
  over the state input and its continuing output. The guard runs on top
  of the datum and lovelace checks, never instead of them.
- Registry versions are exact by construction: the validator recomputes
  `update`, and `update` bumps the version by one, so a version skip is
  just a wrong datum. The reference NFT plus the one-shot seed UTxO
  make the state UTxO unforgeable: only one such token can ever exist,
  and every spend must carry it forward.
- Termination is part of the step function, not a side channel: the
  registry's `update` returns `Option<Model>`, and the validator
  branches on the recomputed verdict. `Some` demands the transition,
  `None` demands the halt plus the NFT burn. The seed UTxO is gone
  after genesis, so a retired registry can never be reminted.

## Roadmap (from the feasibility sketch)

1. This probe: generic transition check + counter, end to end. [DONE:
   on-chain half]
2. Off-chain client: build/submit transitions (Lucid Evolution or
   Mesh), browser-side optimistic `update` via WASM UPLC. [DONE:
   `client-ocaml/`, mirrored OCaml `update`; WASM UPLC deferred]
3. IPFS deployment registry: CIP-68-style reference NFT holding
   `{cid, version, frontend_hash}` with a validator-enforced upgrade
   policy. [DONE: on-chain half + client mirror]
4. Pinning tooling: derive a bundle's CIDv1 and blake2b-256 hash in
   pure OCaml, publish a real bundle end to end, and pin it on a kubo
   daemon under a differential CID check. [DONE for one raw block
   (up to 256 KiB); chunked dag-pb trees deferred]
5. Registry retirement: a terminal `Retire` message ends the state
   UTxO and burns the reference NFT under the same owner policy.
   [DONE: on-chain half + client mirror]

## Client (step 2)

`client-ocaml/` is the off-chain half: a generic TEA runtime in OCaml,
compiled with js_of_ocaml against
[Lucid Evolution](https://github.com/Anastasia-Labs/lucid-evolution).
`dispatch(msg)` computes the next model locally with the mirrored pure
`update` (the optimistic update) and submits the transition transaction;
the validator re-computes the same step on-chain, so a divergent mirror
cannot confirm. `subscribe` polls the script address for the confirmed
model (the Sub side).

The registry app is mirrored too (`src/pure/registry.ml`,
`src/js/registry_app.ml`): the client applies the `(owner, seed)`
parameters to the blueprint, derives the policy id, performs the
one-shot genesis mint, and decorates every build with the owner's
required signature. `Registry_app.retire`, over the generic `Tea.halt`,
ends the app: the mirrored `update` returns `None` for `Retire`, which
licenses a transaction that recreates nothing and burns the reference
NFT. Its emulator suite covers the genesis path, the upgrade policy
(signature, exact version bump, NFT preservation, well-formedness) and
the retirement (accepted burn; rejected unsigned, state-keeping and
burn-less variants).

The pinning pipeline (step 4) closes the loop from bundle bytes to an
on-chain publish. The pure half (`src/pure/hashes.ml`, `cid.ml`,
`bundle.ml`) computes the bundle's blake2b-256 `frontend_hash` and its
CIDv1 (raw leaf, sha2-256 multihash, lowercase base32) with no
dependencies, and `Bundle.publish_msg` turns raw bytes into the
`Publish` message. The JS half reads a bundle file and dispatches it
(`Registry_app.publish_bundle`); `Registry_app.pin_bundle`
adds-and-pins on a kubo daemon
(`/api/v0/add?cid-version=1&raw-leaves=true&pin=true`) and requires
the daemon's CID to equal the locally derived one: a differential
check of the CID construction. One raw block (up to 256 KiB) is
supported; chunked dag-pb trees are out of scope for the probe.
Digest, base32 and CID vectors in the native suite are pinned to
python3 hashlib / shasum / b2sum, and the emulator suite publishes the
real `test/js/fixture-bundle.html` end to end. Set
`IPFS_API=http://127.0.0.1:5001` to also exercise the daemon pin.

```sh
cd client-ocaml
dune test        # native Data codec suite
pnpm install
pnpm test        # js_of_ocaml bundles + Lucid emulator, no network needed
```

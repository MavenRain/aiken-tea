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
  lovelace removed.
- `lib/tea/counter.ak` - the smallest application: `Model` = a count,
  `Msg` = `Increment | Decrement | Reset`.
- `lib/tea/counter_test.ak` - transaction-level tests: 3 accepted
  transitions, 6 rejected shapes (wrong datum, lovelace drain, missing
  datum, split state, vanished state, non-inline datum).
- `validators/counter.ak` - the deployable validator, a one-line
  delegation to the library.
- `lib/tea/registry.ak` - the IPFS deployment registry app (step 3):
  `Model` = `{cid, version, frontend_hash}`, `Msg` = `Publish`. The
  spend policy stacks an owner signature, payload shape checks and
  reference-NFT preservation on the generic transition
  (`tea.transition_with`); a one-shot `mint` creates the CIP-68-style
  reference NFT at genesis.
- `lib/tea/registry_test.ak` - transaction-level tests for the upgrade
  policy and the genesis mint.
- `validators/registry.ak` - one script, two purposes: the mint policy
  id is the script's own hash, so the spend handler recovers it from
  its own address with no extra parameter.

## Checks

```
aiken check   # 30/30
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

## Roadmap (from the feasibility sketch)

1. This probe: generic transition check + counter, end to end. [DONE:
   on-chain half]
2. Off-chain client: build/submit transitions (Lucid Evolution or
   Mesh), browser-side optimistic `update` via WASM UPLC. [DONE:
   `client-ocaml/`, mirrored OCaml `update`; WASM UPLC deferred]
3. IPFS deployment registry: CIP-68-style reference NFT holding
   `{cid, version, frontend_hash}` with a validator-enforced upgrade
   policy. [DONE: on-chain half; client mirror + pinning tooling
   deferred]

## Client (step 2)

`client-ocaml/` is the off-chain half: a generic TEA runtime in OCaml,
compiled with js_of_ocaml against
[Lucid Evolution](https://github.com/Anastasia-Labs/lucid-evolution).
`dispatch(msg)` computes the next model locally with the mirrored pure
`update` (the optimistic update) and submits the transition transaction;
the validator re-computes the same step on-chain, so a divergent mirror
cannot confirm. `subscribe` polls the script address for the confirmed
model (the Sub side).

```sh
cd client-ocaml
dune test        # native Data codec suite
pnpm install
pnpm test        # js_of_ocaml bundle + Lucid emulator, no network needed
```

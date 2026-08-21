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

## Checks

```
aiken check   # 12/12
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

## Roadmap (from the feasibility sketch)

1. This probe: generic transition check + counter, end to end. [DONE:
   on-chain half]
2. Off-chain client: build/submit transitions (Lucid Evolution or
   Mesh), browser-side optimistic `update` via WASM UPLC.
3. IPFS deployment registry: CIP-68-style reference NFT holding
   `{cid, version, frontend_hash}` with a validator-enforced upgrade
   policy.

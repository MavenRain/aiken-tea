// Emulator suite for the TEA client runtime. The acceptance tests are
// also the mirror-parity oracle: every dispatched transition is
// re-verified on-chain by the Aiken validator, so a TS/Aiken `update`
// divergence fails the test. The reject tests hand-build transactions
// that bypass `dispatch` to prove the validator (not the client) is
// what enforces the protocol.

import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { beforeEach, describe, expect, it } from "vitest";
import type { LucidEvolution } from "@lucid-evolution/lucid";
import { Emulator, Lucid, generateEmulatorAccount } from "@lucid-evolution/lucid";
import type { Handle } from "../src/tea.js";
import {
  connect,
  currentModel,
  deploy,
  dispatch,
  stateUtxo,
} from "../src/tea.js";
import type { Model, Msg } from "../src/counter.js";
import { counterApp } from "../src/counter.js";

type BlueprintValidator = {
  readonly title: string;
  readonly compiledCode: string;
};

const blueprintPath = fileURLToPath(new URL("../../plutus.json", import.meta.url));
const blueprint: { readonly validators: readonly BlueprintValidator[] } =
  JSON.parse(readFileSync(blueprintPath, "utf8"));
const spendEntry = blueprint.validators.find(
  (validator) => validator.title === "counter.counter.spend",
);
if (spendEntry === undefined) {
  throw new Error("counter.counter.spend missing from plutus.json");
}
const app = counterApp(spendEntry.compiledCode);

const STATE_LOVELACE = 5_000_000n;

describe("counter over the TEA runtime", () => {
  let emulator: Emulator;
  let lucid: LucidEvolution;
  let handle: Handle<Model, Msg>;

  beforeEach(async () => {
    const account = generateEmulatorAccount({ lovelace: 100_000_000_000n });
    emulator = new Emulator([account]);
    lucid = await Lucid(emulator, "Custom");
    lucid.selectWallet.fromSeed(account.seedPhrase);
    handle = connect(lucid, app);
    await deploy(handle, { count: 0n }, STATE_LOVELACE);
    emulator.awaitBlock(1);
  });

  const step = async (msg: Msg): Promise<Model> => {
    const { predicted } = await dispatch(handle, msg);
    emulator.awaitBlock(1);
    return predicted;
  };

  it("deploys the initial model", async () => {
    expect(await currentModel(handle)).toEqual({ count: 0n });
  });

  it("confirms each optimistic update on-chain", async () => {
    const afterFirst = await step("Increment");
    expect(afterFirst).toEqual({ count: 1n });
    expect(await currentModel(handle)).toEqual({ count: 1n });

    const afterSecond = await step("Increment");
    expect(afterSecond).toEqual({ count: 2n });
    const afterThird = await step("Decrement");
    expect(afterThird).toEqual({ count: 1n });
    expect(await currentModel(handle)).toEqual({ count: 1n });
  });

  it("resets to zero", async () => {
    await step("Increment");
    await step("Reset");
    expect(await currentModel(handle)).toEqual({ count: 0n });
  });

  it("rejects a transition whose datum is not update(msg, model)", async () => {
    const utxo = await stateUtxo(handle);
    const forged = app.model.toData({ count: 7n });
    await expect(
      lucid
        .newTx()
        .collectFrom([utxo], app.msg.toData("Increment"))
        .attach.SpendingValidator(app.validator)
        .pay.ToContract(
          handle.address,
          { kind: "inline", value: forged },
          { lovelace: STATE_LOVELACE },
        )
        .complete(),
    ).rejects.toThrow();
  });

  it("rejects a state split into two script outputs", async () => {
    const utxo = await stateUtxo(handle);
    const next = app.model.toData(app.update("Increment", { count: 0n }));
    await expect(
      lucid
        .newTx()
        .collectFrom([utxo], app.msg.toData("Increment"))
        .attach.SpendingValidator(app.validator)
        .pay.ToContract(
          handle.address,
          { kind: "inline", value: next },
          { lovelace: STATE_LOVELACE },
        )
        .pay.ToContract(
          handle.address,
          { kind: "inline", value: next },
          { lovelace: 2_000_000n },
        )
        .complete(),
    ).rejects.toThrow();
  });

  it("rejects a transition that drains lovelace from the state", async () => {
    const utxo = await stateUtxo(handle);
    const next = app.model.toData(app.update("Increment", { count: 0n }));
    await expect(
      lucid
        .newTx()
        .collectFrom([utxo], app.msg.toData("Increment"))
        .attach.SpendingValidator(app.validator)
        .pay.ToContract(
          handle.address,
          { kind: "inline", value: next },
          { lovelace: 2_000_000n },
        )
        .complete(),
    ).rejects.toThrow();
  });
});

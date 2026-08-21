// Counter: the client half of lib/tea/counter.ak. `update` mirrors the
// Aiken step function; the emulator suite proves the mirror faithful,
// because the validator recomputes the step and rejects any divergence.

import type { SpendingValidator } from "@lucid-evolution/lucid";
import { Data, applyDoubleCborEncoding } from "@lucid-evolution/lucid";
import type { App } from "./tea.js";

const ModelSchema = Data.Object({ count: Data.Integer() });
export type Model = Data.Static<typeof ModelSchema>;
const ModelShape = ModelSchema as unknown as Model;

const MsgSchema = Data.Enum([
  Data.Literal("Increment"),
  Data.Literal("Decrement"),
  Data.Literal("Reset"),
]);
export type Msg = Data.Static<typeof MsgSchema>;
const MsgShape = MsgSchema as unknown as Msg;

const transitions: Record<Msg, (model: Model) => Model> = {
  Increment: (model) => ({ count: model.count + 1n }),
  Decrement: (model) => ({ count: model.count - 1n }),
  Reset: () => ({ count: 0n }),
};

/// The pure TEA step function, mirroring `counter.update` in Aiken.
export const update = (msg: Msg, model: Model): Model =>
  transitions[msg](model);

/// Bind the counter app to its compiled validator (from plutus.json).
export const counterApp = (compiledCode: string): App<Model, Msg> => ({
  update,
  model: {
    toData: (value) => Data.to(value, ModelShape),
    fromData: (data) => Data.from(data, ModelShape),
  },
  msg: { toData: (value) => Data.to(value, MsgShape) },
  validator: {
    type: "PlutusV3",
    script: applyDoubleCborEncoding(compiledCode),
  },
});

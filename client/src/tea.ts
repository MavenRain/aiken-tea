// Generic Elm-Architecture (TEA) client runtime for Cardano.
//
// The on-chain half (lib/tea.ak) verifies one TEA step per transaction;
// this half produces those transactions. `dispatch` computes the next
// model locally with the mirrored pure `update` (the optimistic update),
// then submits a transaction the validator re-checks by recomputing the
// same step on-chain. `subscribe` is the Sub side: it polls the script
// address and reports the confirmed model.

import type {
  LucidEvolution,
  Network,
  SpendingValidator,
  UTxO,
} from "@lucid-evolution/lucid";
import { validatorToAddress } from "@lucid-evolution/lucid";

/// Datum codec between the app model and Plutus Data (CBOR hex).
export type Codec<A> = {
  readonly toData: (value: A) => string;
  readonly fromData: (data: string) => A;
};

/// A TEA application: the pure step function plus the codecs and the
/// compiled validator that enforces that step on-chain.
export type App<Model, Msg> = {
  readonly update: (msg: Msg, model: Model) => Model;
  readonly model: Codec<Model>;
  readonly msg: { readonly toData: (value: Msg) => string };
  readonly validator: SpendingValidator;
};

/// A connected app: Lucid instance plus the derived script address.
export type Handle<Model, Msg> = {
  readonly lucid: LucidEvolution;
  readonly app: App<Model, Msg>;
  readonly address: string;
};

const network = (lucid: LucidEvolution): Network =>
  lucid.config().network ?? "Custom";

export const connect = <Model, Msg>(
  lucid: LucidEvolution,
  app: App<Model, Msg>,
): Handle<Model, Msg> => ({
  lucid,
  app,
  address: validatorToAddress(network(lucid), app.validator),
});

const submit = async (
  tx: Awaited<ReturnType<ReturnType<LucidEvolution["newTx"]>["complete"]>>,
): Promise<string> => {
  const signed = await tx.sign.withWallet().complete();
  return signed.submit();
};

/// Create the state UTxO carrying the initial model as an inline datum.
export const deploy = async <Model, Msg>(
  handle: Handle<Model, Msg>,
  initial: Model,
  lovelace: bigint,
): Promise<string> => {
  const tx = await handle.lucid
    .newTx()
    .pay.ToContract(
      handle.address,
      { kind: "inline", value: handle.app.model.toData(initial) },
      { lovelace },
    )
    .complete();
  return submit(tx);
};

/// The single state UTxO at the script address. More or fewer than one
/// datum-carrying UTxO means the app state is broken (or not deployed).
export const stateUtxo = async <Model, Msg>(
  handle: Handle<Model, Msg>,
): Promise<UTxO> => {
  const withDatum = (await handle.lucid.utxosAt(handle.address)).filter(
    (utxo) => utxo.datum !== null && utxo.datum !== undefined,
  );
  const [only, ...rest] = withDatum;
  if (only === undefined || rest.length > 0) {
    throw new Error(
      `expected exactly one state UTxO at ${handle.address}, found ${withDatum.length}`,
    );
  }
  return only;
};

/// The confirmed on-chain model.
export const currentModel = async <Model, Msg>(
  handle: Handle<Model, Msg>,
): Promise<Model> => {
  const utxo = await stateUtxo(handle);
  return handle.app.model.fromData(utxo.datum as string);
};

/// One TEA step: read the confirmed model, compute the next model
/// locally (the optimistic update, returned immediately as `predicted`),
/// and submit the transition transaction with the message as redeemer.
export const dispatch = async <Model, Msg>(
  handle: Handle<Model, Msg>,
  msg: Msg,
): Promise<{ readonly predicted: Model; readonly txHash: string }> => {
  const utxo = await stateUtxo(handle);
  const model = handle.app.model.fromData(utxo.datum as string);
  const predicted = handle.app.update(msg, model);
  const tx = await handle.lucid
    .newTx()
    .collectFrom([utxo], handle.app.msg.toData(msg))
    .attach.SpendingValidator(handle.app.validator)
    .pay.ToContract(
      handle.address,
      { kind: "inline", value: handle.app.model.toData(predicted) },
      { lovelace: utxo.assets["lovelace"] ?? 0n },
    )
    .complete();
  const txHash = await submit(tx);
  return { predicted, txHash };
};

/// The Sub side: poll the script address and report each confirmed
/// model. Returns the unsubscribe function.
export const subscribe = <Model, Msg>(
  handle: Handle<Model, Msg>,
  onModel: (model: Model) => void,
  intervalMs: number,
): (() => void) => {
  const timer = setInterval(() => {
    void currentModel(handle)
      .then(onModel)
      .catch(() => undefined);
  }, intervalMs);
  return () => clearInterval(timer);
};

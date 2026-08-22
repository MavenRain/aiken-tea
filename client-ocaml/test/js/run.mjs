// Node driver for the js_of_ocaml emulator suite. The OCaml bundle
// cannot import the ESM-only @lucid-evolution/lucid package, so this
// shim injects it (and the parsed blueprint) as globals first, then
// imports the compiled bundle, which runs the suite at toplevel.
import { readFileSync } from "node:fs";
import { createRequire } from "node:module";
import { fileURLToPath, pathToFileURL } from "node:url";
import * as LucidLib from "@lucid-evolution/lucid";

// The js_of_ocaml runtime reaches for CommonJS require (fs, tty, ...);
// give the ESM-loaded bundle one.
globalThis.require = createRequire(import.meta.url);

globalThis.LucidLib = LucidLib;
globalThis.Blueprint = JSON.parse(
  readFileSync(fileURLToPath(new URL("../../../plutus.json", import.meta.url)), "utf8"),
);

globalThis.FixtureDir = fileURLToPath(new URL(".", import.meta.url));

const bundle =
  process.argv[2] ??
  fileURLToPath(
    new URL("../../_build/default/test/js/test_counter.bc.js", import.meta.url),
  );
await import(pathToFileURL(bundle));

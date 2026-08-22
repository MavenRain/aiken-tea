// Dev-time oracle for the chunked dag-pb CID vectors in
// test/native/test_data.ml: ipfs-unixfs-importer is the reference
// UnixFS implementation (Helia), and with cid-version 1, raw leaves,
// a fixed-size chunker, and the balanced layout it derives the same
// CIDs kubo does for `ipfs add --cid-version 1 --raw-leaves`. Inputs
// are byte ramps (i & 0xff) so no two chunks are equal and an
// ordering mutation cannot hide. Run: node test/js/oracle_cids.mjs
import { importer } from "ipfs-unixfs-importer";
import { fixedSize } from "ipfs-unixfs-importer/chunker";
import { balanced } from "ipfs-unixfs-importer/layout";
import { MemoryBlockstore } from "blockstore-core/memory";

const ramp = (n) => Uint8Array.from({ length: n }, (_, i) => i & 0xff);

async function cidOf(bytes, chunkSize, maxChildrenPerNode) {
  const entries = [];
  for await (const entry of importer([{ content: bytes }], new MemoryBlockstore(), {
    cidVersion: 1,
    rawLeaves: true,
    chunker: fixedSize({ chunkSize }),
    layout: balanced({ maxChildrenPerNode }),
  })) entries.push(entry);
  return entries.at(-1).cid.toString();
}

const cases = [
  ["ramp 4, chunk 4, fanout 2 (one raw leaf)", 4, 4, 2],
  ["ramp 8, chunk 4, fanout 2 (2 chunks, 1 level)", 8, 4, 2],
  ["ramp 9, chunk 4, fanout 2 (3 chunks, 2 levels)", 9, 4, 2],
  ["ramp 16, chunk 4, fanout 2 (4 chunks, 2 levels)", 16, 4, 2],
  ["ramp 20, chunk 4, fanout 2 (5 chunks, 3 levels)", 20, 4, 2],
  ["ramp 262145, production params (2 chunks)", 262145, 262144, 174],
  ["ramp 786432, production params (3 chunks exact)", 786432, 262144, 174],
];

for (const [label, n, chunk, fanout] of cases) {
  console.log(`${label}: ${await cidOf(ramp(n), chunk, fanout)}`);
}

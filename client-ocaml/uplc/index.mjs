// Re-export the CEK-machine trio from this package's own dependency
// tree. The main client package imports this file by path, so Lucid's
// harmoniclabs peer resolution is never disturbed.
export * as UplcLib from "@harmoniclabs/uplc";
export * as PlutusMachine from "@harmoniclabs/plutus-machine";
export * as PlutusData from "@harmoniclabs/plutus-data";

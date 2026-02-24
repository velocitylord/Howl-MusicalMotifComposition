#!/usr/bin/env wolframscript

(* Usage:
   wolframscript -file Scripts/makeDataset.wls <datasetDir> [outputFile]
*)

repoRoot = DirectoryName[DirectoryName[ExpandFileName[First[$ScriptCommandLine]]]];
SetDirectory[repoRoot];

If[Length[$ScriptCommandLine] < 2,
  Print["Usage: wolframscript -file Scripts/makeDataset.wls <datasetDir> [outputFile]"];
  Exit[1];
];

datasetPath = $ScriptCommandLine[[2]];
outputPath = If[Length[$ScriptCommandLine] >= 3, $ScriptCommandLine[[3]], "Scripts/dataset.wxf"];

(* Avoid context shadow warnings if these symbols exist in Global` from prior evaluations. *)
Remove["Global`HowlFindMidis", "Global`HowlMidiImport"];

Get["Howl/HowlMidiTools.wl"];

midiFiles = HowlFindMidis[datasetPath];
Print["MIDI files found: ", Length[midiFiles]];

imported = Map[HowlMidiImport, midiFiles];
valid = Select[imported, !FailureQ[#["Notes"]] &];

Print["Total imported: ", Length[imported]];
Print["Valid imports: ", Length[valid]];
Print["Failed imports: ", Length[imported] - Length[valid]];

Export[outputPath, imported];
Print["Saved: ", outputPath];

(* Mathematica Script *)
(* :Author: Howl contributors *)
(* :Date: 2026-02-23 *)

(* :Mathematica Version: 12.3 *)
(* THE UNLICENSE *)

scriptPath = DirectoryName[ExpandFileName[First[$ScriptCommandLine]]];
startDirectory = Directory[];
SetDirectory[scriptPath];
<< "../Howl/HowlMidiTools.wl";

parseCliArgs[args_List] := Module[{pairs},
  pairs = StringCases[args, "--" ~~ key : (WordCharacter..) ~~ "=" ~~ value__ :> (key -> value)];
  Association[pairs]
];

argOrDefault[assoc_, key_, default_] := Lookup[assoc, key, default];

loadJsonConfig[path_] := Module[{cfg},
  If[StringLength[path] == 0, Return[<||>]];
  cfg = Quiet@Check[Import[path, "RawJSON"], $Failed];
  If[AssociationQ[cfg], cfg, <||>]
];

rawNumber[val_] := Which[
  NumberQ[val], val,
  StringQ[val], Module[{stream, parsed},
    stream = StringToStream[val];
    parsed = Quiet@Check[Read[stream, Number], $Failed];
    Close[stream];
    parsed
  ],
  True, $Failed
];

toIntValue[val_, default_] := Module[{parsed = rawNumber[val]},
  If[IntegerQ[parsed], parsed, default]
];

toRealValue[val_, default_] := Module[{parsed = rawNumber[val]},
  If[NumericQ[parsed], parsed, default]
];

toPositiveIntValue[val_, default_] := Max[1, Round@toIntValue[val, default]];

<<<<<<< HEAD
=======
<<<<<<< HEAD
>>>>>>> origin/improvements
resolvePath[path_String] := Module[{normalized = StringReplace[path, "\\" -> "/"]},
  If[
    StringLength[normalized] == 0,
    normalized,
    If[AbsoluteFileNameQ[normalized], normalized, ExpandFileName[normalized, startDirectory]]
  ]
<<<<<<< HEAD
];

=======
=======
resolvePath[path_String] := If[
  StringLength[path] == 0,
  path,
  If[AbsoluteFileNameQ[path], path, ExpandFileName[path, startDirectory]]
>>>>>>> origin/improvements
];
>>>>>>> origin/improvements
toTimeSigValue[val_, default_] := Which[
  ListQ[val] && Length[val] == 2,
    {toPositiveIntValue[val[[1]], default[[1]]], toPositiveIntValue[val[[2]], default[[2]]]},
  StringQ[val], Module[{parts = StringSplit[val, "/"]},
    If[Length[parts] == 2,
      {toPositiveIntValue[parts[[1]], default[[1]]], toPositiveIntValue[parts[[2]], default[[2]]]},
      default
    ]
  ],
  True, default
];

quantizeEncoded[encoded_, beatSeconds_, quantizationDiv_] := Module[
  {grid = beatSeconds/quantizationDiv, starts, ends, qStarts, qEnds},
  starts = Accumulate[encoded[[All, 1]]];
  ends = starts + encoded[[All, 2]];
  qStarts = Round[starts/grid]*grid;
  qEnds = Round[ends/grid]*grid;
  qEnds = MapThread[Max, {qEnds, qStarts + grid}];
  Transpose@{
    Prepend[Differences[qStarts], qStarts[[1]]],
    qEnds - qStarts,
    Clip[encoded[[All, 3]], {0.1, 1.0}],
    encoded[[All, 4]]
  }
];

trimToBars[encoded_, beatSeconds_, timeSig_, bars_] := Module[
  {barSeconds, starts, mask},
  barSeconds = timeSig[[1]] * beatSeconds;
  starts = Accumulate[encoded[[All, 1]]];
  mask = starts <= bars * barSeconds;
  Pick[encoded, mask]
];

applySimpleStructure[encoded_, structure_] := Module[
  {transpose, pitchMin, pitchMax, adjusted},
  If[!AssociationQ[structure], Return[encoded]];
  transpose = toIntValue[Lookup[structure, "transpose", 0], 0);
  pitchMin = Clip[toIntValue[Lookup[structure, "pitchMin", 0], 0], {0, 127}];
  pitchMax = Clip[toIntValue[Lookup[structure, "pitchMax", 127], 127], {pitchMin, 127}];
  adjusted = encoded;
  adjusted[[All, 4]] = Clip[adjusted[[All, 4]] + transpose, {pitchMin, pitchMax}];
  adjusted
];

sampleNext[predictor_, notes_, noteData_] := Module[{pred},
  pred = predictor[<|"Notes" -> notes, "NoteData" -> NumericArray[noteData, "Real32"]|>];
  {
    pred["NotesPred"],
    pred["NoteDataPred"]
  }
];

rawArgs = Join[
  If[ListQ[$ScriptCommandLine] && Length[$ScriptCommandLine] > 0, Rest[$ScriptCommandLine], {}],
  If[ListQ[$CommandLine], Select[$CommandLine, StringStartsQ["--"]], {}]
];
args = parseCliArgs[DeleteDuplicates[rawArgs]];

predictorPath = argOrDefault[args, "predictor", ""];
outPath = argOrDefault[args, "out", FileNameJoin[{scriptPath, "generated.mid"}]];
(* wolframscript reserves --config for its own settings; use --runConfig for this script. *)
<<<<<<< HEAD
=======
<<<<<<< HEAD
>>>>>>> origin/improvements
configPath = argOrDefault[args, "runConfig", ""];
If[StringLength[configPath] == 0,
  configPath = argOrDefault[args, "config", ""]
];
<<<<<<< HEAD
=======
=======
configPath = argOrDefault[args, "runConfig", argOrDefault[args, "config", ""]);
>>>>>>> origin/improvements
>>>>>>> origin/improvements
predictorPath = resolvePath[predictorPath];
outPath = resolvePath[outPath];
configPath = resolvePath[configPath];
config = loadJsonConfig[configPath];
argOrCfg[key_, default_] := Lookup[config, key, argOrDefault[args, key, default]];

steps = toPositiveIntValue[argOrCfg["steps", 400], 400];
seedPitch = toIntValue[argOrCfg["seedPitch", 60], 60);
seedDelay = Max[0.0, toRealValue[argOrCfg["seedDelay", 0.25], 0.25]];
seedDuration = Max[0.02, toRealValue[argOrCfg["seedDuration", 0.3], 0.3]];
seedVolume = Clip[toRealValue[argOrCfg["seedVolume", 0.8], 0.8), {0.1, 1.0}];
bpm = Max[30.0, toRealValue[argOrCfg["bpm", 84.0], 84.0]];
timeSignature = toTimeSigValue[argOrCfg["timeSig", {4, 4}], {4, 4}];
quantizationDiv = Max[1, toPositiveIntValue[argOrCfg["quantizationDiv", 4], 4]];
bars = Max[1, toPositiveIntValue[argOrCfg["bars", 16], 16]];
structure = Lookup[config, "structure", <||>];
If[AssociationQ[structure] && KeyExistsQ[structure, "bars"],
  bars = Max[1, toPositiveIntValue[structure["bars"], bars]]
];

If[StringLength[predictorPath] == 0,
  Print["Missing --predictor=/path/to/predictor.wlnet"]; Exit[1]
];

predictor = Import[predictorPath];
If[FailureQ[predictor],
  Print["Failed to import predictor net: ", predictorPath]; Exit[1]
];

notes = {seedPitch};
noteData = {{seedDelay, seedDuration, seedVolume}};

Do[
  Module[{nextPitch, nextData},
    {nextPitch, nextData} = sampleNext[predictor, notes, noteData];
    AppendTo[notes, Clip[toIntValue[nextPitch, seedPitch], {0, 127}]];
    AppendTo[noteData, {
      Max[0.0, toRealValue[nextData[[1]], seedDelay]],
      Max[0.02, toRealValue[nextData[[2]], seedDuration]],
      Clip[toRealValue[nextData[[3]], seedVolume], {0.1, 1.0}]
    }];
  ],
  {steps}
];

encoded = Transpose@{
  noteData[[All, 1]],
  noteData[[All, 2]],
  noteData[[All, 3]],
  notes
};

encoded = applySimpleStructure[encoded, structure];
beatSeconds = 60.0 / bpm;
structured = encoded // quantizeEncoded[#, beatSeconds, quantizationDiv] & // trimToBars[#, beatSeconds, timeSignature, bars] &;

If[Length[structured] < 2,
  Print["Generated note sequence is empty after structuring. Try larger --steps or --bars."]; Exit[1]
];

sound = Sound[HowlDecodeNotesV1[structured]];
CreateDirectory[DirectoryName[outPath], CreateIntermediateDirectories -> True];
Export[outPath, sound, "MIDI"];

Print["Generated MIDI: ", outPath];
Print["BPM target: ", bpm, ", Time signature target: ", StringRiffle[ToString /@ timeSignature, "/"]];
Print["Notes exported: ", Length[structured]];

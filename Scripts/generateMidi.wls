(* Mathematica Script *)
(* :Author: Howl contributors *)
(* :Date: 2026-02-23 *)

(* :Mathematica Version: 12.3 *)
(* THE UNLICENSE *)

scriptPath = DirectoryName[ExpandFileName[First[$ScriptCommandLine]]];
SetDirectory[scriptPath];
<< "../Howl/HowlMidiTools.wl";

parseCliArgs[args_List] := Module[{pairs},
  pairs = StringCases[args, "--" ~~ key : (WordCharacter..) ~~ "=" ~~ value__ :> (key -> value)];
  Association[pairs]
];

argOrDefault[assoc_, key_, default_] := Lookup[assoc, key, default];

toInt[str_, default_] := Module[{parsed},
  parsed = Quiet@Check[ToExpression[str], $Failed];
  If[IntegerQ[parsed], parsed, default]
];

toReal[str_, default_] := Module[{parsed},
  parsed = Quiet@Check[ToExpression[str], $Failed];
  If[NumericQ[parsed], parsed, default]
];

toPositiveInt[str_, default_] := Max[1, Round@toInt[str, default]];

toTimeSig[str_, default_] := Module[{parts},
  parts = StringSplit[str, "/"];
  If[Length[parts] == 2,
    {toPositiveInt[parts[[1]], default[[1]]], toPositiveInt[parts[[2]], default[[2]]]},
    default
  ]
];

(* Snap note start/end to beat grid and keep durations > 0 *)
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
steps = toPositiveInt[argOrDefault[args, "steps", "400"], 400);
seedPitch = toInt[argOrDefault[args, "seedPitch", "0"], 0);
seedDelay = Max[0.0, toReal[argOrDefault[args, "seedDelay", "0.25"], 0.25)];
seedDuration = Max[0.02, toReal[argOrDefault[args, "seedDuration", "0.3"], 0.3)];
seedVolume = Clip[toReal[argOrDefault[args, "seedVolume", "0.8"], 0.8), {0.1, 1.0}];
bpm = Max[30.0, toReal[argOrDefault[args, "bpm", "84"], 84.0)];
timeSignature = toTimeSig[argOrDefault[args, "timeSig", "4/4"], {4, 4});
quantizationDiv = Max[1, toPositiveInt[argOrDefault[args, "quantizationDiv", "4"], 4)];
bars = Max[1, toPositiveInt[argOrDefault[args, "bars", "16"], 16)];

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
    AppendTo[notes, nextPitch];
    AppendTo[noteData, nextData];
  ],
  {steps}
];

encoded = Transpose@{
  noteData[[All, 1]],
  noteData[[All, 2]],
  noteData[[All, 3]],
  notes
};

beatSeconds = 60.0 / bpm;
structured = encoded // quantizeEncoded[#, beatSeconds, quantizationDiv] & // trimToBars[#, beatSeconds, timeSignature, bars] &;

If[Length[structured] < 2,
  Print["Generated note sequence is empty after structuring. Try larger --steps or --bars."]; Exit[1]
];

sound = Sound[HowlDecodeNotesV1[structured]];
Export[outPath, sound, "MIDI"];

Print["Generated MIDI: ", outPath];
Print["BPM target: ", bpm, ", Time signature target: ", StringRiffle[ToString /@ timeSignature, "/"]];
Print["Notes exported: ", Length[structured]];

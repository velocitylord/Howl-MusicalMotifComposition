#!/usr/bin/env wolframscript

(* Usage:
   wolframscript -file Scripts/improviseFromPiece.wls \
     <predictorFile> <datasetFile> <pieceQuery> <startSec> <endSec> [mode] [numGenerated] [outputMidi] [harmonyKey]

   mode: "melody" (default) or "harmony"
   harmonyKey (optional, harmony mode): "auto" (default), "C major", "A minor", etc.
*)

repoRoot = DirectoryName[DirectoryName[ExpandFileName[First[$ScriptCommandLine]]]];
SetDirectory[repoRoot];

If[Length[$ScriptCommandLine] < 6,
  Print[
    "Usage: wolframscript -file Scripts/improviseFromPiece.wls <predictorFile> <datasetFile> <pieceQuery> <startSec> <endSec> [mode] [numGenerated] [outputMidi] [harmonyKey]"
  ];
  Exit[1];
];

predictorPath = $ScriptCommandLine[[2]];
datasetPath = $ScriptCommandLine[[3]];
pieceQuery = $ScriptCommandLine[[4]];
startSec = Interpreter["Number"][$ScriptCommandLine[[5]]];
endSec = Interpreter["Number"][$ScriptCommandLine[[6]]];
mode = If[Length[$ScriptCommandLine] >= 7, ToLowerCase[$ScriptCommandLine[[7]]], "melody"];
numGenerated = If[Length[$ScriptCommandLine] >= 8, Interpreter["Integer"][$ScriptCommandLine[[8]]], 256];
outputMidi = If[Length[$ScriptCommandLine] >= 9, $ScriptCommandLine[[9]], "Scripts/improvisation.mid"];
harmonyKeyArg = If[Length[$ScriptCommandLine] >= 10, $ScriptCommandLine[[10]], "auto"];

If[FailureQ[startSec] || FailureQ[endSec] || endSec <= startSec,
  Print["Invalid timestamp range. Example: 4 25"]; Exit[1];
];

If[FailureQ[numGenerated] || numGenerated < 1,
  Print["numGenerated must be a positive integer."]; Exit[1];
];

If[!MemberQ[{"melody", "harmony"}, mode],
  Print["mode must be 'melody' or 'harmony'."]; Exit[1];
];

Get["Howl/HowlMidiTools.wl"];

dataset = Import[datasetPath];
If[FailureQ[dataset],
  Print["Failed to load dataset file: ", datasetPath];
  Exit[1];
];

predictor = Import[predictorPath];
If[FailureQ[predictor],
  Print["Failed to load predictor: ", predictorPath];
  Exit[1];
];

validDataset = Select[
  dataset,
  AssociationQ[#] && KeyExistsQ[#, "File"] && KeyExistsQ[#, "EncodedNotesV1"] && !FailureQ[#["EncodedNotesV1"]] &
];

matches = Select[
  validDataset,
  StringContainsQ[ToLowerCase[FileNameTake[#["File"]]], ToLowerCase[pieceQuery]] &
];

If[Length[matches] == 0,
  Print["No piece matched query: ", pieceQuery];
  Exit[1];
];

selected = First[matches];
If[Length[matches] > 1,
  Print["Multiple matches found. Using first: ", selected["File"]];
];

Print["Selected piece: ", selected["File"]];

encodedToStartTimes[enc_] := With[{et = Transpose[enc]}, Transpose@Join[{Accumulate[et[[1]]]}, et[[2 ;;]], 1]];
startTimesToEncoded[raw_] := Module[{sorted, starts, durations, volumes, pitches, delays},
  sorted = SortBy[raw, First];
  starts = sorted[[All, 1]];
  durations = sorted[[All, 2]];
  volumes = sorted[[All, 3]];
  pitches = sorted[[All, 4]];
  delays = If[Length[starts] == 0, {}, Join[{starts[[1]]}, Differences[starts]]];
  Transpose[{delays, durations, volumes, pitches}]
];

(* Harmony helpers *)
ClearAll[notePcMap, pcToMajorName, majorScaleSteps, minorScaleSteps, buildScalePCs,
  parseKeySpec, scoreKeyFit, detectKeyFromSeed, circularDistance, nearestScaleDegree,
  triadPcsForDegree, bestHarmonyToneBelow, chooseHarmonyPitch];

notePcMap = <|
  "C" -> 0, "C#" -> 1, "DB" -> 1, "D" -> 2, "D#" -> 3, "EB" -> 3,
  "E" -> 4, "F" -> 5, "F#" -> 6, "GB" -> 6, "G" -> 7, "G#" -> 8,
  "AB" -> 8, "A" -> 9, "A#" -> 10, "BB" -> 10, "B" -> 11
|>;

pcToMajorName = <|0 -> "C", 1 -> "C#", 2 -> "D", 3 -> "Eb", 4 -> "E", 5 -> "F", 6 -> "F#", 7 -> "G", 8 -> "Ab", 9 -> "A", 10 -> "Bb", 11 -> "B"|>;
majorScaleSteps = {0, 2, 4, 5, 7, 9, 11};
minorScaleSteps = {0, 2, 3, 5, 7, 8, 10};

buildScalePCs[tonicPc_, scaleType_] := Mod[
  tonicPc + If[scaleType == "minor", minorScaleSteps, majorScaleSteps],
  12
];

parseKeySpec[key_String] := Module[{parts, tonicStr, scaleStr, tonicPc},
  parts = StringSplit[StringTrim[key]];
  If[Length[parts] < 2, Return[$Failed]];
  tonicStr = ToUpperCase[parts[[1]]];
  scaleStr = ToLowerCase[parts[[2]]];
  If[!MemberQ[{"major", "minor"}, scaleStr], Return[$Failed]];
  tonicPc = Lookup[notePcMap, tonicStr, Missing["UnknownTonic"]];
  If[MissingQ[tonicPc], Return[$Failed]];
  <|"TonicPC" -> tonicPc, "ScaleType" -> scaleStr, "ScalePCs" -> buildScalePCs[tonicPc, scaleStr]|>
];

scoreKeyFit[seedPitches_, seedDurations_, tonicPc_, scaleType_] := Module[{pcs, scalePcs, inScaleWeights, tonicWeights},
  pcs = Mod[seedPitches, 12];
  scalePcs = buildScalePCs[tonicPc, scaleType];
  inScaleWeights = Pick[seedDurations, MemberQ[scalePcs, #] & /@ pcs, True];
  tonicWeights = Pick[seedDurations, (# == tonicPc) & /@ pcs, True];
  Total[inScaleWeights] + 0.5 Total[tonicWeights]
];

detectKeyFromSeed[seedPitches_, seedDurations_] := Module[{candidates, best},
  candidates = Flatten[
    Table[
      With[{score = scoreKeyFit[seedPitches, seedDurations, tonic, scaleType]},
        <|"TonicPC" -> tonic, "ScaleType" -> scaleType, "Score" -> score|>
      ],
      {scaleType, {"major", "minor"}},
      {tonic, Range[0, 11]}
    ],
    1
  ];
  best = First@MaximalBy[candidates, #["Score"] &];
  Join[best, <|"ScalePCs" -> buildScalePCs[best["TonicPC"], best["ScaleType"]]|>]
];

circularDistance[a_, b_] := Min[Mod[a - b, 12], Mod[b - a, 12]];

nearestScaleDegree[pitchClass_, scalePcs_] := Module[{distances, minDist, idxs},
  distances = circularDistance[pitchClass, #] & /@ scalePcs;
  minDist = Min[distances];
  idxs = Flatten@Position[distances, minDist];
  First[idxs]
];

triadPcsForDegree[degree_, scalePcs_] := Module[{d = degree},
  {
    scalePcs[[d]],
    scalePcs[[Mod[d + 2, 7, 1]]],
    scalePcs[[Mod[d + 4, 7, 1]]]
  }
];

bestHarmonyToneBelow[melodyPitch_, triadPcs_] := Module[{melOct, candidates, below, inWindow, preferredIntervals, scored},
  melOct = Quotient[melodyPitch, 12];
  candidates = Flatten@Table[12 oct + pc, {oct, melOct - 3, melOct + 1}, {pc, triadPcs}];
  below = Select[candidates, # < melodyPitch &];
  inWindow = Select[below, 3 <= (melodyPitch - #) <= 16 &];
  If[Length[inWindow] == 0,
    If[Length[below] == 0, Return[melodyPitch - 7], inWindow = below]
  ];
  preferredIntervals = {4, 3, 7, 9, 12};
  scored = Map[
    Function[c,
      With[{interval = melodyPitch - c},
        {c, Min[Abs[preferredIntervals - interval]], -c}
      ]
    ],
    inWindow
  ];
  First@First@SortBy[scored, Rest]
];

chooseHarmonyPitch[melodyPitch_, keyProfile_] := Module[{scalePcs, degree, triadPcs},
  scalePcs = keyProfile["ScalePCs"];
  degree = nearestScaleDegree[Mod[melodyPitch, 12], scalePcs];
  triadPcs = triadPcsForDegree[degree, scalePcs];
  bestHarmonyToneBelow[melodyPitch, triadPcs]
];

encodedPiece = selected["EncodedNotesV1"];
pieceRaw = encodedToStartTimes[encodedPiece];

seedRaw = Select[pieceRaw, startSec <= #[[1]] <= endSec &];
If[Length[seedRaw] < 4,
  Print["Timestamp range contains too few notes (", Length[seedRaw], "). Try a wider range."];
  Exit[1];
];

seedEncoded = startTimesToEncoded[seedRaw];
seedNotes = seedEncoded[[All, 4]];
seedNoteData = seedEncoded[[All, 1 ;; 3]];

Print["Seed note count: ", Length[seedEncoded]];
Print["Generating ", numGenerated, " new notes in ", mode, " mode..."];

applyMelodyConstraint[nextPitch_, seedPitches_] := Module[{seedMedian = Median[seedPitches], constrained = nextPitch},
  If[constrained < seedMedian, constrained = constrained + 12];
  constrained
];

harmonyKeyProfile = None;
If[mode == "harmony",
  harmonyKeyProfile = If[ToLowerCase[StringTrim[harmonyKeyArg]] == "auto",
    detectKeyFromSeed[seedNotes, seedNoteData[[All, 2]]],
    parseKeySpec[harmonyKeyArg]
  ];

  If[FailureQ[harmonyKeyProfile] || harmonyKeyProfile === $Failed,
    Print["Could not parse harmonyKey '", harmonyKeyArg, "'. Use \"auto\" or like \"C major\" / \"A minor\"."];
    Exit[1];
  ];

  Print[
    "Harmony key: ",
    pcToMajorName[harmonyKeyProfile["TonicPC"]], " ",
    ToUpperCase[StringTake[harmonyKeyProfile["ScaleType"], 1]] <> StringDrop[harmonyKeyProfile["ScaleType"], 1]
  ];
];

Do[
  pred = predictor[<|
    "Notes" -> seedNotes,
    "NoteData" -> NumericArray[seedNoteData, "Real32"]
  |>];

  nextPitch = pred["NotesPred"];
  nextData = pred["NoteDataPred"];

  If[mode == "melody",
    nextPitch = applyMelodyConstraint[nextPitch, seedNotes];
  ];

  AppendTo[seedNotes, nextPitch];
  AppendTo[seedNoteData, nextData];
, {numGenerated}];

fullEncoded = Transpose@{seedNoteData[[All, 1]], seedNoteData[[All, 2]], seedNoteData[[All, 3]], seedNotes};

decoded = HowlDecodeNotesV1[fullEncoded];

finalNotes = If[mode == "harmony",
  Module[{baseNotes, harmonyNotes},
    baseNotes = decoded;
    harmonyNotes = Cases[
      baseNotes,
      SoundNote[p_, t_, rest___] :>
        Module[{melPitch = HowlNoteToInt[p], harmPitch},
          harmPitch = chooseHarmonyPitch[melPitch, harmonyKeyProfile];
          SoundNote[HowlIntToNote[harmPitch], t, SoundVolume -> 0.62]
        ]
    ];
    Join[baseNotes, harmonyNotes]
  ],
  decoded
];

sortedFinalNotes = SortBy[finalNotes, First[Replace[#, SoundNote[_, x_, __] :> x]] &];
Export[outputMidi, Sound[sortedFinalNotes], "MIDI"];
Print["Saved improvisation MIDI: ", outputMidi];

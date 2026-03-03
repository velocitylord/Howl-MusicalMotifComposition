(* Notebook helper for short-piece generation from predictor_*.wlnet *)

Get[FileNameJoin[{DirectoryName[DirectoryName[$InputFileName]], "Howl", "HowlMidiTools.wl"}]];

ClearAll[HowlCoercePitch, HowlCanonicalizeEncoded, HowlEncToNetInput, HowlFromPred,
  HowlFirstNote, HowlAntiRepeatStep, HowlSmoothNoteDataStep, HowlLimitLeapStep,
  HowlQualityStep, GenerateShortPieceFromPredictor];

HowlCoercePitch[p_] := HowlNoteToInt[p];

HowlCanonicalizeEncoded[enc_] := Module[{clean = N[enc]},
  If[Length[clean] == 0, Return[clean]];
  clean[[All, 1]] = Max[0.001, #] & /@ clean[[All, 1]];
  clean[[All, 2]] = Max[0.05, #] & /@ clean[[All, 2]];
  clean[[All, 3]] = Clip[#, {0.1, 1.0}] & /@ clean[[All, 3]];
  clean[[All, 4]] = HowlCoercePitch /@ clean[[All, 4]];
  clean
];

HowlEncToNetInput[encSong_] := With[{clean = HowlCanonicalizeEncoded[encSong]}, <|
  "NoteData" -> NumericArray[clean[[All, 1 ;; 3]], "Real32"],
  "Notes" -> Round[clean[[All, 4]]]
|>];

HowlFromPred[pred_, fallback_] := Module[{pData, pNote, row},
  pData = Lookup[pred, "NoteDataPred", fallback[[1 ;; 3]]];
  pNote = Lookup[pred, "NotesPred", fallback[[4]]];
  If[Head[pData] === NumericArray, pData = Normal[pData]];
  If[!ListQ[pData] || Length[pData] != 3 || !And @@ (NumericQ /@ pData), pData = fallback[[1 ;; 3]]];
  If[!NumericQ[pNote], pNote = fallback[[4]]];
  row = N@Join[pData, {HowlCoercePitch[pNote]}];
  First@HowlCanonicalizeEncoded[{row}]
];

HowlFirstNote[] := {{RandomReal[{0.02, 0.12}], RandomReal[{0.12, 0.48}], RandomReal[{0.65, 0.95}], RandomInteger[{-12, 12}]}};

HowlAntiRepeatStep[seq_, runWindow_ : 6, uniqueFloor_ : 3] := Module[{out = seq, n = Length[seq], recent},
  If[n >= runWindow,
    recent = out[[-runWindow ;;, 4]];
    If[Length[Union[recent]] <= uniqueFloor,
      out[[-1, 4]] = Clip[out[[-1, 4]] + RandomChoice[{-7, -5, -4, 4, 5, 7}], {-24, 24}]
    ]
  ];
  out
];

HowlSmoothNoteDataStep[seq_] := Module[{out = seq, n = Length[seq]},
  If[n >= 2,
    out[[-1, 1]] = Clip[0.70*out[[-1, 1]] + 0.30*out[[-2, 1]], {0.01, 0.75}];
    out[[-1, 2]] = Clip[0.75*out[[-1, 2]] + 0.25*out[[-2, 2]], {0.06, 1.60}];
    out[[-1, 3]] = Clip[0.70*out[[-1, 3]] + 0.30*out[[-2, 3]], {0.20, 1.00}]
  ];
  out
];

HowlLimitLeapStep[seq_, maxLeap_ : 9] := Module[{out = seq, n = Length[seq], leap},
  If[n >= 2,
    leap = out[[-1, 4]] - out[[-2, 4]];
    If[Abs[leap] > maxLeap, out[[-1, 4]] = out[[-2, 4]] + Sign[leap]*maxLeap]
  ];
  out
];

HowlQualityStep[seq_] := HowlSmoothNoteDataStep@HowlLimitLeapStep@HowlAntiRepeatStep@seq;

GenerateShortPieceFromPredictor[predictorPath_String, outputMidi_String : "Scripts/generated_short_piece.mid", steps_Integer : 128, context_Integer : 320, seed_Integer : 1337] :=
 Module[{predictor, encoded, generatedSound, pred, input, fallback, cleaned},
  SeedRandom[seed];
  predictor = Import[predictorPath];
  If[FailureQ[predictor], Print["Failed to import predictor: ", predictorPath]; Return[$Failed]];

  encoded = Nest[
    Function[curr,
      input = HowlEncToNetInput[curr[[-Min[Length[curr], context] ;;]]];
      pred = Quiet@Check[predictor[input, TargetDevice -> "CPU"], $Failed];
      fallback = Last[curr];
      HowlQualityStep@Join[curr, {HowlFromPred[If[pred === $Failed, <||>, pred], fallback]}]
    ],
    HowlFirstNote[],
    steps
  ];

  cleaned = HowlCanonicalizeEncoded[encoded];
  generatedSound = Sound[HowlDecodeNotesV1[cleaned]];
  Export[outputMidi, generatedSound, "MIDI"];
  Print["Exported MIDI: ", outputMidi];
  <|"Sound" -> generatedSound, "Encoded" -> cleaned, "OutputMidi" -> outputMidi|>
];

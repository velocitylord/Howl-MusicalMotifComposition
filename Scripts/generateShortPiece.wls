#!/usr/bin/env wolframscript

(*
  Usage:
  wolframscript -file Scripts/generateShortPiece.wls <predictorFile> [outputMidi] [steps] [context] [seed]

  Generates a short MIDI phrase directly from an exported predictor net.
*)

repoRoot = DirectoryName[DirectoryName[ExpandFileName[First[$ScriptCommandLine]]]];
SetDirectory[repoRoot];

If[Length[$ScriptCommandLine] < 2,
  Print["Usage: wolframscript -file Scripts/generateShortPiece.wls <predictorFile> [outputMidi] [steps] [context] [seed]"];
  Exit[1];
];

predictorPath = $ScriptCommandLine[[2]];
outputMidi = If[Length[$ScriptCommandLine] >= 3, $ScriptCommandLine[[3]], "Scripts/generated_short_piece.mid"];
steps = If[Length[$ScriptCommandLine] >= 4, Interpreter["Integer"][$ScriptCommandLine[[4]]], 128];
context = If[Length[$ScriptCommandLine] >= 5, Interpreter["Integer"][$ScriptCommandLine[[5]]], 320];
seed = If[Length[$ScriptCommandLine] >= 6, Interpreter["Integer"][$ScriptCommandLine[[6]]], 1337];

If[FailureQ[steps] || steps < 8, Print["steps must be an integer >= 8"]; Exit[1]];
If[FailureQ[context] || context < 8, Print["context must be an integer >= 8"]; Exit[1]];
If[FailureQ[seed], Print["seed must be an integer"]; Exit[1]];

Get["Howl/HowlMidiTools.wl"];
SeedRandom[seed];

predictor = Import[predictorPath];
If[FailureQ[predictor], Print["Failed to import predictor: ", predictorPath]; Exit[1]];

encToNetInput[encSong_] := <|"NoteData" -> N[encSong[[All, 1 ;; 3]]], "Notes" -> Round[encSong[[All, 4]]]|>;

fromPred[pred_, fallback_] := Module[{pData, pNote},
  pData = Lookup[pred, "NoteDataPred", fallback[[1 ;; 3]]];
  pNote = Lookup[pred, "NotesPred", fallback[[4]]];
  If[Head[pData] === NumericArray, pData = Normal[pData]];
  If[!ListQ[pData] || Length[pData] != 3 || !And @@ (NumericQ /@ pData), pData = fallback[[1 ;; 3]]];
  If[!NumericQ[pNote], pNote = fallback[[4]]];
  N@Join[pData, {Round[pNote]}]
];

firstNote[] := {{RandomReal[{0.02, 0.12}], RandomReal[{0.12, 0.48}], RandomReal[{0.65, 0.95}], RandomInteger[{-12, 12}]}};

antiRepeatStep[seq_, runWindow_ : 6, uniqueFloor_ : 3] := Module[{out = seq, n = Length[seq], recent},
  If[n >= runWindow,
    recent = out[[-runWindow ;;, 4]];
    If[Length[Union[recent]] <= uniqueFloor,
      out[[-1, 4]] = Clip[out[[-1, 4]] + RandomChoice[{-7, -5, -4, 4, 5, 7}], {-24, 24}]
    ]
  ];
  out
];

smoothNoteDataStep[seq_] := Module[{out = seq, n = Length[seq]},
  If[n >= 2,
    out[[-1, 1]] = Clip[0.70*out[[-1, 1]] + 0.30*out[[-2, 1]], {0.01, 0.75}];
    out[[-1, 2]] = Clip[0.75*out[[-1, 2]] + 0.25*out[[-2, 2]], {0.06, 1.60}];
    out[[-1, 3]] = Clip[0.70*out[[-1, 3]] + 0.30*out[[-2, 3]], {0.20, 1.00}]
  ];
  out
];

limitLeapStep[seq_, maxLeap_ : 9] := Module[{out = seq, n = Length[seq], leap},
  If[n >= 2,
    leap = out[[-1, 4]] - out[[-2, 4]];
    If[Abs[leap] > maxLeap, out[[-1, 4]] = out[[-2, 4]] + Sign[leap]*maxLeap]
  ];
  out
];

qualityStep[seq_] := smoothNoteDataStep@limitLeapStep@antiRepeatStep@seq;

makeMusicShort[predictor_, start_, len_ : 128, ctx_ : 320] := Module[{encoded, generatedSound, pred, input, fallback},
  encoded = Nest[
    Function[curr,
      input = encToNetInput[curr[[-Min[Length[curr], ctx] ;;]]];
      pred = Quiet@Check[predictor[input, TargetDevice -> "CPU"], $Failed];
      fallback = Last[curr];
      qualityStep@Join[curr, {fromPred[If[pred === $Failed, <||>, pred], fallback]}]
    ],
    start,
    len
  ];
  generatedSound = Sound[HowlDecodeNotesV1[encoded]];
  Export[outputMidi, generatedSound, "MIDI"];
  Print["Exported MIDI: ", outputMidi];
  generatedSound
];

makeMusicShort[predictor, firstNote[], steps, context];

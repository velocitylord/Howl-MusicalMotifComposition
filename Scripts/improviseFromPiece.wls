#!/usr/bin/env wolframscript

(* Usage:
   wolframscript -file Scripts/improviseFromPiece.wls \
     <predictorFile> <datasetFile> <pieceQuery> <startSec> <endSec> [outputMidi] [maxPitchDelta] [maxTimingFrac] [keepOriginalProb] [analysisJson]

   outputMidi (optional): output path, default "Scripts/rework.mid"
   maxPitchDelta (optional): max semitone deviation per note, default 2
   maxTimingFrac (optional): max relative timing/duration deviation, default 0.12
   keepOriginalProb (optional): probability to keep each original note unchanged, default 0.35
   analysisJson (optional): save bar-by-bar key/chord analysis, default "Scripts/rework_analysis.json"
*)

repoRoot = DirectoryName[DirectoryName[ExpandFileName[First[$ScriptCommandLine]]]];
SetDirectory[repoRoot];

If[Length[$ScriptCommandLine] < 6,
  Print[
    "Usage: wolframscript -file Scripts/improviseFromPiece.wls <predictorFile> <datasetFile> <pieceQuery> <startSec> <endSec> [outputMidi] [maxPitchDelta] [maxTimingFrac] [keepOriginalProb] [analysisJson]"
  ];
  Exit[1];
];

predictorPath = $ScriptCommandLine[[2]];
datasetPath = $ScriptCommandLine[[3]];
pieceQuery = $ScriptCommandLine[[4]];
startSec = Interpreter["Number"][$ScriptCommandLine[[5]]];
endSec = Interpreter["Number"][$ScriptCommandLine[[6]]];
outputMidi = If[Length[$ScriptCommandLine] >= 7, $ScriptCommandLine[[7]], "Scripts/rework.mid"];
maxPitchDelta = If[Length[$ScriptCommandLine] >= 8, Interpreter["Integer"][$ScriptCommandLine[[8]]], 2];
maxTimingFrac = If[Length[$ScriptCommandLine] >= 9, Interpreter["Number"][$ScriptCommandLine[[9]]], 0.12];
keepOriginalProb = If[Length[$ScriptCommandLine] >= 10, Interpreter["Number"][$ScriptCommandLine[[10]]], 0.35];
analysisJson = If[Length[$ScriptCommandLine] >= 11, $ScriptCommandLine[[11]], "Scripts/rework_analysis.json"];

If[FailureQ[startSec] || FailureQ[endSec] || endSec <= startSec,
  Print["Invalid timestamp range. Example: 4 25"]; Exit[1];
];

If[FailureQ[maxPitchDelta] || maxPitchDelta < 0,
  Print["maxPitchDelta must be a non-negative integer."]; Exit[1];
];

If[FailureQ[maxTimingFrac] || maxTimingFrac < 0 || maxTimingFrac > 0.8,
  Print["maxTimingFrac must be in [0, 0.8]."]; Exit[1];
];

If[FailureQ[keepOriginalProb] || keepOriginalProb < 0 || keepOriginalProb > 1,
  Print["keepOriginalProb must be in [0, 1]."]; Exit[1];
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
  AssociationQ[#] && KeyExistsQ[#, "File"] && KeyExistsQ[#, "EncodedNotesV1"] && !FailureQ[# ["EncodedNotesV1"]] &
];

matches = Select[
  validDataset,
  StringContainsQ[ToLowerCase[FileNameTake[# ["File"]]], ToLowerCase[pieceQuery]] &
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

(* ---------- Chord/key helpers ---------- *)
ClearAll[noteNames, noteNameForPc, majorScaleSteps, minorScaleSteps, buildScalePCs,
  scoreKeyFit, detectKeyFromPcs, chordTemplateCategories, flattenChordTemplates,
  chordNameFor, buildRootedCandidates, overlapWeight, barWindows, estimateBeatsPerBar,
  estimateBeatSeconds, scoreChordCandidate, bestChordForWindow, chordTimelineForWindow,
  nearestAllowedPitch, keyByBar, activeChordAtTime, normalizePredNoteData, normalizePredPitch];

noteNames = <|0 -> "C", 1 -> "C#", 2 -> "D", 3 -> "Eb", 4 -> "E", 5 -> "F", 6 -> "F#", 7 -> "G", 8 -> "Ab", 9 -> "A", 10 -> "Bb", 11 -> "B"|>;
noteNameForPc[pc_] := Lookup[noteNames, Mod[pc, 12], "C"];
majorScaleSteps = {0, 2, 4, 5, 7, 9, 11};
minorScaleSteps = {0, 2, 3, 5, 7, 8, 10};
buildScalePCs[tonicPc_, scaleType_] := Mod[tonicPc + If[scaleType == "minor", minorScaleSteps, majorScaleSteps], 12];

scoreKeyFit[pcs_, w_, tonicPc_, scaleType_] := Module[{scalePcs, inScaleW, tonicW},
  scalePcs = buildScalePCs[tonicPc, scaleType];
  inScaleW = Total@Pick[w, MemberQ[scalePcs, #] & /@ pcs, True];
  tonicW = Total@Pick[w, (# == tonicPc) & /@ pcs, True];
  inScaleW + 0.45 tonicW
];

detectKeyFromPcs[pcs_, weights_] := Module[{candidates, best},
  candidates = Flatten@Table[
    <|"TonicPC" -> tonic, "ScaleType" -> scale, "Score" -> scoreKeyFit[pcs, weights, tonic, scale]|>,
    {scale, {"major", "minor"}}, {tonic, 0, 11}
  ];
  best = First@MaximalBy[candidates, # ["Score"] &];
  Join[best, <|"Name" -> (noteNameForPc[best["TonicPC"]] <> " " <> best["ScaleType"]), "ScalePCs" -> buildScalePCs[best["TonicPC"], best["ScaleType"]]|>]
];

chordTemplateCategories = <|
  "Triads" -> <|
    "maj" -> {0, 4, 7}, "min" -> {0, 3, 7}, "dim" -> {0, 3, 6}, "aug" -> {0, 4, 8},
    "sus2" -> {0, 2, 7}, "sus4" -> {0, 5, 7}
  |>,
  "Sixths" -> <|
    "6" -> {0, 4, 7, 9}, "m6" -> {0, 3, 7, 9}, "69" -> {0, 2, 4, 7, 9}, "m69" -> {0, 2, 3, 7, 9}
  |>,
  "Sevenths" -> <|
    "7" -> {0, 4, 7, 10}, "maj7" -> {0, 4, 7, 11}, "min7" -> {0, 3, 7, 10},
    "mMaj7" -> {0, 3, 7, 11}, "hdim7" -> {0, 3, 6, 10}, "dim7" -> {0, 3, 6, 9},
    "7sus4" -> {0, 5, 7, 10}, "aug7" -> {0, 4, 8, 10}, "augMaj7" -> {0, 4, 8, 11}
  |>,
  "Ninths" -> <|
    "add9" -> {0, 2, 4, 7}, "madd9" -> {0, 2, 3, 7}, "9" -> {0, 2, 4, 7, 10},
    "maj9" -> {0, 2, 4, 7, 11}, "min9" -> {0, 2, 3, 7, 10}, "9sus4" -> {0, 2, 5, 7, 10}
  |>
|>;

flattenChordTemplates[] := Flatten[KeyValueMap[Function[{cat, assoc}, KeyValueMap[Function[{name, pcs}, <|"Category" -> cat, "Quality" -> name, "PCs" -> pcs|>], assoc]], chordTemplateCategories], 1];

chordNameFor[rootPc_, quality_] := noteNameForPc[rootPc] <> quality;

buildRootedCandidates[] := Flatten@Table[
  <|
    "RootPC" -> root,
    "Category" -> template["Category"],
    "Quality" -> template["Quality"],
    "PCs" -> Mod[root + template["PCs"], 12],
    "Label" -> chordNameFor[root, template["Quality"]]
  |>,
  {template, flattenChordTemplates[]}, {root, 0, 11}
];

overlapWeight[note_, windowStart_, windowEnd_] := Module[{noteStart, noteEnd, overlapSec, vol},
  noteStart = note[[1]];
  noteEnd = note[[1]] + note[[2]];
  overlapSec = Max[0, Min[noteEnd, windowEnd] - Max[noteStart, windowStart]];
  vol = note[[3]];
  overlapSec*(0.7 + 0.3*vol)
];

estimateBeatSeconds[rawNotes_] := Module[{starts, deltas, positive},
  starts = Sort[rawNotes[[All, 1]]];
  deltas = Differences[starts];
  positive = Select[deltas, # > 0.05 &];
  If[Length[positive] == 0, 0.5, Median[positive]]
];

estimateBeatsPerBar[file_] := Module[{sigCandidates, firstSig},
  sigCandidates = DeleteCases[
    Quiet@Check[{Import[file, {"MIDI", "TimeSignatures"}], Import[file, {"MIDI", "TimeSignature"}]}, {}],
    _Failure | $Failed | Missing[__] | {}
  ];
  If[Length[sigCandidates] == 0, Return[4]];
  firstSig = First@Flatten@sigCandidates;
  Which[
    MatchQ[firstSig, {_Integer, _Integer}], firstSig[[1]],
    MatchQ[firstSig, {_?NumericQ, _?NumericQ}], Round[firstSig[[1]]],
    AssociationQ[firstSig] && KeyExistsQ[firstSig, "Numerator"], firstSig["Numerator"],
    True, 4
  ]
];

barWindows[startT_, endT_, barSeconds_] := Module[{bars, barStarts},
  barStarts = Range[Floor[startT/barSeconds]*barSeconds, endT, barSeconds];
  bars = Map[<|"Start" -> #, "End" -> Min[# + barSeconds, endT]|> &, barStarts];
  Select[bars, # ["End"] > startT &]
];

scoreChordCandidate[notesInWindow_, candidate_] := Module[{weights, pcs, bassPc, chordPcs, inChord, outChord, bassBonus},
  If[Length[notesInWindow] == 0, Return[-Infinity]];
  weights = notesInWindow[[All, 5]];
  pcs = Mod[notesInWindow[[All, 4]], 12];
  bassPc = Mod[First@SortBy[notesInWindow, #[[4]] &][[4]], 12];
  chordPcs = candidate["PCs"];
  inChord = Total@Pick[weights, MemberQ[chordPcs, #] & /@ pcs, True];
  outChord = Total@Pick[weights, MemberQ[chordPcs, #] & /@ pcs, False];
  bassBonus = If[bassPc == candidate["RootPC"], 0.4, 0.0];
  inChord - 0.75 outChord + bassBonus
];

bestChordForWindow[rawNotes_, win_, rootedCandidates_] := Module[{notesInWindow, weighted, scored, best},
  notesInWindow = Select[rawNotes, (#[[1]] < win["End"]) && (#[[1]] + #[[2]] > win["Start"]) &];
  weighted = If[Length[notesInWindow] == 0, {}, Map[Append[#, overlapWeight[#, win["Start"], win["End"]]] &, notesInWindow]];
  If[Length[weighted] == 0,
    Return[<|"Start" -> win["Start"], "End" -> win["End"], "Chord" -> "N.C.", "Category" -> "None", "PCs" -> {}|>]
  ];
  scored = Map[Append[#, "Score" -> scoreChordCandidate[weighted, #]] &, rootedCandidates];
  best = First@MaximalBy[scored, # ["Score"] &];
  <|"Start" -> win["Start"], "End" -> win["End"], "Chord" -> best["Label"], "Category" -> best["Category"], "PCs" -> best["PCs"]|>
];

keyByBar[rawNotes_, win_] := Module[{notesInWindow, weighted, pcs, w},
  notesInWindow = Select[rawNotes, (#[[1]] < win["End"]) && (#[[1]] + #[[2]] > win["Start"]) &];
  weighted = If[Length[notesInWindow] == 0, {}, Map[Append[#, overlapWeight[#, win["Start"], win["End"]]] &, notesInWindow]];
  If[Length[weighted] == 0, Return[<|"Name" -> "Unknown", "ScalePCs" -> {}|>]];
  pcs = Mod[weighted[[All, 4]], 12];
  w = weighted[[All, 5]];
  detectKeyFromPcs[pcs, w]
];

activeChordAtTime[t_, timeline_] := Module[{match},
  match = SelectFirst[timeline, # ["Start"] <= t < # ["End"] &];
  If[match === Missing["NotFound"], Last[timeline], match]
];

nearestAllowedPitch[candidatePitch_, originalPitch_, allowedPcs_] := Module[{oct, pitchCandidates, sorted},
  If[Length[allowedPcs] == 0, Return[Round[candidatePitch]]];
  oct = Quotient[Round[candidatePitch], 12];
  pitchCandidates = Flatten@Table[12 o + pc, {o, oct - 2, oct + 2}, {pc, allowedPcs}];
  sorted = SortBy[pitchCandidates, {Abs[# - candidatePitch] &, Abs[# - originalPitch] &} &];
  First[sorted]
];

normalizePredNoteData[predDataRaw_, origData_] := Module[{candidate = predDataRaw, asList},
  asList = Which[
    FailureQ[candidate] || candidate === $Failed, origData,
    Head[candidate] === NumericArray, Normal[candidate],
    ListQ[candidate], candidate,
    True, origData
  ];
  If[!ListQ[asList] || Length[asList] != 3 || !And @@ (NumericQ /@ asList), origData, N[asList]]
];

normalizePredPitch[predPitchRaw_, origPitch_] := Module[{candidate = predPitchRaw},
  If[FailureQ[candidate] || candidate === $Failed || !NumericQ[candidate], origPitch, Round[candidate]]
];

(* ---------- Select and normalize target window ---------- *)
encodedPiece = selected["EncodedNotesV1"];
pieceRaw = encodedToStartTimes[encodedPiece];

windowRawAbsolute = Select[pieceRaw, startSec <= #[[1]] <= endSec &];
If[Length[windowRawAbsolute] < 8,
  Print["Timestamp range contains too few notes (", Length[windowRawAbsolute], "). Try a wider range."];
  Exit[1];
];

windowOffset = Min[windowRawAbsolute[[All, 1]]];
windowRaw = windowRawAbsolute;
windowRaw[[All, 1]] = windowRaw[[All, 1]] - windowOffset;
windowDuration = Max[windowRaw[[All, 1]] + windowRaw[[All, 2]]];

windowEncoded = startTimesToEncoded[windowRaw];
origNotes = windowEncoded[[All, 4]];
origNoteData = N[windowEncoded[[All, 1 ;; 3]]];

Print["Window note count: ", Length[windowEncoded]];
Print["Building per-bar key/chord timeline..."];

beatSeconds = estimateBeatSeconds[windowRaw];
beatsPerBar = estimateBeatsPerBar[selected["File"]];
barSeconds = Max[0.4, beatSeconds*beatsPerBar];
bars = barWindows[0, windowDuration, barSeconds];
rootedCandidates = buildRootedCandidates[];

barChordTimeline = bestChordForWindow[windowRaw, #, rootedCandidates] & /@ bars;
barKeyTimeline = keyByBar[windowRaw, #] & /@ bars;

timeline = MapThread[Join[#1, <|"Key" -> #2["Name"]|>] &, {barChordTimeline, barKeyTimeline}];

Print["Detected bars: ", Length[timeline], " (", ToString@NumberForm[barSeconds, {4, 2}], " sec each approx.)"];
Print["Reworking with maxPitchDelta=", maxPitchDelta, ", maxTimingFrac=", maxTimingFrac, ", keepOriginalProb=", keepOriginalProb];

(* ---------- Guided rework loop ---------- *)
rewriteNotes = Take[origNotes, UpTo[4]];
rewriteData = Take[origNoteData, UpTo[4]];
blendAlpha = 0.35;

Do[
  origPitch = origNotes[[i]];
  origData = origNoteData[[i]];

  predInput = <|"Notes" -> rewriteNotes, "NoteData" -> NumericArray[rewriteData, "Real32"]|>;
  predResult = Quiet@Check[predictor[predInput], $Failed];

  predPitchRaw = If[predResult === $Failed || FailureQ[predResult], origPitch, predResult["NotesPred"]];
  predDataRaw = If[predResult === $Failed || FailureQ[predResult], origData, predResult["NoteDataPred"]];

  predPitch = normalizePredPitch[predPitchRaw, origPitch];
  predData = normalizePredNoteData[predDataRaw, origData];

  keepOriginal = RandomReal[] < keepOriginalProb;

  If[keepOriginal,
    newPitch = origPitch;
    newData = origData;,
    pitchDelta = Clip[predPitch - Round[origPitch], {-maxPitchDelta, maxPitchDelta}];
    candidatePitch = Round[origPitch] + pitchDelta;

    barCtx = activeChordAtTime[Total[rewriteData[[All, 1]]] + origData[[1]], timeline];
    allowedPcs = If[AssociationQ[barCtx] && KeyExistsQ[barCtx, "PCs"], barCtx["PCs"], {}];
    newPitch = nearestAllowedPitch[candidatePitch, origPitch, allowedPcs];
    newPitch = Clip[newPitch, {-60, 59}];

    dataBlend = (1 - blendAlpha) origData + blendAlpha predData;
    newDelay = Clip[dataBlend[[1]], {origData[[1]]*(1 - maxTimingFrac), origData[[1]]*(1 + maxTimingFrac)}];
    newDur = Clip[dataBlend[[2]], {origData[[2]]*(1 - maxTimingFrac), origData[[2]]*(1 + maxTimingFrac)}];
    newVol = Clip[dataBlend[[3]], {Max[0.2, origData[[3]] - 0.25], Min[1.0, origData[[3]] + 0.25]}];
    newData = N[{newDelay, newDur, newVol}];
  ];

  AppendTo[rewriteNotes, Round[newPitch]];
  AppendTo[rewriteData, newData];
, {i, Length[rewriteNotes] + 1, Length[origNotes]}];

reworkedEncoded = Transpose@{rewriteData[[All, 1]], rewriteData[[All, 2]], rewriteData[[All, 3]], rewriteNotes};
reworkedNotes = HowlDecodeNotesV1[reworkedEncoded];

Export[outputMidi, Sound[reworkedNotes], "MIDI"];

analysis = <|
  "SelectedPiece" -> selected["File"],
  "WindowSeconds" -> <|"Start" -> startSec, "End" -> endSec|>,
  "EstimatedBeatSeconds" -> beatSeconds,
  "EstimatedBeatsPerBar" -> beatsPerBar,
  "EstimatedBarSeconds" -> barSeconds,
  "ChordTemplateCategories" -> chordTemplateCategories,
  "BarTimeline" -> timeline
|>;

Export[analysisJson, analysis, "JSON"];

Print["Saved rework MIDI: ", outputMidi];
Print["Saved bar-by-bar key/chord analysis: ", analysisJson];

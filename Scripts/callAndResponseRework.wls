#!/usr/bin/env wolframscript

(*
  Usage:
  wolframscript -file Scripts/callAndResponseRework.wls \
    <predictorFile> <datasetFile> <pieceQuery> <startSec> <endSec> [outputMidi] [analysisJson] [maxPitchDelta] [responseKeyShift] [rhythmBlend]

  Structure:
  - Bars 1-2: original melody
  - Bars 3-4: predictor-constrained response over original harmony
*)

repoRoot = DirectoryName[DirectoryName[ExpandFileName[First[$ScriptCommandLine]]]];
SetDirectory[repoRoot];

If[Length[$ScriptCommandLine] < 6,
  Print["Usage: wolframscript -file Scripts/callAndResponseRework.wls <predictorFile> <datasetFile> <pieceQuery> <startSec> <endSec> [outputMidi] [analysisJson] [maxPitchDelta] [responseKeyShift] [rhythmBlend]"];
  Exit[1];
];

predictorPath = $ScriptCommandLine[[2]];
datasetPath = $ScriptCommandLine[[3]];
pieceQuery = $ScriptCommandLine[[4]];
startSec = Interpreter["Number"][$ScriptCommandLine[[5]]];
endSec = Interpreter["Number"][$ScriptCommandLine[[6]]];
outputMidi = If[Length[$ScriptCommandLine] >= 7, $ScriptCommandLine[[7]], "Scripts/call_response_rework.mid"];
analysisJson = If[Length[$ScriptCommandLine] >= 8, $ScriptCommandLine[[8]], "Scripts/call_response_rework_analysis.json"];
maxPitchDelta = If[Length[$ScriptCommandLine] >= 9, Interpreter["Integer"][$ScriptCommandLine[[9]]], 2];
responseKeyShift = If[Length[$ScriptCommandLine] >= 10, Interpreter["Integer"][$ScriptCommandLine[[10]]], 0];
rhythmBlend = If[Length[$ScriptCommandLine] >= 11, Interpreter["Number"][$ScriptCommandLine[[11]]], 0.15];

If[FailureQ[startSec] || FailureQ[endSec] || endSec <= startSec, Print["Invalid timestamp range."]; Exit[1]];
If[FailureQ[maxPitchDelta] || maxPitchDelta < 0, Print["maxPitchDelta must be >= 0."]; Exit[1]];
If[FailureQ[responseKeyShift], Print["responseKeyShift must be integer semitones."]; Exit[1]];
If[FailureQ[rhythmBlend] || rhythmBlend < 0 || rhythmBlend > 1, Print["rhythmBlend must be in [0,1]."]; Exit[1]];

Get["Howl/HowlMidiTools.wl"];
dataset = Import[datasetPath];
predictor = Import[predictorPath];
If[FailureQ[dataset] || FailureQ[predictor], Print["Failed to load dataset or predictor."]; Exit[1]];

encodedToStartTimes[enc_] := With[{et = Transpose[enc]}, Transpose@Join[{Accumulate[et[[1]]]}, et[[2 ;;]], 1]];
startTimesToEncoded[raw_] := Module[{sorted, starts, durations, vols, pitches, delays},
  sorted = SortBy[raw, First];
  If[Length[sorted] == 0, Return[{}]];
  starts = sorted[[All, 1]]; durations = sorted[[All, 2]]; vols = sorted[[All, 3]]; pitches = sorted[[All, 4]];
  delays = Join[{starts[[1]]}, Differences[starts]];
  Transpose[{delays, durations, vols, pitches}]
];
sanitizeRawRows[rows_] := Cases[rows, r_List /; Length[r] >= 4 && And @@ (NumericQ /@ r[[1 ;; 4]]), Infinity];

splitMelodyHarmonyByOnset[rawRows_, tol_:0.03] := Module[{sorted, groups = {}, current, melody = {}, harmony = {}, top},
  sorted = SortBy[rawRows, First];
  If[Length[sorted] == 0, Return[{{}, {}}]];
  current = {First[sorted]};
  Do[
    If[Abs[row[[1]] - current[[-1, 1]]] <= tol, AppendTo[current, row], AppendTo[groups, current]; current = {row}],
    {row, Rest[sorted]}
  ];
  AppendTo[groups, current];
  Do[top = Last@SortBy[g, #[[4]] &]; AppendTo[melody, top]; harmony = Join[harmony, DeleteCases[g, top]], {g, groups}];
  {SortBy[melody, First], SortBy[harmony, First]}
];

noteNames = <|0 -> "C", 1 -> "C#", 2 -> "D", 3 -> "Eb", 4 -> "E", 5 -> "F", 6 -> "F#", 7 -> "G", 8 -> "Ab", 9 -> "A", 10 -> "Bb", 11 -> "B"|>;
majorScaleSteps = {0, 2, 4, 5, 7, 9, 11};
minorScaleSteps = {0, 2, 3, 5, 7, 8, 10};
noteNameForPc[pc_] := Lookup[noteNames, Mod[pc, 12], "C"];
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

chordTemplates = {
  <|"Quality" -> "maj", "PCs" -> {0, 4, 7}|>, <|"Quality" -> "min", "PCs" -> {0, 3, 7}|>,
  <|"Quality" -> "dim", "PCs" -> {0, 3, 6}|>, <|"Quality" -> "7", "PCs" -> {0, 4, 7, 10}|>
};
buildRootedCandidates[] := Flatten@Table[
  <|"RootPC" -> root, "PCs" -> Mod[root + tpl["PCs"], 12], "Label" -> (noteNameForPc[root] <> tpl["Quality"])|>,
  {tpl, chordTemplates}, {root, 0, 11}
];

overlapWeight[note_, winStart_, winEnd_] := Module[{a, b, overlap, vol},
  a = note[[1]]; b = note[[1]] + note[[2]]; overlap = Max[0, Min[b, winEnd] - Max[a, winStart]]; vol = note[[3]];
  overlap*(0.7 + 0.3*vol)
];

estimateBeatSeconds[rawNotes_] := Module[{starts, deltas, positive},
  starts = Sort[rawNotes[[All, 1]]]; deltas = Differences[starts]; positive = Select[deltas, # > 0.05 &];
  If[Length[positive] == 0, 0.5, Median[positive]]
];
barWindows[startT_, endT_, barSeconds_] := Module[{barStarts},
  barStarts = Range[0, endT, barSeconds];
  Select[Map[<|"Start" -> #, "End" -> Min[# + barSeconds, endT]|> &, barStarts], # ["End"] > startT &]
];
scoreChordCandidate[weightedNotes_, cand_] := Module[{weights, pcs, chordPcs, inW, outW},
  weights = weightedNotes[[All, 5]]; pcs = Mod[weightedNotes[[All, 4]], 12]; chordPcs = cand["PCs"];
  inW = Total@Pick[weights, MemberQ[chordPcs, #] & /@ pcs, True];
  outW = Total@Pick[weights, MemberQ[chordPcs, #] & /@ pcs, False];
  inW - 0.75 outW
];
bestChordForWindow[rawNotes_, win_, candidates_] := Module[{notesIn, weighted, scored, best},
  notesIn = Select[rawNotes, (#[[1]] < win["End"]) && (#[[1]] + #[[2]] > win["Start"]) &];
  weighted = Map[Append[#, overlapWeight[#, win["Start"], win["End"]]] &, notesIn];
  If[Length[weighted] == 0, Return[<|"Start" -> win["Start"], "End" -> win["End"], "Chord" -> "N.C.", "PCs" -> {}|>]];
  scored = Map[Append[#, "Score" -> scoreChordCandidate[weighted, #]] &, candidates];
  best = First@MaximalBy[scored, # ["Score"] &];
  <|"Start" -> win["Start"], "End" -> win["End"], "Chord" -> best["Label"], "PCs" -> best["PCs"]|>
];
activeChordAtTime[t_, timeline_] := Module[{m}, m = SelectFirst[timeline, # ["Start"] <= t < # ["End"] &]; If[m === Missing["NotFound"], Last[timeline], m]];

normalizePredNoteData[predDataRaw_, origData_] := Module[{asList = If[Head[predDataRaw] === NumericArray, Normal[predDataRaw], predDataRaw]},
  If[ListQ[asList] && Length[asList] == 3 && And @@ (NumericQ /@ asList), N[asList], N[origData]]
];

candidatePitchesForPcs[centerPitch_, allowedPcs_] := Module[{oct, choices},
  If[Length[allowedPcs] == 0, Return[{Round[centerPitch]}]];
  oct = Quotient[Round[centerPitch], 12];
  choices = Flatten@Table[12 o + pc, {o, oct - 2, oct + 2}, {pc, allowedPcs}];
  DeleteDuplicates[Sort[choices]]
];
chooseConstrainedPitch[targetPitch_, allowedPcs_, anchorPitch_, maxDelta_] := Module[{pool},
  pool = Select[candidatePitchesForPcs[anchorPitch, allowedPcs], Abs[# - anchorPitch] <= Max[2, maxDelta + 1] &];
  If[Length[pool] == 0, Return[Round[anchorPitch]]];
  First@SortBy[pool, {Abs[# - targetPitch] &, Abs[# - anchorPitch] &} &]
];

validDataset = Select[dataset, AssociationQ[#] && KeyExistsQ[#, "File"] && KeyExistsQ[#, "EncodedNotesV1"] && !FailureQ[# ["EncodedNotesV1"]] &];
matches = Select[validDataset, StringContainsQ[ToLowerCase[FileNameTake[# ["File"]]], ToLowerCase[pieceQuery]] &];
If[Length[matches] == 0, Print["No piece matched query: ", pieceQuery]; Exit[1]];
selected = First[matches];

pieceRaw = sanitizeRawRows[encodedToStartTimes[selected["EncodedNotesV1"]]];
windowAbs = Select[pieceRaw, startSec <= #[[1]] <= endSec &];
If[Length[windowAbs] < 10, Print["Timestamp range has too few notes."]; Exit[1]];

windowOffset = Min[windowAbs[[All, 1]]];
windowRaw = SortBy[windowAbs /. {s_, d_, v_, p_} :> {s - windowOffset, d, v, p}, First];
windowDuration = Max[windowRaw[[All, 1]] + windowRaw[[All, 2]]];

{melodyRaw, harmonyRaw} = splitMelodyHarmonyByOnset[windowRaw, 0.03];
If[Length[melodyRaw] < 8, melodyRaw = windowRaw; harmonyRaw = {}];

beatSeconds = estimateBeatSeconds[windowRaw];
barSeconds = Max[0.4, 4*beatSeconds];
bars = barWindows[0, windowDuration, barSeconds];
If[Length[bars] < 4, Print["Need at least 4 bars in selected window. Choose a longer range."]; Exit[1]];

callSpan = <|"Start" -> bars[[1, "Start"]], "End" -> bars[[2, "End"]]|>;
responseSpan = <|"Start" -> bars[[3, "Start"]], "End" -> bars[[4, "End"]]|>;

pcs = Mod[windowRaw[[All, 4]], 12];
weights = windowRaw[[All, 2]]*(0.7 + 0.3*windowRaw[[All, 3]]);
globalKey = detectKeyFromPcs[pcs, weights];
candidates = buildRootedCandidates[];
timeline = Map[Join[#, <|"Key" -> globalKey["Name"]|>] &, Map[bestChordForWindow[windowRaw, #, candidates] &, bars]];

callMelody = Select[melodyRaw, callSpan["Start"] <= #[[1]] < callSpan["End"] &];
responseKeep = Select[melodyRaw, #[[1]] < responseSpan["Start"] || #[[1]] >= responseSpan["End"] &];
If[Length[callMelody] < 4, Print["Bars 1-2 do not contain enough melody notes."]; Exit[1]];

timeShift = responseSpan["Start"] - callSpan["Start"];
callEncoded = startTimesToEncoded[callMelody];
seedNotes = callEncoded[[All, 4]];
seedData = N[callEncoded[[All, 1 ;; 3]]];

rewriteNotes = Take[seedNotes, UpTo[4]];
rewriteData = Take[seedData, UpTo[4]];
responseRaw = {};

Do[
  anchor = callMelody[[i]];
  anchorPitch = anchor[[4]] + responseKeyShift;
  anchorData = {anchor[[1]] - If[i == 1, callMelody[[1, 1]], callMelody[[i - 1, 1]]], anchor[[2]], anchor[[3]]};
  predResult = Quiet@Check[predictor[<|"Notes" -> rewriteNotes, "NoteData" -> NumericArray[rewriteData, "Real32"]|>], $Failed];
  predPitch = If[predResult === $Failed || FailureQ[predResult] || !NumericQ[predResult["NotesPred"]], anchorPitch, predResult["NotesPred"]];
  predData = If[predResult === $Failed || FailureQ[predResult], anchorData, normalizePredNoteData[predResult["NoteDataPred"], anchorData]];

  respStart = anchor[[1]] + timeShift;
  chordCtx = activeChordAtTime[respStart, timeline];
  allowedPcs = DeleteDuplicates@Join[chordCtx["PCs"], globalKey["ScalePCs"]];

  newPitch = chooseConstrainedPitch[predPitch + responseKeyShift, allowedPcs, anchorPitch, maxPitchDelta];
  newDur = Max[0.05, (1 - rhythmBlend) anchor[[2]] + rhythmBlend predData[[2]]];
  newVol = Clip[(1 - 0.5 rhythmBlend) anchor[[3]] + 0.5 rhythmBlend predData[[3]], {0.2, 1.0}];

  If[responseSpan["Start"] <= respStart < responseSpan["End"],
    AppendTo[responseRaw, {respStart, newDur, newVol, Round[newPitch]}];
    AppendTo[rewriteNotes, Round[newPitch]];
    AppendTo[rewriteData, {Max[0.001, anchorData[[1]]], newDur, newVol}];
  ];
,{i, 1, Length[callMelody]}];

finalMelody = SortBy[Join[responseKeep, responseRaw], First];
combinedRaw = SortBy[Join[finalMelody, harmonyRaw], First];
combinedEncoded = startTimesToEncoded[combinedRaw];
Export[outputMidi, Sound[HowlDecodeNotesV1[combinedEncoded]], "MIDI"];

analysis = <|
  "SelectedPiece" -> selected["File"],
  "WindowSeconds" -> <|"Start" -> startSec, "End" -> endSec|>,
  "GlobalKey" -> globalKey["Name"],
  "CallBars" -> callSpan,
  "ResponseBars" -> responseSpan,
  "EstimatedBeatSeconds" -> beatSeconds,
  "EstimatedBarSeconds" -> barSeconds,
  "BarTimeline" -> timeline,
  "ResponseSettings" -> <|"MaxPitchDelta" -> maxPitchDelta, "ResponseKeyShift" -> responseKeyShift, "RhythmBlend" -> rhythmBlend|>,
  "MelodyNotesInCall" -> Length[callMelody],
  "GeneratedResponseNotes" -> Length[responseRaw],
  "OutputTotalNotes" -> Length[combinedRaw]
|>;
Export[analysisJson, analysis /. {Missing[__] -> "None"}, "JSON"];

Print["Selected piece: ", selected["File"]];
Print["Saved call-response MIDI: ", outputMidi];
Print["Saved analysis JSON: ", analysisJson];

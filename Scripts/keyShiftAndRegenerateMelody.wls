#!/usr/bin/env wolframscript

(*
  Usage:
  wolframscript -file Scripts/keyShiftAndRegenerateMelody.wls \
    <predictorFile> <datasetFile> <pieceQuery> <startSec> <endSec> <targetKey> [outputMidi] [analysisJson] [maxPitchDelta] [timingBlend]

  Example targetKey strings: "D major", "F# minor", "Bb major"
*)

repoRoot = DirectoryName[DirectoryName[ExpandFileName[First[$ScriptCommandLine]]]];
SetDirectory[repoRoot];

If[Length[$ScriptCommandLine] < 7,
  Print["Usage: wolframscript -file Scripts/keyShiftAndRegenerateMelody.wls <predictorFile> <datasetFile> <pieceQuery> <startSec> <endSec> <targetKey> [outputMidi] [analysisJson] [maxPitchDelta] [timingBlend]"];
  Exit[1];
];

predictorPath = $ScriptCommandLine[[2]];
datasetPath = $ScriptCommandLine[[3]];
pieceQuery = $ScriptCommandLine[[4]];
startSec = Interpreter["Number"][$ScriptCommandLine[[5]]];
endSec = Interpreter["Number"][$ScriptCommandLine[[6]]];
targetKeySpec = $ScriptCommandLine[[7]];
outputMidi = If[Length[$ScriptCommandLine] >= 8, $ScriptCommandLine[[8]], "Scripts/keyshift_regenerated.mid"];
analysisJson = If[Length[$ScriptCommandLine] >= 9, $ScriptCommandLine[[9]], "Scripts/keyshift_regenerated_analysis.json"];
maxPitchDelta = If[Length[$ScriptCommandLine] >= 10, Interpreter["Integer"][$ScriptCommandLine[[10]]], 3];
timingBlend = If[Length[$ScriptCommandLine] >= 11, Interpreter["Number"][$ScriptCommandLine[[11]]], 0.10];

If[FailureQ[startSec] || FailureQ[endSec] || endSec <= startSec, Print["Invalid timestamp range."]; Exit[1]];
If[FailureQ[maxPitchDelta] || maxPitchDelta < 0, Print["maxPitchDelta must be non-negative integer."]; Exit[1]];
If[FailureQ[timingBlend] || timingBlend < 0 || timingBlend > 1, Print["timingBlend must be in [0,1]."]; Exit[1]];

Get["Howl/HowlMidiTools.wl"];

dataset = Import[datasetPath];
If[FailureQ[dataset], Print["Failed to import dataset: ", datasetPath]; Exit[1]];
predictor = Import[predictorPath];
If[FailureQ[predictor], Print["Failed to import predictor: ", predictorPath]; Exit[1]];

encodedToStartTimes[enc_] := With[{et = Transpose[enc]}, Transpose@Join[{Accumulate[et[[1]]]}, et[[2 ;;]], 1]];
startTimesToEncoded[raw_] := Module[{sorted, starts, durations, vols, pitches, delays},
  sorted = SortBy[raw, First];
  starts = sorted[[All, 1]]; durations = sorted[[All, 2]]; vols = sorted[[All, 3]]; pitches = sorted[[All, 4]];
  delays = If[Length[starts] == 0, {}, Join[{starts[[1]]}, Differences[starts]]];
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
nameToPc = <|"C" -> 0, "C#" -> 1, "DB" -> 1, "D" -> 2, "D#" -> 3, "EB" -> 3, "E" -> 4, "F" -> 5, "F#" -> 6, "GB" -> 6, "G" -> 7, "G#" -> 8, "AB" -> 8, "A" -> 9, "A#" -> 10, "BB" -> 10, "B" -> 11|>;
majorScaleSteps = {0, 2, 4, 5, 7, 9, 11};
minorScaleSteps = {0, 2, 3, 5, 7, 8, 10};

noteNameForPc[pc_] := Lookup[noteNames, Mod[pc, 12], "C"];
buildScalePCs[tonicPc_, scaleType_] := Mod[tonicPc + If[scaleType == "minor", minorScaleSteps, majorScaleSteps], 12];

parseKeySpec[spec_] := Module[{parts, root, mode, pc},
  parts = StringSplit[StringTrim[spec]];
  If[Length[parts] != 2, Return[$Failed]];
  root = ToUpperCase[parts[[1]]];
  mode = ToLowerCase[parts[[2]]];
  If[!MemberQ[{"major", "minor"}, mode], Return[$Failed]];
  pc = Lookup[nameToPc, root, Missing["NotFound"]];
  If[MissingQ[pc], Return[$Failed]];
  <|"TonicPC" -> pc, "ScaleType" -> mode, "Name" -> (noteNameForPc[pc] <> " " <> mode), "ScalePCs" -> buildScalePCs[pc, mode]|>
];

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
  <|"Quality" -> "dim", "PCs" -> {0, 3, 6}|>, <|"Quality" -> "7", "PCs" -> {0, 4, 7, 10}|>,
  <|"Quality" -> "maj7", "PCs" -> {0, 4, 7, 11}|>, <|"Quality" -> "min7", "PCs" -> {0, 3, 7, 10}|>
};
buildRootedCandidates[] := Flatten@Table[
  <|"RootPC" -> root, "Quality" -> tpl["Quality"], "PCs" -> Mod[root + tpl["PCs"], 12], "Label" -> (noteNameForPc[root] <> tpl["Quality"])|>,
  {tpl, chordTemplates}, {root, 0, 11}
];

estimateBeatSeconds[rawNotes_] := Module[{starts, deltas, positive}, starts = Sort[rawNotes[[All, 1]]]; deltas = Differences[starts]; positive = Select[deltas, # > 0.05 &]; If[Length[positive] == 0, 0.5, Median[positive]]];
estimateBeatsPerBar[file_] := Module[{sigCandidates, firstSig},
  sigCandidates = DeleteCases[Quiet@Check[{Import[file, {"MIDI", "TimeSignatures"}], Import[file, {"MIDI", "TimeSignature"}]}, {}], _Failure | $Failed | Missing[__] | {}];
  If[Length[sigCandidates] == 0, Return[4]];
  firstSig = First@Flatten@sigCandidates;
  Which[MatchQ[firstSig, {_Integer, _Integer}], firstSig[[1]], MatchQ[firstSig, {_?NumericQ, _?NumericQ}], Round[firstSig[[1]]], AssociationQ[firstSig] && KeyExistsQ[firstSig, "Numerator"], firstSig["Numerator"], True, 4]
];
barWindows[startT_, endT_, barSeconds_] := Module[{barStarts}, barStarts = Range[Floor[startT/barSeconds]*barSeconds, endT, barSeconds]; Select[Map[<|"Start" -> #, "End" -> Min[# + barSeconds, endT]|> &, barStarts], #["End"] > startT &]];

overlapWeight[note_, winStart_, winEnd_] := Module[{a, b, overlap, vol}, a = note[[1]]; b = note[[1]] + note[[2]]; overlap = Max[0, Min[b, winEnd] - Max[a, winStart]]; vol = note[[3]]; overlap*(0.7 + 0.3*vol)];
scoreChordCandidate[weightedNotes_, cand_] := Module[{weights, pcs, chordPcs, inW, outW, bassPc, bassBonus},
  If[Length[weightedNotes] == 0, Return[-Infinity]];
  weights = weightedNotes[[All, 5]]; pcs = Mod[weightedNotes[[All, 4]], 12]; chordPcs = cand["PCs"];
  inW = Total@Pick[weights, MemberQ[chordPcs, #] & /@ pcs, True];
  outW = Total@Pick[weights, MemberQ[chordPcs, #] & /@ pcs, False];
  bassPc = Mod[(First@SortBy[weightedNotes, #[[4]] &])[[4]], 12]; bassBonus = If[bassPc == cand["RootPC"], 0.4, 0.0];
  inW - 0.75 outW + bassBonus
];
bestChordForWindow[rawNotes_, win_, candidates_] := Module[{notesIn, weighted, scored, best},
  notesIn = Select[rawNotes, (#[[1]] < win["End"]) && (#[[1]] + #[[2]] > win["Start"]) &];
  weighted = Map[Append[#, overlapWeight[#, win["Start"], win["End"]]] &, notesIn];
  If[Length[weighted] == 0, Return[<|"Start" -> win["Start"], "End" -> win["End"], "Chord" -> "N.C.", "PCs" -> {}, "RootPC" -> Missing["None"], "Quality" -> "", "Score" -> -Infinity|>]];
  scored = Map[Append[#, "Score" -> scoreChordCandidate[weighted, #]] &, candidates];
  best = First@MaximalBy[scored, # ["Score"] &];
  <|"Start" -> win["Start"], "End" -> win["End"], "Chord" -> best["Label"], "PCs" -> best["PCs"], "RootPC" -> best["RootPC"], "Quality" -> best["Quality"], "Score" -> best["Score"]|>
];

activeChordAtTime[t_, timeline_] := Module[{m}, If[Length[timeline] == 0, Return[<|"PCs" -> {}, "RootPC" -> Missing["None"], "Chord" -> "N.C."|>]]; m = SelectFirst[timeline, #["Start"] <= t < #["End"] &]; If[m === Missing["NotFound"], Last[timeline], m]];
nearestAllowedPitch[candidatePitch_, anchorPitch_, allowedPcs_] := Module[{oct, choices, sorted}, If[Length[allowedPcs] == 0, Return[Round[candidatePitch]]]; oct = Quotient[Round[candidatePitch], 12]; choices = Flatten@Table[12 o + pc, {o, oct - 2, oct + 2}, {pc, allowedPcs}]; sorted = SortBy[choices, {Abs[# - candidatePitch] &, Abs[# - anchorPitch] &} &]; First[sorted]];
candidatePitchesForPcs[centerPitch_, allowedPcs_] := Module[{oct, choices}, If[Length[allowedPcs] == 0, Return[{Round[centerPitch]}]]; oct = Quotient[Round[centerPitch], 12]; choices = Flatten@Table[12 o + pc, {o, oct - 2, oct + 2}, {pc, allowedPcs}]; DeleteDuplicates[Sort[choices]]];

chooseMusicalPitch[origPitch_, predPitch_, prevPitch_, allowedPcs_, maxDelta_] := Module[{base, constrained, pool, leapPenalty, scored},
  base = Round[origPitch] + Clip[Round[predPitch] - Round[origPitch], {-maxDelta, maxDelta}];
  constrained = nearestAllowedPitch[base, origPitch, allowedPcs];
  pool = DeleteDuplicates@Join[{constrained, Round[origPitch]}, Select[candidatePitchesForPcs[origPitch, allowedPcs], Abs[# - origPitch] <= Max[3, maxDelta + 2] &]];
  scored = SortBy[pool, Function[p, leapPenalty = If[NumericQ[prevPitch], 0.2*Max[0, Abs[p - prevPitch] - 5], 0]; {0.8*Abs[p - constrained] + 0.4*Abs[p - Round[origPitch]] + leapPenalty, Abs[p - Round[predPitch]]}]];
  Clip[Round[First[scored]], {-60, 59}]
];

normalizePredPitch[predPitchRaw_, origPitch_] := If[NumericQ[predPitchRaw], Round[predPitchRaw], Round[origPitch]];
normalizePredNoteData[predDataRaw_, origData_] := Module[{candidate = predDataRaw, asList}, asList = Which[Head[candidate] === NumericArray, Normal[candidate], ListQ[candidate], candidate, True, origData]; If[ListQ[asList] && Length[asList] == 3 && And @@ (NumericQ /@ asList), N[asList], N[origData]]];
transposeRawNotes[raw_, semitones_] := raw /. {s_, d_, v_, p_} :> {s, d, v, Clip[Round[p + semitones], {-60, 59}]};

validDataset = Select[dataset, AssociationQ[#] && KeyExistsQ[#, "File"] && KeyExistsQ[#, "EncodedNotesV1"] && !FailureQ[# ["EncodedNotesV1"]] &];
matches = Select[validDataset, StringContainsQ[ToLowerCase[FileNameTake[# ["File"]]], ToLowerCase[pieceQuery]] &];
If[Length[matches] == 0, Print["No piece matched query: ", pieceQuery]; Exit[1]];
selected = First[matches];
If[Length[matches] > 1, Print["Multiple matches found; using first: ", selected["File"]]];
Print["Selected piece: ", selected["File"]];

pieceRaw = sanitizeRawRows[encodedToStartTimes[selected["EncodedNotesV1"]]];
windowAbs = Select[pieceRaw, startSec <= #[[1]] <= endSec &];
If[Length[windowAbs] < 8, Print["Timestamp range has too few notes."]; Exit[1]];
windowOffset = Min[windowAbs[[All, 1]]];
windowRaw = SortBy[windowAbs /. {s_, d_, v_, p_} :> {s - windowOffset, d, v, p}, First];
windowDuration = Max[windowRaw[[All, 1]] + windowRaw[[All, 2]]];

{melodyRaw, harmonyRaw} = splitMelodyHarmonyByOnset[windowRaw, 0.03];
If[Length[melodyRaw] < 8, melodyRaw = windowRaw; harmonyRaw = {}];

pcs = Mod[windowRaw[[All, 4]], 12];
weights = windowRaw[[All, 2]]*(0.7 + 0.3*windowRaw[[All, 3]]);
origKey = detectKeyFromPcs[pcs, weights];
targetKey = parseKeySpec[targetKeySpec];
If[targetKey === $Failed, Print["Could not parse targetKey. Use format like 'D major' or 'Bb minor'."]; Exit[1]];

semiShift = Mod[targetKey["TonicPC"] - origKey["TonicPC"] + 6, 12] - 6;
Print["Detected key: ", origKey["Name"], " | Target key: ", targetKey["Name"], " | Semitone shift: ", semiShift];

melodyShiftedRaw = transposeRawNotes[melodyRaw, semiShift];
harmonyShiftedRaw = transposeRawNotes[harmonyRaw, semiShift];
allShiftedRaw = SortBy[Join[melodyShiftedRaw, harmonyShiftedRaw], First];

beatSeconds = estimateBeatSeconds[allShiftedRaw];
beatsPerBar = estimateBeatsPerBar[selected["File"]];
barSeconds = Max[0.4, beatSeconds*beatsPerBar];
bars = barWindows[0, windowDuration, barSeconds];

candidates = buildRootedCandidates[];
rawTimeline = Map[bestChordForWindow[allShiftedRaw, #, candidates] &, bars];
timeline = Map[Join[#, <|"Key" -> targetKey["Name"]|>] &, rawTimeline];

melodyEncoded = startTimesToEncoded[melodyShiftedRaw];
origNotes = melodyEncoded[[All, 4]];
origData = N[melodyEncoded[[All, 1 ;; 3]]];

rewriteNotes = Take[origNotes, UpTo[4]];
rewriteData = Take[origData, UpTo[4]];

Do[
  thisOrigPitch = origNotes[[i]];
  thisOrigData = origData[[i]];
  predInput = <|"Notes" -> rewriteNotes, "NoteData" -> NumericArray[rewriteData, "Real32"]|>;
  predResult = Quiet@Check[predictor[predInput], $Failed];
  predPitchRaw = If[predResult === $Failed || FailureQ[predResult], thisOrigPitch, predResult["NotesPred"]];
  predDataRaw = If[predResult === $Failed || FailureQ[predResult], thisOrigData, predResult["NoteDataPred"]];
  predPitch = normalizePredPitch[predPitchRaw, thisOrigPitch];
  predData = normalizePredNoteData[predDataRaw, thisOrigData];

  absStart = Total[rewriteData[[All, 1]]] + thisOrigData[[1]];
  barCtx = activeChordAtTime[absStart, timeline];
  allowedPcs = DeleteDuplicates@Join[If[KeyExistsQ[barCtx, "PCs"], barCtx["PCs"], {}], targetKey["ScalePCs"]];
  prevNewPitch = If[Length[rewriteNotes] > 0, Last[rewriteNotes], Missing["None"]];

  newPitch = chooseMusicalPitch[thisOrigPitch, predPitch, prevNewPitch, allowedPcs, maxPitchDelta];
  mixed = (1 - timingBlend) thisOrigData + timingBlend predData;
  newData = N[{Max[0.001, thisOrigData[[1]]], Max[0.05, 0.85*thisOrigData[[2]] + 0.15*mixed[[2]]], Clip[0.8*thisOrigData[[3]] + 0.2*mixed[[3]], {0.2, 1.0}]}];

  AppendTo[rewriteNotes, Round[newPitch]];
  AppendTo[rewriteData, newData];
,
{i, Length[rewriteNotes] + 1, Length[origNotes]}
];

newMelodyEncoded = Transpose@{rewriteData[[All, 1]], rewriteData[[All, 2]], rewriteData[[All, 3]], rewriteNotes};
newMelodyRaw = sanitizeRawRows[encodedToStartTimes[newMelodyEncoded]];
combinedRaw = SortBy[Join[newMelodyRaw, harmonyShiftedRaw], First];
combinedEncoded = startTimesToEncoded[combinedRaw];

Export[outputMidi, Sound[HowlDecodeNotesV1[combinedEncoded]], "MIDI"];

analysis = <|
  "SelectedPiece" -> selected["File"],
  "WindowSeconds" -> <|"Start" -> startSec, "End" -> endSec|>,
  "DetectedOriginalKey" -> origKey["Name"],
  "TargetKey" -> targetKey["Name"],
  "AppliedSemitoneShift" -> semiShift,
  "EstimatedBeatSeconds" -> beatSeconds,
  "EstimatedBeatsPerBar" -> beatsPerBar,
  "EstimatedBarSeconds" -> barSeconds,
  "BarTimeline" -> timeline,
  "OriginalMelodyNoteCount" -> Length[melodyRaw],
  "OriginalHarmonyNoteCount" -> Length[harmonyRaw],
  "RegeneratedMelodyNoteCount" -> Length[newMelodyRaw],
  "OutputTotalNoteCount" -> Length[combinedRaw],
  "Settings" -> <|"MaxPitchDelta" -> maxPitchDelta, "TimingBlend" -> timingBlend|>
|>;

safeAnalysis = analysis /. {Missing[__] -> "None", DirectedInfinity[_] -> Null, Infinity -> Null, -Infinity -> Null};
Export[analysisJson, safeAnalysis, "JSON"];

Print["Saved key-shift regenerated MIDI: ", outputMidi];
Print["Saved analysis JSON: ", analysisJson];

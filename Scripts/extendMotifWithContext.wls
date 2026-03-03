#!/usr/bin/env wolframscript

(*
  Usage:
  wolframscript -file Scripts/extendMotifWithContext.wls \
    <predictorFile> <motifMidi> <motifStartSec> <motifEndSec> <extensionSec> [analysisMidi] [outputMidi] [analysisJson] [maxPitchDelta] [timingBlend] [keepMotifRhythm]

  Extracts a motif from a MIDI file, detects key/chord context (optionally from a full-piece MIDI),
  and appends a tightly constrained extension using predictor hints.
*)

repoRoot = DirectoryName[DirectoryName[ExpandFileName[First[$ScriptCommandLine]]]];
SetDirectory[repoRoot];

If[Length[$ScriptCommandLine] < 6,
  Print["Usage: wolframscript -file Scripts/extendMotifWithContext.wls <predictorFile> <motifMidi> <motifStartSec> <motifEndSec> <extensionSec> [analysisMidi] [outputMidi] [analysisJson] [maxPitchDelta] [timingBlend] [keepMotifRhythm]"];
  Exit[1];
];

predictorPath = $ScriptCommandLine[[2]];
motifMidi = $ScriptCommandLine[[3]];
motifStart = Interpreter["Number"][$ScriptCommandLine[[4]]];
motifEnd = Interpreter["Number"][$ScriptCommandLine[[5]]];
extensionSec = Interpreter["Number"][$ScriptCommandLine[[6]]];
analysisMidi = If[Length[$ScriptCommandLine] >= 7, $ScriptCommandLine[[7]], motifMidi];
outputMidi = If[Length[$ScriptCommandLine] >= 8, $ScriptCommandLine[[8]], "Scripts/motif_extended.mid"];
analysisJson = If[Length[$ScriptCommandLine] >= 9, $ScriptCommandLine[[9]], "Scripts/motif_extended_analysis.json"];
maxPitchDelta = If[Length[$ScriptCommandLine] >= 10, Interpreter["Integer"][$ScriptCommandLine[[10]]], 2];
timingBlend = If[Length[$ScriptCommandLine] >= 11, Interpreter["Number"][$ScriptCommandLine[[11]]], 0.08];
keepMotifRhythm = If[Length[$ScriptCommandLine] >= 12, Interpreter["Number"][$ScriptCommandLine[[12]]], 0.90];

If[FailureQ[motifStart] || FailureQ[motifEnd] || motifEnd <= motifStart, Print["Invalid motif window."]; Exit[1]];
If[FailureQ[extensionSec] || extensionSec <= 0, Print["extensionSec must be > 0."]; Exit[1]];
If[FailureQ[maxPitchDelta] || maxPitchDelta < 0, Print["maxPitchDelta must be non-negative integer."]; Exit[1]];
If[FailureQ[timingBlend] || timingBlend < 0 || timingBlend > 1, Print["timingBlend must be in [0,1]."]; Exit[1]];
If[FailureQ[keepMotifRhythm] || keepMotifRhythm < 0 || keepMotifRhythm > 1, Print["keepMotifRhythm must be in [0,1]."]; Exit[1]];

normalizePathString[s_String] := StringTrim[StringReplace[s, """ -> ""]];

resolveMidiFile[input_String] := Module[{raw, dir, base, candidates},
  raw = normalizePathString[input];
  If[FileExistsQ[raw], Return[raw]];
  If[FileExistsQ[raw <> ".mid"], Return[raw <> ".mid"]];
  If[FileExistsQ[raw <> ".midi"], Return[raw <> ".midi"]];

  dir = DirectoryName[raw];
  base = FileNameTake[raw];
  If[!DirectoryQ[dir], dir = "."];

  candidates = DeleteDuplicates@Join[
    FileNames[base <> ".mid", dir, Infinity, IgnoreCase -> True],
    FileNames[base <> ".midi", dir, Infinity, IgnoreCase -> True],
    Select[FileNames["*.mid", dir, Infinity, IgnoreCase -> True], StringContainsQ[ToLowerCase[FileNameTake[#]], ToLowerCase[base]] &],
    Select[FileNames["*.midi", dir, Infinity, IgnoreCase -> True], StringContainsQ[ToLowerCase[FileNameTake[#]], ToLowerCase[base]] &]
  ];

  If[Length[candidates] > 0, First@SortBy[candidates, StringLength], $Failed]
];

Get["Howl/HowlMidiTools.wl"];

predictor = Import[predictorPath];
If[FailureQ[predictor], Print["Failed to import predictor: ", predictorPath]; Exit[1]];

sanitizeRawRows[rows_] := Cases[rows, r_List /; Length[r] >= 4 && And @@ (NumericQ /@ r[[1 ;; 4]]), Infinity];

encodedToStartTimes[enc_] := With[{et = Transpose[enc]}, Transpose@Join[{Accumulate[et[[1]]]}, et[[2 ;;]], 1]];
startTimesToEncoded[raw_] := Module[{sorted, starts, durations, vols, pitches, delays},
  sorted = SortBy[raw, First];
  starts = sorted[[All, 1]];
  durations = sorted[[All, 2]];
  vols = sorted[[All, 3]];
  pitches = sorted[[All, 4]];
  delays = If[Length[starts] == 0, {}, Join[{starts[[1]]}, Differences[starts]]];
  Transpose[{delays, durations, vols, pitches}]
];

splitMelodyHarmonyByOnset[rawRows_, tol_: 0.03] := Module[{sorted, groups = {}, current, melody = {}, harmony = {}, top},
  sorted = SortBy[rawRows, First];
  If[Length[sorted] == 0, Return[{{}, {}}]];
  current = {First[sorted]};
  Do[
    If[Abs[row[[1]] - current[[-1, 1]]] <= tol,
      AppendTo[current, row],
      AppendTo[groups, current];
      current = {row}
    ],
    {row, Rest[sorted]}
  ];
  AppendTo[groups, current];
  Do[
    top = Last@SortBy[g, #[[4]] &];
    AppendTo[melody, top];
    harmony = Join[harmony, DeleteCases[g, top]],
    {g, groups}
  ];
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
  inScaleW + 0.45*tonicW
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
  <|"Quality" -> "maj", "PCs" -> {0, 4, 7}|>,
  <|"Quality" -> "min", "PCs" -> {0, 3, 7}|>,
  <|"Quality" -> "dim", "PCs" -> {0, 3, 6}|>,
  <|"Quality" -> "7", "PCs" -> {0, 4, 7, 10}|>,
  <|"Quality" -> "maj7", "PCs" -> {0, 4, 7, 11}|>,
  <|"Quality" -> "min7", "PCs" -> {0, 3, 7, 10}|>
};
buildRootedCandidates[] := Flatten@Table[
  <|"RootPC" -> root, "PCs" -> Mod[root + tpl["PCs"], 12], "Label" -> (noteNameForPc[root] <> tpl["Quality"] )|>,
  {tpl, chordTemplates}, {root, 0, 11}
];

overlapWeight[note_, winStart_, winEnd_] := Module[{a, b, overlap, vol},
  a = note[[1]]; b = note[[1]] + note[[2]];
  overlap = Max[0, Min[b, winEnd] - Max[a, winStart]];
  vol = note[[3]];
  overlap*(0.7 + 0.3*vol)
];

estimateBeatSeconds[rawNotes_] := Module[{starts, deltas, positive},
  starts = Sort[rawNotes[[All, 1]]];
  deltas = Differences[starts];
  positive = Select[deltas, # > 0.05 &];
  If[Length[positive] == 0, 0.5, Median[positive]]
];

estimateBeatsPerBar[file_] := Module[{sigCandidates, firstSig},
  sigCandidates = DeleteCases[Quiet@Check[{Import[file, {"MIDI", "TimeSignatures"}], Import[file, {"MIDI", "TimeSignature"}]}, {}], _Failure | $Failed | Missing[__] | {}];
  If[Length[sigCandidates] == 0, Return[4]];
  firstSig = First@Flatten@sigCandidates;
  Which[
    MatchQ[firstSig, {_Integer, _Integer}], firstSig[[1]],
    MatchQ[firstSig, {_?NumericQ, _?NumericQ}], Round[firstSig[[1]]],
    AssociationQ[firstSig] && KeyExistsQ[firstSig, "Numerator"], firstSig["Numerator"],
    True, 4
  ]
];

barWindows[startT_, endT_, barSeconds_] := Module[{barStarts},
  barStarts = Range[Floor[startT/barSeconds]*barSeconds, endT, barSeconds];
  Select[Map[<|"Start" -> #, "End" -> Min[# + barSeconds, endT]|> &, barStarts], #["End"] > startT &]
];

scoreChordCandidate[weightedNotes_, cand_] := Module[{weights, pcs, chordPcs, inW, outW, bassPc, bassBonus},
  If[Length[weightedNotes] == 0, Return[-Infinity]];
  weights = weightedNotes[[All, 5]];
  pcs = Mod[weightedNotes[[All, 4]], 12];
  chordPcs = cand["PCs"];
  inW = Total@Pick[weights, MemberQ[chordPcs, #] & /@ pcs, True];
  outW = Total@Pick[weights, MemberQ[chordPcs, #] & /@ pcs, False];
  bassPc = Mod[(First@SortBy[weightedNotes, #[[4]] &])[[4]], 12];
  bassBonus = If[bassPc == cand["RootPC"], 0.4, 0.0];
  inW - 0.75*outW + bassBonus
];

bestChordForWindow[rawNotes_, win_, candidates_] := Module[{notesIn, weighted, scored, best},
  notesIn = Select[rawNotes, (#[[1]] < win["End"]) && (#[[1]] + #[[2]] > win["Start"]) &];
  weighted = Map[Append[#, overlapWeight[#, win["Start"], win["End"]]] &, notesIn];
  If[Length[weighted] == 0, Return[<|"Start" -> win["Start"], "End" -> win["End"], "Chord" -> "N.C.", "PCs" -> {}, "RootPC" -> Missing["None"]|>]];
  scored = Map[Append[#, "Score" -> scoreChordCandidate[weighted, #]] &, candidates];
  best = First@MaximalBy[scored, # ["Score"] &];
  <|"Start" -> win["Start"], "End" -> win["End"], "Chord" -> best["Label"], "PCs" -> best["PCs"], "RootPC" -> best["RootPC"]|>
];

activeChordAtTime[t_, timeline_] := Module[{m},
  If[Length[timeline] == 0, Return[<|"PCs" -> {}, "RootPC" -> Missing["None"], "Chord" -> "N.C."|>]];
  m = SelectFirst[timeline, #["Start"] <= t < #["End"] &];
  If[m === Missing["NotFound"], Last[timeline], m]
];

candidatePitchesForPcs[centerPitch_, allowedPcs_] := Module[{oct, choices},
  If[Length[allowedPcs] == 0, Return[{Round[centerPitch]}]];
  oct = Quotient[Round[centerPitch], 12];
  choices = Flatten@Table[12*o + pc, {o, oct - 2, oct + 2}, {pc, allowedPcs}];
  DeleteDuplicates[Sort[choices]]
];

chooseConstrainedPitch[origPitch_, predPitch_, prevPitch_, allowedPcs_, maxDelta_] := Module[{target, pool, scored},
  target = Round[origPitch] + Clip[Round[predPitch] - Round[origPitch], {-maxDelta, maxDelta}];
  pool = Select[candidatePitchesForPcs[origPitch, allowedPcs], Abs[# - Round[origPitch]] <= Max[2, maxDelta + 1] &];
  pool = DeleteDuplicates@Join[{target, Round[origPitch]}, pool];
  scored = SortBy[pool, {Abs[# - target] &, If[NumericQ[prevPitch], Abs[# - prevPitch], 0] &, Abs[# - Round[origPitch]] &} &];
  Round[First[scored]]
];

coercePitch[p_] := HowlNoteToInt[p];

canonicalizeEncoded[enc_] := Module[{clean = N[enc]},
  If[Length[clean] == 0, Return[clean]];
  clean[[All, 1]] = Max[0.001, #] & /@ clean[[All, 1]];
  clean[[All, 2]] = Max[0.05, #] & /@ clean[[All, 2]];
  clean[[All, 3]] = Clip[#, {0.1, 1.0}] & /@ clean[[All, 3]];
  clean[[All, 4]] = coercePitch /@ clean[[All, 4]];
  clean
];

encToNetInput[encSong_] := With[{clean = canonicalizeEncoded[encSong]}, <|
  "NoteData" -> NumericArray[clean[[All, 1 ;; 3]], "Real32"],
  "Notes" -> Round[clean[[All, 4]]]
|>];
normalizePredData[pred_, fallback_] := Module[{d = Lookup[pred, "NoteDataPred", fallback]},
  If[Head[d] === NumericArray, d = Normal[d]];
  If[ListQ[d] && Length[d] == 3 && And @@ (NumericQ /@ d), N[d], N[fallback]]
];
normalizePredPitch[pred_, fallback_] := Module[{p = Lookup[pred, "NotesPred", fallback]}, If[NumericQ[p], coercePitch[p], coercePitch[fallback]]];

motifMidiResolved = resolveMidiFile[motifMidi];
If[motifMidiResolved === $Failed, Print["Failed to resolve motif MIDI path: ", motifMidi]; Exit[1]];

analysisMidiResolved = If[analysisMidi === motifMidi, motifMidiResolved, resolveMidiFile[analysisMidi]];
If[analysisMidiResolved === $Failed, Print["Failed to resolve analysis MIDI path: ", analysisMidi]; Exit[1]];

motifImported = HowlMidiImport[motifMidiResolved];
If[FailureQ[motifImported["Notes"]], Print["Failed to import motif MIDI: ", motifMidiResolved]; Exit[1]];

analysisImported = If[analysisMidiResolved === motifMidiResolved, motifImported, HowlMidiImport[analysisMidiResolved]];
If[FailureQ[analysisImported["Notes"]], Print["Failed to import analysis MIDI: ", analysisMidiResolved]; Exit[1]];

motifRawAbs = sanitizeRawRows[encodedToStartTimes[motifImported["EncodedNotesV1"]]];
motifAbsWindow = Select[motifRawAbs, motifStart <= #[[1]] <= motifEnd &];
If[Length[motifAbsWindow] < 8, Print["Motif window has too few notes."]; Exit[1]];

motifOffset = Min[motifAbsWindow[[All, 1]]];
motifRaw = SortBy[motifAbsWindow /. {s_, d_, v_, p_} :> {s - motifOffset, d, v, p}, First];
{motifMelodyRaw, motifHarmonyRaw} = splitMelodyHarmonyByOnset[motifRaw, 0.03];
If[Length[motifMelodyRaw] < 8, motifMelodyRaw = motifRaw; motifHarmonyRaw = {}];

analysisRaw = sanitizeRawRows[encodedToStartTimes[analysisImported["EncodedNotesV1"]]];
If[Length[analysisRaw] == 0, Print["Analysis MIDI has no valid notes: ", analysisMidiResolved]; Exit[1]];
analysisPcs = Mod[analysisRaw[[All, 4]], 12];
analysisWeights = analysisRaw[[All, 2]]*(0.7 + 0.3*analysisRaw[[All, 3]]);
globalKey = detectKeyFromPcs[analysisPcs, analysisWeights];

beatSeconds = estimateBeatSeconds[motifRaw];
beatsPerBar = estimateBeatsPerBar[analysisMidiResolved];
barSeconds = Max[0.4, beatSeconds*beatsPerBar];
motifDuration = Max[motifRaw[[All, 1]] + motifRaw[[All, 2]]];
extensionEnd = motifDuration + extensionSec;

bars = barWindows[0, extensionEnd, barSeconds];
candidates = buildRootedCandidates[];
rawTimeline = Map[bestChordForWindow[motifRaw, #, candidates] &, bars];
timeline = Map[If[# ["Chord"] === "N.C.", Join[#, <|"PCs" -> globalKey["ScalePCs"], "Chord" -> "Scale"|>], #] &, rawTimeline];

motifMelodyEncoded = canonicalizeEncoded[startTimesToEncoded[motifMelodyRaw]];
notes = Round[motifMelodyEncoded[[All, 4]]];
data = N[motifMelodyEncoded[[All, 1 ;; 3]]];

templateDur = motifMelodyEncoded[[All, 2]];
templateVol = motifMelodyEncoded[[All, 3]];
meanDelay = Max[0.02, Mean[motifMelodyEncoded[[All, 1]]]];

currentTime = Total[data[[All, 1]]];
i = 1;
While[currentTime < extensionEnd,
  seqEncoded = Transpose@{data[[All, 1]], data[[All, 2]], data[[All, 3]], notes};
  predInput = encToNetInput[seqEncoded[[-Min[Length[notes], 256] ;;]]];
  pred = Quiet@Check[predictor[predInput, TargetDevice -> "CPU"], <||>];

  origRef = motifMelodyEncoded[[1 + Mod[i - 1, Length[motifMelodyEncoded]]]];
  predPitch = normalizePredPitch[pred, origRef[[4]]];
  predData = normalizePredData[pred, origRef[[1 ;; 3]]];

  barCtx = activeChordAtTime[currentTime, timeline];
  allowedPcs = DeleteDuplicates@Join[barCtx["PCs"], globalKey["ScalePCs"]];
  prevPitch = If[Length[notes] > 0, Last[notes], Missing["None"]];
  newPitch = chooseConstrainedPitch[origRef[[4]], predPitch, prevPitch, allowedPcs, maxPitchDelta];

  refDur = templateDur[[1 + Mod[i - 1, Length[templateDur]]]];
  refVol = templateVol[[1 + Mod[i - 1, Length[templateVol]]]];
  newDelay = Max[0.01, (keepMotifRhythm*origRef[[1]] + (1 - keepMotifRhythm)*meanDelay)];
  mixed = (1 - timingBlend)*origRef[[1 ;; 3]] + timingBlend*predData;
  newDur = Max[0.05, 0.85*refDur + 0.15*mixed[[2]]];
  newVol = Clip[0.85*refVol + 0.15*mixed[[3]], {0.2, 1.0}];

  AppendTo[notes, coercePitch[newPitch]];
  AppendTo[data, N[{newDelay, newDur, newVol}]];

  currentTime = currentTime + newDelay;
  i++;
];

melodyExtendedEncoded = Transpose@{data[[All, 1]], data[[All, 2]], data[[All, 3]], notes};
melodyExtendedRaw = sanitizeRawRows[encodedToStartTimes[melodyExtendedEncoded]];
combinedRaw = SortBy[Join[melodyExtendedRaw, motifHarmonyRaw], First];
combinedEncoded = canonicalizeEncoded[startTimesToEncoded[combinedRaw]];

Export[outputMidi, Sound[HowlDecodeNotesV1[combinedEncoded]], "MIDI"];

analysis = <|
  "MotifMidi" -> motifMidiResolved,
  "AnalysisMidi" -> analysisMidiResolved,
  "MotifWindow" -> <|"Start" -> motifStart, "End" -> motifEnd|>,
  "DetectedKey" -> globalKey["Name"],
  "EstimatedBeatSeconds" -> beatSeconds,
  "EstimatedBeatsPerBar" -> beatsPerBar,
  "EstimatedBarSeconds" -> barSeconds,
  "Timeline" -> timeline,
  "MotifMelodyNotes" -> Length[motifMelodyRaw],
  "MotifHarmonyNotes" -> Length[motifHarmonyRaw],
  "OutputTotalNotes" -> Length[combinedRaw],
  "Settings" -> <|"ExtensionSeconds" -> extensionSec, "MaxPitchDelta" -> maxPitchDelta, "TimingBlend" -> timingBlend, "KeepMotifRhythm" -> keepMotifRhythm|>
|>;

Export[analysisJson, analysis /. {Missing[__] -> "None"}, "JSON"];
Print["Saved extended MIDI: ", outputMidi];
Print["Saved analysis JSON: ", analysisJson];

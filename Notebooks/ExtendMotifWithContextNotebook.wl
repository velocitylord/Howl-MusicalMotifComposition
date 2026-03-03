(* Notebook helper for motif extension with key/chord-aware constraints *)

Get[FileNameJoin[{DirectoryName[DirectoryName[$InputFileName]], "Howl", "HowlMidiTools.wl"}]];

ClearAll[
  HowlNormalizePathString, HowlResolveMidiFile, HowlSanitizeRawRows,
  HowlEncodedToStartTimes, HowlStartTimesToEncoded, HowlSplitMelodyHarmonyByOnset,
  HowlEstimateBeatSeconds, HowlEstimateBeatsPerBar, HowlBarWindows,
  HowlDetectKeyFromPcs, HowlBuildRootedCandidates, HowlBestChordForWindow,
  HowlActiveChordAtTime, HowlChooseConstrainedPitch,
  HowlCoercePitch, HowlCanonicalizeEncoded, HowlEncToNetInput,
  ExtendMotifWithContext
];

HowlNormalizePathString[s_String] := StringTrim[StringReplace[s, "\"" -> ""]];
HowlResolveMidiFile[input_String] := Module[{raw, dir, base, candidates},
  raw = HowlNormalizePathString[input];
  If[FileExistsQ[raw], Return[raw]];
  If[FileExistsQ[raw <> ".mid"], Return[raw <> ".mid"]];
  If[FileExistsQ[raw <> ".midi"], Return[raw <> ".midi"]];
  dir = DirectoryName[raw];
  base = FileNameTake[raw];
  If[!DirectoryQ[dir], dir = "."];
  candidates = DeleteDuplicates@Join[
    FileNames[base <> ".mid", dir, Infinity],
    FileNames[base <> ".midi", dir, Infinity],
    Select[FileNames["*.mid", dir, Infinity], StringContainsQ[ToLowerCase[FileNameTake[#]], ToLowerCase[base]] &],
    Select[FileNames["*.midi", dir, Infinity], StringContainsQ[ToLowerCase[FileNameTake[#]], ToLowerCase[base]] &]
  ];
  If[Length[candidates] > 0, First@SortBy[candidates, StringLength], $Failed]
];

HowlSanitizeRawRows[rows_] := Cases[rows, r_List /; Length[r] >= 4 && And @@ (NumericQ /@ r[[1 ;; 4]]), Infinity];
HowlEncodedToStartTimes[enc_] := With[{et = Transpose[enc]}, Transpose@Join[{Accumulate[et[[1]]]}, et[[2 ;;]], 1]];
HowlStartTimesToEncoded[raw_] := Module[{sorted, starts, durations, vols, pitches, delays},
  sorted = SortBy[raw, First];
  starts = sorted[[All, 1]]; durations = sorted[[All, 2]]; vols = sorted[[All, 3]]; pitches = sorted[[All, 4]];
  delays = If[Length[starts] == 0, {}, Join[{starts[[1]]}, Differences[starts]]];
  Transpose[{delays, durations, vols, pitches}]
];

HowlSplitMelodyHarmonyByOnset[rawRows_, tol_ : 0.03] := Module[{sorted, groups = {}, current, melody = {}, harmony = {}, top},
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
HowlDetectKeyFromPcs[pcs_, weights_] := Module[{buildScalePCs, scoreKeyFit, candidates, best},
  buildScalePCs[tonicPc_, scaleType_] := Mod[tonicPc + If[scaleType == "minor", minorScaleSteps, majorScaleSteps], 12];
  scoreKeyFit[tonicPc_, scaleType_] := Module[{scalePcs, inScaleW, tonicW},
    scalePcs = buildScalePCs[tonicPc, scaleType];
    inScaleW = Total@Pick[weights, MemberQ[scalePcs, #] & /@ pcs, True];
    tonicW = Total@Pick[weights, (# == tonicPc) & /@ pcs, True];
    inScaleW + 0.45*tonicW
  ];
  candidates = Flatten@Table[<|"TonicPC" -> tonic, "ScaleType" -> scale, "Score" -> scoreKeyFit[tonic, scale]|>, {scale, {"major", "minor"}}, {tonic, 0, 11}];
  best = First@MaximalBy[candidates, # ["Score"] &];
  Join[best, <|"Name" -> (Lookup[noteNames, best["TonicPC"], "C"] <> " " <> best["ScaleType"]), "ScalePCs" -> buildScalePCs[best["TonicPC"], best["ScaleType"]]|>]
];

HowlBuildRootedCandidates[] := Module[{chordTemplates},
  chordTemplates = {
    <|"Quality" -> "maj", "PCs" -> {0, 4, 7}|>, <|"Quality" -> "min", "PCs" -> {0, 3, 7}|>,
    <|"Quality" -> "dim", "PCs" -> {0, 3, 6}|>, <|"Quality" -> "7", "PCs" -> {0, 4, 7, 10}|>,
    <|"Quality" -> "maj7", "PCs" -> {0, 4, 7, 11}|>, <|"Quality" -> "min7", "PCs" -> {0, 3, 7, 10}|>
  };
  Flatten@Table[<|"RootPC" -> root, "PCs" -> Mod[root + tpl["PCs"], 12], "Label" -> (Lookup[noteNames, root, "C"] <> tpl["Quality"])|>, {tpl, chordTemplates}, {root, 0, 11}]
];

HowlEstimateBeatSeconds[rawNotes_] := Module[{starts, deltas, positive},
  starts = Sort[rawNotes[[All, 1]]]; deltas = Differences[starts]; positive = Select[deltas, # > 0.05 &];
  If[Length[positive] == 0, 0.5, Median[positive]]
];
HowlEstimateBeatsPerBar[file_] := Module[{sigCandidates, firstSig},
  sigCandidates = DeleteCases[Quiet@Check[{Import[file, {"MIDI", "TimeSignatures"}], Import[file, {"MIDI", "TimeSignature"}]}, {}], _Failure | $Failed | Missing[__] | {}];
  If[Length[sigCandidates] == 0, Return[4]];
  firstSig = First@Flatten@sigCandidates;
  Which[MatchQ[firstSig, {_Integer, _Integer}], firstSig[[1]], MatchQ[firstSig, {_?NumericQ, _?NumericQ}], Round[firstSig[[1]]], AssociationQ[firstSig] && KeyExistsQ[firstSig, "Numerator"], firstSig["Numerator"], True, 4]
];
HowlBarWindows[startT_, endT_, barSeconds_] := Module[{barStarts},
  barStarts = Range[Floor[startT/barSeconds]*barSeconds, endT, barSeconds];
  Select[Map[<|"Start" -> #, "End" -> Min[# + barSeconds, endT]|> &, barStarts], #["End"] > startT &]
];
HowlBestChordForWindow[rawNotes_, win_, candidates_] := Module[{overlapWeight, scoreChordCandidate, notesIn, weighted, scored, best},
  overlapWeight[note_] := Module[{a, b, overlap, vol}, a = note[[1]]; b = note[[1]] + note[[2]]; overlap = Max[0, Min[b, win["End"]] - Max[a, win["Start"]]]; vol = note[[3]]; overlap*(0.7 + 0.3*vol)];
  scoreChordCandidate[weightedNotes_, cand_] := Module[{weights, pcs, chordPcs, inW, outW, bassPc, bassBonus},
    If[Length[weightedNotes] == 0, Return[-Infinity]];
    weights = weightedNotes[[All, 5]]; pcs = Mod[weightedNotes[[All, 4]], 12]; chordPcs = cand["PCs"];
    inW = Total@Pick[weights, MemberQ[chordPcs, #] & /@ pcs, True]; outW = Total@Pick[weights, MemberQ[chordPcs, #] & /@ pcs, False];
    bassPc = Mod[(First@SortBy[weightedNotes, #[[4]] &])[[4]], 12]; bassBonus = If[bassPc == cand["RootPC"], 0.4, 0.0]; inW - 0.75*outW + bassBonus
  ];
  notesIn = Select[rawNotes, (#[[1]] < win["End"]) && (#[[1]] + #[[2]] > win["Start"]) &];
  weighted = Map[Append[#, overlapWeight[#]] &, notesIn];
  If[Length[weighted] == 0, Return[<|"Start" -> win["Start"], "End" -> win["End"], "Chord" -> "N.C.", "PCs" -> {}, "RootPC" -> Missing["None"]|>]];
  scored = Map[Append[#, "Score" -> scoreChordCandidate[weighted, #]] &, candidates];
  best = First@MaximalBy[scored, # ["Score"] &];
  <|"Start" -> win["Start"], "End" -> win["End"], "Chord" -> best["Label"], "PCs" -> best["PCs"], "RootPC" -> best["RootPC"]|>
];
HowlActiveChordAtTime[t_, timeline_] := Module[{m}, m = SelectFirst[timeline, # ["Start"] <= t < # ["End"] &]; If[m === Missing["NotFound"], Last[timeline], m]];
HowlChooseConstrainedPitch[origPitch_, predPitch_, prevPitch_, allowedPcs_, maxDelta_] := Module[{oct, choices, target, pool, scored},
  target = Round[origPitch] + Clip[Round[predPitch] - Round[origPitch], {-maxDelta, maxDelta}];
  oct = Quotient[Round[origPitch], 12];
  choices = If[Length[allowedPcs] == 0, {Round[origPitch]}, DeleteDuplicates[Sort[Flatten@Table[12*o + pc, {o, oct - 2, oct + 2}, {pc, allowedPcs}]]]];
  pool = Select[choices, Abs[# - Round[origPitch]] <= Max[2, maxDelta + 1] &];
  pool = DeleteDuplicates@Join[{target, Round[origPitch]}, pool];
  scored = SortBy[pool, {Abs[# - target] &, If[NumericQ[prevPitch], Abs[# - prevPitch], 0] &, Abs[# - Round[origPitch]] &} &];
  Round[First[scored]]
];

HowlCoercePitch[p_] := HowlNoteToInt[p];
HowlCanonicalizeEncoded[enc_] := Module[{clean = N[enc]},
  If[Length[clean] == 0, Return[clean]];
  clean[[All, 1]] = Max[0.001, #] & /@ clean[[All, 1]];
  clean[[All, 2]] = Max[0.05, #] & /@ clean[[All, 2]];
  clean[[All, 3]] = Clip[#, {0.1, 1.0}] & /@ clean[[All, 3]];
  clean[[All, 4]] = HowlCoercePitch /@ clean[[All, 4]];
  clean
];
HowlEncToNetInput[encSong_] := With[{clean = HowlCanonicalizeEncoded[encSong]}, <|"NoteData" -> NumericArray[clean[[All, 1 ;; 3]], "Real32"], "Notes" -> Round[clean[[All, 4]]]|>];

ExtendMotifWithContext[predictorPath_String, motifMidiPath_String, motifStart_?NumericQ, motifEnd_?NumericQ, extensionSec_?NumericQ, analysisMidiPath_:Automatic, outputMidi_String:"Scripts/motif_extended.mid", analysisJson_String:"Scripts/motif_extended_analysis.json", maxPitchDelta_Integer:2, timingBlend_?NumericQ:0.08, keepMotifRhythm_?NumericQ:0.90] :=
 Module[{predictor, motifFile, analysisFile, motifImported, analysisImported, motifRawAbs, motifAbsWindow, motifOffset, motifRaw,
   motifMelodyRaw, motifHarmonyRaw, analysisRaw, analysisPcs, analysisWeights, globalKey, beatSeconds, beatsPerBar, barSeconds,
   motifDuration, extensionEnd, bars, candidates, rawTimeline, timeline, motifMelodyEncoded, notes, data, templateDur, templateVol,
   meanDelay, currentTime, i, seqEncoded, predInput, pred, origRef, predPitch, predData, barCtx, allowedPcs, prevPitch, newPitch,
   refDur, refVol, newDelay, mixed, newDur, newVol, melodyExtendedEncoded, melodyExtendedRaw, combinedRaw, combinedEncoded, analysis},

  predictor = Import[predictorPath];
  If[FailureQ[predictor], Print["Failed to import predictor: ", predictorPath]; Return[$Failed]];

  motifFile = HowlResolveMidiFile[motifMidiPath];
  analysisFile = If[analysisMidiPath === Automatic, motifFile, HowlResolveMidiFile[analysisMidiPath]];
  If[motifFile === $Failed || analysisFile === $Failed, Print["Could not resolve motif/analysis MIDI paths."]; Return[$Failed]];

  motifImported = HowlMidiImport[motifFile];
  analysisImported = If[analysisFile === motifFile, motifImported, HowlMidiImport[analysisFile]];
  If[FailureQ[motifImported["Notes"]] || FailureQ[analysisImported["Notes"]], Print["MIDI import failed."]; Return[$Failed]];

  motifRawAbs = HowlSanitizeRawRows[HowlEncodedToStartTimes[motifImported["EncodedNotesV1"]]];
  motifAbsWindow = Select[motifRawAbs, motifStart <= #[[1]] <= motifEnd &];
  If[Length[motifAbsWindow] < 8, Print["Motif window has too few notes."]; Return[$Failed]];

  motifOffset = Min[motifAbsWindow[[All, 1]]];
  motifRaw = SortBy[motifAbsWindow /. {s_, d_, v_, p_} :> {s - motifOffset, d, v, p}, First];
  {motifMelodyRaw, motifHarmonyRaw} = HowlSplitMelodyHarmonyByOnset[motifRaw, 0.03];
  If[Length[motifMelodyRaw] < 8, motifMelodyRaw = motifRaw; motifHarmonyRaw = {}];

  analysisRaw = HowlSanitizeRawRows[HowlEncodedToStartTimes[analysisImported["EncodedNotesV1"]]];
  If[Length[analysisRaw] == 0, Print["Analysis MIDI has no valid notes."]; Return[$Failed]];
  analysisPcs = Mod[analysisRaw[[All, 4]], 12];
  analysisWeights = analysisRaw[[All, 2]]*(0.7 + 0.3*analysisRaw[[All, 3]]);
  globalKey = HowlDetectKeyFromPcs[analysisPcs, analysisWeights];

  beatSeconds = HowlEstimateBeatSeconds[motifRaw];
  beatsPerBar = HowlEstimateBeatsPerBar[analysisFile];
  barSeconds = Max[0.4, beatSeconds*beatsPerBar];
  motifDuration = Max[motifRaw[[All, 1]] + motifRaw[[All, 2]]];
  extensionEnd = motifDuration + extensionSec;

  bars = HowlBarWindows[0, extensionEnd, barSeconds];
  candidates = HowlBuildRootedCandidates[];
  rawTimeline = Map[HowlBestChordForWindow[motifRaw, #, candidates] &, bars];
  timeline = Map[If[# ["Chord"] === "N.C.", Join[#, <|"PCs" -> globalKey["ScalePCs"], "Chord" -> "Scale"|>], #] &, rawTimeline];

  motifMelodyEncoded = HowlCanonicalizeEncoded[HowlStartTimesToEncoded[motifMelodyRaw]];
  notes = Round[motifMelodyEncoded[[All, 4]]];
  data = N[motifMelodyEncoded[[All, 1 ;; 3]]];
  templateDur = motifMelodyEncoded[[All, 2]];
  templateVol = motifMelodyEncoded[[All, 3]];
  meanDelay = Max[0.02, Mean[motifMelodyEncoded[[All, 1]]]];

  currentTime = Total[data[[All, 1]]];
  i = 1;
  While[currentTime < extensionEnd,
    seqEncoded = Transpose@{data[[All, 1]], data[[All, 2]], data[[All, 3]], notes};
    predInput = HowlEncToNetInput[seqEncoded[[-Min[Length[notes], 256] ;;]]];
    pred = Quiet@Check[predictor[predInput, TargetDevice -> "CPU"], <||>];

    origRef = motifMelodyEncoded[[1 + Mod[i - 1, Length[motifMelodyEncoded]]]];
    predPitch = If[NumericQ[Lookup[pred, "NotesPred", origRef[[4]]]], HowlCoercePitch[Lookup[pred, "NotesPred", origRef[[4]]]], origRef[[4]]];
    predData = Lookup[pred, "NoteDataPred", origRef[[1 ;; 3]]];
    If[Head[predData] === NumericArray, predData = Normal[predData]];
    If[!ListQ[predData] || Length[predData] != 3 || !And @@ (NumericQ /@ predData), predData = origRef[[1 ;; 3]]];

    barCtx = HowlActiveChordAtTime[currentTime, timeline];
    allowedPcs = DeleteDuplicates@Join[barCtx["PCs"], globalKey["ScalePCs"]];
    prevPitch = If[Length[notes] > 0, Last[notes], Missing["None"]];
    newPitch = HowlChooseConstrainedPitch[origRef[[4]], predPitch, prevPitch, allowedPcs, maxPitchDelta];

    refDur = templateDur[[1 + Mod[i - 1, Length[templateDur]]]];
    refVol = templateVol[[1 + Mod[i - 1, Length[templateVol]]]];
    newDelay = Max[0.01, (keepMotifRhythm*origRef[[1]] + (1 - keepMotifRhythm)*meanDelay)];
    mixed = (1 - timingBlend)*origRef[[1 ;; 3]] + timingBlend*predData;
    newDur = Max[0.05, 0.85*refDur + 0.15*mixed[[2]]];
    newVol = Clip[0.85*refVol + 0.15*mixed[[3]], {0.2, 1.0}];

    AppendTo[notes, HowlCoercePitch[newPitch]];
    AppendTo[data, N[{newDelay, newDur, newVol}]];
    currentTime = currentTime + newDelay; i++;
  ];

  melodyExtendedEncoded = Transpose@{data[[All, 1]], data[[All, 2]], data[[All, 3]], notes};
  melodyExtendedRaw = HowlSanitizeRawRows[HowlEncodedToStartTimes[melodyExtendedEncoded]];
  combinedRaw = SortBy[Join[melodyExtendedRaw, motifHarmonyRaw], First];
  combinedEncoded = HowlCanonicalizeEncoded[HowlStartTimesToEncoded[combinedRaw]];

  Export[outputMidi, Sound[HowlDecodeNotesV1[combinedEncoded]], "MIDI"];

  analysis = <|
    "MotifMidi" -> motifFile, "AnalysisMidi" -> analysisFile,
    "MotifWindow" -> <|"Start" -> motifStart, "End" -> motifEnd|>,
    "DetectedKey" -> globalKey["Name"], "Timeline" -> timeline,
    "OutputTotalNotes" -> Length[combinedRaw],
    "Settings" -> <|"ExtensionSeconds" -> extensionSec, "MaxPitchDelta" -> maxPitchDelta, "TimingBlend" -> timingBlend, "KeepMotifRhythm" -> keepMotifRhythm|>
  |>;
  Export[analysisJson, analysis /. {Missing[__] -> "None"}, "JSON"];
  Print["Saved extended MIDI: ", outputMidi];
  Print["Saved analysis JSON: ", analysisJson];
  <|"OutputMidi" -> outputMidi, "AnalysisJson" -> analysisJson, "Analysis" -> analysis|>
];

# Howl

Howl is a Wolfram Language project for MIDI dataset creation, RNN training, and predictor-guided phrase generation.

## Core scripts (recommended)

- `Scripts/makeDataset.wls` → build `dataset.wxf`
- `Scripts/trainRnn.wls` → train and export `predictor_*.wlnet`
- `Scripts/reinterpretPhraseWithHarmony.wls` → key/chord analysis + conservative phrase reinterpretation
- `Scripts/callAndResponseRework.wls` → presentation workflow: bars 1–2 original melody, bars 3–4 predictor-constrained response

## 1) Build dataset

```bash
wolframscript -file Scripts/makeDataset.wls /path/to/midi/folder Scripts/dataset.wxf
```

## 2) Train model

```bash
wolframscript -file Scripts/trainRnn.wls Scripts/dataset.wxf Scripts/checkpoints
```

Use the exported `predictor_*.wlnet` in the next steps.

## 3) Reinterpret phrase with key/chord analysis (kept)

```bash
wolframscript -file Scripts/reinterpretPhraseWithHarmony.wls \
  Scripts/checkpoints_xxxx/predictor_yyyy.wlnet \
  Scripts/dataset.wxf \
  "Ashitaka" 4 25 \
  Scripts/ashitaka_reinterpreted.mid \
  Scripts/ashitaka_reinterpreted_analysis.json \
  1 0.80 0.05
```

Arguments:
1. `predictorFile`
2. `datasetFile`
3. `pieceQuery`
4. `startSec endSec`
5. `outputMidi` (optional)
6. `analysisJson` (optional)
7. `maxPitchDelta` (optional, default `2`)
8. `keepOriginalProb` (optional, default `0.65`)
9. `timingBlend` (optional, default `0.10`)

This exports MIDI and JSON including detected global key + bar-level chord timeline.

## 4) Call-and-response demo workflow (new)

This script is aimed at presentation-ready output:
- Bars 1–2: original melody
- Bars 3–4: predictor-constrained response over the same harmony
- Optional slight response key shift / rhythmic variation

```bash
wolframscript -file Scripts/callAndResponseRework.wls \
  Scripts/checkpoints_xxxx/predictor_yyyy.wlnet \
  Scripts/dataset.wxf \
  "Ashitaka" 4 35 \
  Scripts/ashitaka_call_response.mid \
  Scripts/ashitaka_call_response_analysis.json \
  2 0 0.15
```

Arguments:
1. `predictorFile`
2. `datasetFile`
3. `pieceQuery`
4. `startSec endSec`
5. `outputMidi` (optional)
6. `analysisJson` (optional)
7. `maxPitchDelta` (optional, default `2`)
8. `responseKeyShift` (optional semitones, default `0`)
9. `rhythmBlend` (optional `0..1`, default `0.15`)

## License

Unlicense.

# Howl 🐺🎶🎵🎶🎼🎹

Howl is a 🐺 Wolfram Language / ➕ Mathematica library for training computers to generate music.

I have really wanted to do this project for a while now, and I am very happy to have been able to work on it.

If anyone else is interested in adding new functionality or their own models, please feel free to contribute through issues / github.

## License

This software is released under the Unlicense, so basically do whatever you want with it.

## Gathering Data

The most important step in any machine learning challenge is data generation and gathering!

I recommend gathering midi files from artists that you like. Many artists will release .mid files of their works on websites like Patreon, or you may be able to find what you are looking for on MuseScore.

## Howl Usage

You can run this software *for free* using the [Wolfram Engine][2] and the `wolframscript` CLI.

The core functionality is *right now* in a Wolfram Language package called HowlMidiTools

To load the functions exposed by the library, simply run:

```mathematica
SetDirectory["Howl"]; (* Wherever the git repo is *)
<< "Howl/HowlMidiTools.wl"
```

To generate a dataset for training from a folder containing .mid or .midi ("MIDI") files, you can simply run:

```mathematica
datasetPath = "/path/to/my/midi/files";
dataset = Map[HowlMidiImport, HowlFindMidis[datasetPath]];
Length[dataset]
```

This can take some time (around 3 seconds per .mid file), so be patient!


On Windows Command Prompt, you can also run the helper script directly:

```bat
wolframscript -file Scripts\makeDataset.wls "C:\Users\jasolomon\Downloads\seminar-dataset-150" "Scripts\dataset.wxf"
```

Then, you can save your dataset to a file so you do not have to re-generate it in the future:

```mathematica
Export["dataset.wxf", dataset]
```

Now, you can use this note data to train your neural network to generate music.

You can use the Scripts/trainRnn.wls script to train your own simple 1-layer LSTM network for audio generation! 

## Fast iteration workflow (CPU-friendly)

If you are training on a smaller custom dataset (e.g. ~150 MIDI files) and do not have a GPU, start with shorter CPU runs and iterate quickly:

- In `Scripts/trainRnn.wls`, set `TargetDevice -> "CPU"`
- Keep `TimeGoal` at 1 hour for first-pass CPU runs, then increase it for longer training once outputs look good
- Lower `BatchSize` (e.g. 8-16)

This lets you validate data quality and output shape quickly before spending longer training time.

## Structured MIDI generation from CLI

After training, use `Scripts/generateMidi.wls` to generate a `.mid` with structure controls suitable for DAW editing.

Example:

```bash
wolframscript -script Scripts/generateMidi.wls \
  --predictor=Scripts/checkpoints_xxxx/predictor_yyyy.wlnet \
  --out=generated_84bpm_44.mid \
  --steps=500 \
  --bpm=84 \
  --timeSig=4/4 \
  --bars=16 \
  --quantizationDiv=4
```

Supported options:

- `--predictor` path to predictor `.wlnet` (required)
- `--out` output `.mid` path (default: `Scripts/generated.mid`)
- `--steps` generation steps before structuring (default: `400`)
- `--bpm` target tempo for beat grid snapping (default: `84`)
- `--timeSig` target bar grouping (`N/D`, default: `4/4`)
- `--bars` trim output to this many bars (default: `16`)
- `--quantizationDiv` grid per beat (default: `4`, i.e. quarter-beat)
- `--seedPitch`, `--seedDelay`, `--seedDuration`, `--seedVolume` for seed note controls

Also, check out [this helpful guide][1] for information about modeling sequential data with neural nets - if you want to dive in deep and make your own generator.

[1]: https://www.wolfram.com/language/12/neural-network-framework/train-a-net-to-model-english.html?product=mathematica

[2]: https://www.wolfram.com/engine/

## TODO

- [ ] Transformer nets support

**Justification:** Many modern algorithms for language modeling (e.g. GPT-3) opt to use transformers instead of recurrent networks. This approach could significantly reduce training times.  

- [ ] Note-Octave encoding instead of integer encoding

**Justification:** Note-octave encoding is a much smaller encoding than the current one-hot encoding using 120 note 'classes'. This note-octave encoding also has the potential to better represent relationships between different notes.

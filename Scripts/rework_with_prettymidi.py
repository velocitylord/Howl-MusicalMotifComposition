#!/usr/bin/env python3
"""Run Wolfram rework generation and produce PrettyMIDI analyses.

This script wraps Scripts/reinterpretPhraseWithHarmony.wls (for predictor-based phrase reinterpretation)
and uses pretty_midi to export comparable analysis JSON for:
1) the selected source piece
2) the reinterpreted output piece
"""

from __future__ import annotations

import argparse
import json
import re
import subprocess
from pathlib import Path
from typing import Any

import numpy as np
import pretty_midi


def _safe_float(x: Any) -> float | None:
    try:
        return float(x)
    except Exception:
        return None


def analyze_midi(path: Path) -> dict[str, Any]:
    pm = pretty_midi.PrettyMIDI(str(path))
    end_time = float(pm.get_end_time())

    note_objs = [n for inst in pm.instruments for n in inst.notes if not inst.is_drum]
    note_count = len(note_objs)

    pitches = np.array([n.pitch for n in note_objs], dtype=float) if note_objs else np.array([])
    durations = np.array([n.end - n.start for n in note_objs], dtype=float) if note_objs else np.array([])
    velocities = np.array([n.velocity for n in note_objs], dtype=float) if note_objs else np.array([])
    onsets = np.sort(np.array([n.start for n in note_objs], dtype=float)) if note_objs else np.array([])
    ioi = np.diff(onsets) if onsets.size > 1 else np.array([])

    tempo = _safe_float(pm.estimate_tempo()) if note_count >= 2 else None
    chroma = pm.get_chroma()
    chroma_sums = chroma.sum(axis=1)
    total_chroma = float(chroma_sums.sum())
    chroma_norm = (chroma_sums / total_chroma).tolist() if total_chroma > 0 else [0.0] * 12

    pitch_hist = pm.get_pitch_class_histogram(use_duration=True, use_velocity=True, normalize=True)

    return {
        "file": str(path),
        "duration_sec": end_time,
        "tempo_estimate_bpm": tempo,
        "instrument_count": len(pm.instruments),
        "note_count": note_count,
        "notes_per_second": (note_count / end_time) if end_time > 0 else 0.0,
        "pitch_range": {
            "min": int(pitches.min()) if pitches.size else None,
            "max": int(pitches.max()) if pitches.size else None,
            "mean": float(pitches.mean()) if pitches.size else None,
        },
        "duration_stats_sec": {
            "mean": float(durations.mean()) if durations.size else None,
            "median": float(np.median(durations)) if durations.size else None,
        },
        "velocity_stats": {
            "mean": float(velocities.mean()) if velocities.size else None,
            "median": float(np.median(velocities)) if velocities.size else None,
        },
        "ioi_stats_sec": {
            "mean": float(ioi.mean()) if ioi.size else None,
            "median": float(np.median(ioi)) if ioi.size else None,
        },
        "chroma_distribution": chroma_norm,
        "pitch_class_histogram": pitch_hist.tolist(),
    }


def run_rework(args: argparse.Namespace) -> tuple[Path | None, str]:
    cmd = [
        "wolframscript",
        "-file",
        "Scripts/reinterpretPhraseWithHarmony.wls",
        args.predictor,
        args.dataset,
        args.piece_query,
        str(args.start_sec),
        str(args.end_sec),
        args.output_midi,
        args.wolfram_analysis_json,
        str(args.max_pitch_delta),
        str(args.keep_original_prob),
        str(args.timing_blend),
    ]

    proc = subprocess.run(cmd, text=True, capture_output=True)
    combined = (proc.stdout or "") + ("\n" + proc.stderr if proc.stderr else "")
    if proc.returncode != 0:
        raise RuntimeError(f"reinterpretPhraseWithHarmony failed (exit {proc.returncode})\n{combined}")

    selected_piece = None
    m = re.search(r"Selected piece:\s*(.+)", combined)
    if m:
        selected_piece = Path(m.group(1).strip())

    return selected_piece, combined


def build_parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(description="Generate rework via Wolfram + analyze with pretty_midi")
    p.add_argument("--predictor", required=True)
    p.add_argument("--dataset", default="Scripts/dataset.wxf")
    p.add_argument("--piece-query", required=True)
    p.add_argument("--start-sec", type=float, required=True)
    p.add_argument("--end-sec", type=float, required=True)
    p.add_argument("--output-midi", default="Scripts/reinterpreted_phrase.mid")
    p.add_argument("--max-pitch-delta", type=int, default=2)
    p.add_argument("--keep-original-prob", type=float, default=0.65)
    p.add_argument("--wolfram-analysis-json", default="Scripts/reinterpreted_phrase_analysis.json")
    p.add_argument("--combined-analysis-json", default="Scripts/reinterpreted_phrase_pretty_analysis.json")
    p.add_argument("--timing-blend", type=float, default=0.10)
    p.add_argument("--max-pitch-delta", type=int, default=5)
    p.add_argument("--keep-original-prob", type=float, default=0.20)
    p.add_argument("--wolfram-analysis-json", default="Scripts/reinterpreted_phrase_analysis.json")
    p.add_argument("--combined-analysis-json", default="Scripts/reinterpreted_phrase_pretty_analysis.json")
    p.add_argument("--timing-blend", type=float, default=0.30)
    return p


def main() -> int:
    args = build_parser().parse_args()

    selected_piece, wolfram_log = run_rework(args)

    output_path = Path(args.output_midi)
    if not output_path.exists():
        raise FileNotFoundError(f"Expected output midi was not produced: {output_path}")

    source_analysis = analyze_midi(selected_piece) if selected_piece and selected_piece.exists() else {
        "warning": "Could not resolve selected source piece path from Wolfram output.",
        "selected_piece": str(selected_piece) if selected_piece else None,
    }
    rework_analysis = analyze_midi(output_path)

    combined = {
        "request": {
            "piece_query": args.piece_query,
            "start_sec": args.start_sec,
            "end_sec": args.end_sec,
            "output_midi": args.output_midi,
            "timing_blend": args.timing_blend,
        },
        "wolfram_log": wolfram_log,
        "source_piece_analysis": source_analysis,
        "rework_piece_analysis": rework_analysis,
        "wolfram_analysis_json": args.wolfram_analysis_json,
    }

    out_json = Path(args.combined_analysis_json)
    out_json.parent.mkdir(parents=True, exist_ok=True)
    out_json.write_text(json.dumps(combined, indent=2), encoding="utf-8")

    print(f"Saved combined pretty_midi analysis: {out_json}")
    print(f"Reworked MIDI: {output_path}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

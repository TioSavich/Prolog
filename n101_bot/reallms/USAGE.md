# N103 Hermes RealLMS Usage

This is the fast May 2026 path. RealLMS handles extraction or revoicing; the
local deterministic pairer handles the auditable pairing decision.

## Local Pairer

From `n101_bot/`:

```bash
.venv/bin/python -m bridge.n103_pairer_cli samples/n103_geometry_events.json
```

Structured output:

```bash
.venv/bin/python -m bridge.n103_pairer_cli samples/n103_geometry_events.json --json
```

Use `--all-candidates` when you want to inspect every possible pair instead of
a one-pass pairing where each student appears once.

The input can be:

- JSON list of `{student, source, timestamp, text}` rows.
- CSV with columns such as `student,text,source,timestamp`.
- Plain Zoom-style transcript text with lines like `Student Name: comment`.

## RealLMS Flow

1. Open a new RealLMS/Open WebUI chat.
2. Paste `N103_HERMES_PROMPT.md` as the first message.
3. If you have raw forum or Zoom text, paste it and copy the JSON event list the
   model returns into a local `.json` file.
4. Run the local pairer on that JSON file.
5. Paste the local pairing packet back into RealLMS if you want polished
   RealLMS-ready prompts.
6. Review every prompt before students see it.

## Why This Split

The pairing should be inspectable: the local code shows the exact geometry
signals, topics, PML stance contrast, evidence snippets, and score. The model is
only the prose layer. That is the version that can be ready for N103 by May 12,
2026 without pretending the ASR and live classroom-listening architecture is
already done.


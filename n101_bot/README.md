# n101_bot

Experimental vocabulary-protected chat built 2026-04-16. Status: prototype.

## Setup

```
cd n101_bot
.venv/bin/pip install -r requirements.txt
```

## One-shot demo

```
scripts/demo.sh "What is a quantity?"
```

Pipeline: Prolog loads vocabulary → emits JSON → Python composes system
prompt → calls REALLMS by default → Prolog validator checks the response
against incompatibility rules → prints the answer and any violations.

Set `REALLMS_API_KEY` before launching. The default model is
`gemma-4-31B-it`; override it with `REALLMS_MODEL=<model>` or
`HERMES_MODEL=<model>`.

## Local Hermes console

```
scripts/console.sh
```

Then open `http://127.0.0.1:8765`. The console uses the Critical Math /
Hermes visual language, calls the existing Prolog-in-the-loop bot, and uses
REALLMS for prose rendering. Without `REALLMS_API_KEY`, the chat pane returns
a configuration message instead of hanging on "thinking..."; pair graph and
metadata-only N103 workflow tools still work.

## Run the overnight test loop

```
nohup scripts/overnight_loop.sh > logs/overnight_$(date +%Y%m%d_%H%M%S).log 2>&1 &
```

Results in `logs/results.csv` and `logs/overnight_*.log`.

## N103 Hermes pairer

Near-term geometry discussion routing for N103:

```
.venv/bin/python -m bridge.n103_pairer_cli samples/n103_geometry_events.json
```

Inputs can be JSON, CSV, or Zoom-style transcript text. The output is an
instructor-reviewable pairing packet with geometry misconception/paradox
signals, evidence snippets, and a dyadic prompt for each recommended pair.
See `reallms/USAGE.md` for the RealLMS/Open WebUI workflow.

To summarize existing N103 `runs_output` folders without exposing raw student
work, point the safe loader at the parent directory:

```
python3 -m bridge.n103_run_loader /path/to/runs_output
```

It reads only `pairings_safe.json`, omits pseudonym maps, and rejects raw-text
fields such as `raw_text`, `student_id`, `author_raw_name`, or `evidence`.

## Authoring a new term

Add a file under `vocabularies/n101/` named `<term>.pl` following the
`term/5` schema in `src/vocabulary.pl`. Add the term to
`vocabularies/n101/_index.pl`. Run `make test-vocab`.

## Scope

See [PLAN.md](PLAN.md). This experiment does NOT edit model weights.
The `.vlp` emitter is real code but is not executed against a vindex
in this prototype; REALLMS + system-prompt injection is the runtime.

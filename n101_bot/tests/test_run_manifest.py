from __future__ import annotations

import importlib.util
import json
from pathlib import Path


ROOT = Path(__file__).resolve().parent.parent
SCRIPT = ROOT / "scripts" / "run_manifest.sh"
SAMPLE_MANIFEST = ROOT / "samples" / "fake_run_manifest.json"
SAMPLE_EVENTS = ROOT / "samples" / "fake_demo_events.json"


def _module():
    assert importlib.util.find_spec("bridge.run_manifest") is not None
    from bridge import run_manifest

    return run_manifest


def _write_json(path: Path, payload: object) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(payload), encoding="utf-8")


def _package_root(tmp_path: Path) -> Path:
    app_root = tmp_path / "Hermes" / "n101_bot"
    app_root.mkdir(parents=True)
    return app_root


def _sample_events() -> list[dict]:
    return json.loads(SAMPLE_EVENTS.read_text(encoding="utf-8"))


def test_sample_run_manifest_is_relative_and_metadata_only():
    manifest = json.loads(SAMPLE_MANIFEST.read_text(encoding="utf-8"))

    assert manifest["run_id"] == "fake_manifest_demo"
    assert manifest["events_file"] == "fake_demo_events.json"
    assert not Path(manifest["events_file"]).is_absolute()
    serialized = json.dumps(manifest)
    for forbidden in ["REALLMS_API_KEY", "raw_text", "student_id", "source_id", "path"]:
        assert forbidden not in serialized


def test_run_manifest_reads_inputs_and_writes_derived_and_outputs(tmp_path):
    run_manifest = _module()
    app_root = _package_root(tmp_path)
    package_root = app_root.parent
    input_root = package_root / "data" / "inputs"
    manifest_path = input_root / "manifests" / "fake.json"
    events_path = input_root / "events" / "fake_events.json"
    _write_json(events_path, _sample_events())
    _write_json(
        manifest_path,
        {
            "run_id": "local_fake",
            "events_file": "events/fake_events.json",
            "description": "Synthetic manifest boundary test.",
        },
    )
    calls = []

    class FakeWorker:
        def request(self, op, **payload):
            calls.append({"op": op, "payload": payload})
            if op == "batch_event_score":
                return [{"event_id": event["event_id"]} for event in payload["events"]]
            if op == "pair_score":
                return [{"pair_id": "pair_ev_demo_0001_ev_demo_0002", "score": 8}]
            if op == "pair_graph":
                return {"nodes": [], "edges": [{"id": "pair_ev_demo_0001_ev_demo_0002"}]}
            raise AssertionError(op)

        def close(self):
            calls.append({"op": "close"})

    packet = run_manifest.run_manifest(
        manifest_path,
        app_root=app_root,
        env={},
        worker_factory=FakeWorker,
    )

    assert packet["run_id"] == "local_fake"
    assert packet["privacy"] == "canonical_metadata_only_no_student_work"
    assert packet["event_count"] == 2
    assert packet["score_count"] == 2
    assert packet["pair_count"] == 1
    assert packet["graph_edge_count"] == 1
    assert [call["op"] for call in calls] == [
        "batch_event_score",
        "pair_score",
        "pair_graph",
        "close",
    ]

    derived_path = Path(packet["derived_events_path"])
    output_path = Path(packet["result_path"])
    summary_path = Path(packet["summary_path"])
    assert derived_path == package_root / "data" / "derived" / "runs" / "local_fake" / "canonical_events.json"
    assert output_path == package_root / "data" / "outputs" / "runs" / "local_fake" / "result.json"
    assert summary_path == package_root / "data" / "outputs" / "runs" / "local_fake" / "summary.json"
    assert json.loads(derived_path.read_text(encoding="utf-8")) == _sample_events()
    assert json.loads(output_path.read_text(encoding="utf-8"))["pair_count"] == 1
    assert json.loads(summary_path.read_text(encoding="utf-8"))["run_id"] == "local_fake"


def test_run_manifest_rejects_input_path_inside_code_root(tmp_path):
    run_manifest = _module()
    app_root = _package_root(tmp_path)
    input_root = app_root.parent / "data" / "inputs"
    unsafe_events = app_root / "samples" / "fake_demo_events.json"
    _write_json(unsafe_events, _sample_events())
    manifest_path = input_root / "manifest.json"
    _write_json(
        manifest_path,
        {
            "run_id": "unsafe",
            "events_file": str(unsafe_events),
        },
    )

    try:
        run_manifest.run_manifest(manifest_path, app_root=app_root, env={}, worker_factory=lambda: None)
    except ValueError as exc:
        assert "events_file must live under input root" in str(exc)
    else:
        raise AssertionError("expected unsafe code-root input path rejection")


def test_run_manifest_rejects_manifest_path_inside_code_root(tmp_path):
    run_manifest = _module()
    app_root = _package_root(tmp_path)
    input_root = app_root.parent / "data" / "inputs"
    events_path = input_root / "events" / "fake_events.json"
    manifest_path = app_root / "samples" / "fake_run_manifest.json"
    _write_json(events_path, _sample_events())
    _write_json(manifest_path, {"run_id": "unsafe_manifest", "events_file": "events/fake_events.json"})

    try:
        run_manifest.run_manifest(manifest_path, app_root=app_root, env={}, worker_factory=lambda: None)
    except ValueError as exc:
        assert "manifest must live under input root" in str(exc)
    else:
        raise AssertionError("expected unsafe manifest path rejection")


def test_run_manifest_rejects_raw_event_fields(tmp_path):
    run_manifest = _module()
    app_root = _package_root(tmp_path)
    input_root = app_root.parent / "data" / "inputs"
    events_path = input_root / "events" / "unsafe.json"
    manifest_path = input_root / "manifest.json"
    _write_json(events_path, [{"event_id": "bad", "raw_text": "student work"}])
    _write_json(manifest_path, {"run_id": "unsafe_raw", "events_file": "events/unsafe.json"})

    try:
        run_manifest.run_manifest(manifest_path, app_root=app_root, env={}, worker_factory=lambda: None)
    except ValueError as exc:
        assert "raw_text" in str(exc)
    else:
        raise AssertionError("expected raw event field rejection")


def test_run_manifest_script_uses_portable_python_and_module_entrypoint():
    script = SCRIPT.read_text(encoding="utf-8")

    assert "HERMES_PYTHON" in script
    assert 'PYTHON_BIN="$HERMES_PYTHON"' in script
    assert 'PYTHON_BIN="$ROOT/.venv/bin/python"' in script
    assert 'PYTHON_BIN="python3"' in script
    assert 'export PYTHONPATH="$ROOT:$ROOT/vendor${PYTHONPATH:+:$PYTHONPATH}"' in script
    assert '"$PYTHON_BIN" -m bridge.run_manifest "$@"' in script

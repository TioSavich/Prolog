from __future__ import annotations

import subprocess

from bridge import prepare, prolog


def test_prolog_detect_uses_hermes_swipl_override(monkeypatch):
    calls = []

    def fake_run(cmd, **kwargs):
        calls.append(cmd)
        return subprocess.CompletedProcess(cmd, 0, stdout="[]\n", stderr="")

    monkeypatch.setenv("HERMES_SWIPL", "/Volumes/Hermes/runtime/swi-prolog/bin/swipl")
    monkeypatch.setattr(prolog.subprocess, "run", fake_run)

    assert prolog.detect("unrelated") == []
    assert calls[0][0] == "/Volumes/Hermes/runtime/swi-prolog/bin/swipl"


def test_geometry_query_uses_bundled_swipl_when_no_override(monkeypatch, tmp_path):
    app_root = tmp_path / "Hermes.app" / "Contents" / "Resources" / "n101_bot"
    bundled = app_root / "runtime" / "swi-prolog" / "bin" / "swipl"
    bundled.parent.mkdir(parents=True)
    bundled.write_text("#!/bin/sh\n", encoding="utf-8")
    runner = app_root / "src" / "geometry_runner.pl"
    calls = []

    def fake_run(cmd, **kwargs):
        calls.append(cmd)
        return subprocess.CompletedProcess(cmd, 0, stdout='{"ok": true}\n', stderr="")

    monkeypatch.delenv("HERMES_SWIPL", raising=False)
    monkeypatch.setattr(prolog, "ROOT", app_root)
    monkeypatch.setattr(prolog, "GEOMETRY_RUNNER_PL", runner)
    monkeypatch.setattr(prolog.subprocess, "run", fake_run)

    assert prolog.geometry_query("pck_synthesis_for", ["tilted_square"]) == {"ok": True}
    assert calls[0][0] == str(bundled)


def test_prepare_regenerate_uses_hermes_swipl_override(monkeypatch, tmp_path):
    logs = tmp_path / "logs"
    src = tmp_path / "src" / "vocabulary.pl"
    src.parent.mkdir(parents=True)
    src.write_text(":- module(vocabulary, []).\n", encoding="utf-8")
    calls = []

    def fake_run(cmd, **kwargs):
        calls.append(cmd)
        logs.mkdir(exist_ok=True)
        for name in ("vocabulary.json", "vocabulary.lql", "system_prompt.txt"):
            (logs / name).write_text("ok\n", encoding="utf-8")
        return subprocess.CompletedProcess(cmd, 0, stdout="", stderr="")

    monkeypatch.setenv("HERMES_SWIPL", "/Volumes/Hermes/runtime/swi-prolog/bin/swipl")
    monkeypatch.setattr(prepare, "ROOT", tmp_path)
    monkeypatch.setattr(prepare, "LOGS", logs)
    monkeypatch.setattr(prepare, "SRC", src)
    monkeypatch.setattr(prepare.subprocess, "run", fake_run)

    prepare.regenerate()
    assert calls[0][0] == "/Volumes/Hermes/runtime/swi-prolog/bin/swipl"

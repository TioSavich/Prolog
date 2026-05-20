"""Persistent SWI-Prolog worker for Hermes.

The existing bridge shells out to `swipl` per call. This adapter keeps one
bounded local worker alive and speaks newline-delimited JSON over stdin/stdout.
It is intentionally local-only and keeps the old subprocess bridge available
elsewhere as fallback.
"""
from __future__ import annotations

import json
import os
import select
import subprocess
import time
from pathlib import Path
from typing import Any

from .runtime_env import resolve_swipl as resolve_runtime_swipl
from .runtime_env import resolve_umedcta_root


ROOT = Path(__file__).resolve().parent.parent
WORKER_PL = ROOT / "src" / "hermes_worker.pl"


class PersistentPrologError(RuntimeError):
    pass


def resolve_swipl(swipl: str | None = None, *, root: Path | str = ROOT) -> str:
    return resolve_runtime_swipl(swipl, root=root)


class PersistentPrologWorker:
    def __init__(
        self,
        *,
        umedcta_root: Path | str | None = None,
        swipl: str | None = None,
        timeout: float = 5.0,
    ) -> None:
        self.umedcta_root = Path(umedcta_root) if umedcta_root is not None else resolve_umedcta_root(ROOT)
        self.swipl = resolve_swipl(swipl)
        self.timeout = timeout
        self._seq = 0
        self._proc: subprocess.Popen[str] | None = None

    def request(self, op: str, **payload: Any) -> Any:
        request = {"id": self._next_id(), "op": op, **payload}
        response = self.raw_request(request)
        if not response.get("ok"):
            error = response.get("error") or {}
            message = error.get("message") or "unknown worker error"
            raise PersistentPrologError(message)
        return response.get("result")

    def raw_request(self, request: dict[str, Any]) -> dict[str, Any]:
        proc = self._ensure_started()
        assert proc.stdin is not None
        assert proc.stdout is not None
        line = json.dumps(request, ensure_ascii=False)
        try:
            proc.stdin.write(line + "\n")
            proc.stdin.flush()
        except BrokenPipeError:
            self.restart()
            raise PersistentPrologError("worker pipe closed while sending request")
        response_line = self._readline(proc)
        try:
            return json.loads(response_line)
        except json.JSONDecodeError as exc:
            self.restart()
            raise PersistentPrologError(f"worker returned malformed json: {response_line!r}") from exc

    def close(self) -> None:
        if self._proc is None:
            return
        proc = self._proc
        self._proc = None
        if proc.poll() is None:
            proc.terminate()
            try:
                proc.wait(timeout=2.0)
            except subprocess.TimeoutExpired:
                proc.kill()
                proc.wait(timeout=2.0)

    def restart(self) -> None:
        self.close()
        self._start()

    def _next_id(self) -> str:
        self._seq += 1
        return f"req_{self._seq:04d}"

    def _ensure_started(self) -> subprocess.Popen[str]:
        if self._proc is None or self._proc.poll() is not None:
            self._start()
        assert self._proc is not None
        return self._proc

    def _start(self) -> None:
        env = os.environ.copy()
        env["UMEDCTA_ROOT"] = str(self.umedcta_root)
        self._proc = subprocess.Popen(
            [self.swipl, "-q", "-s", str(WORKER_PL), "-g", "worker_main"],
            cwd=ROOT,
            env=env,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            bufsize=1,
        )

    def _readline(self, proc: subprocess.Popen[str]) -> str:
        assert proc.stdout is not None
        deadline = time.monotonic() + self.timeout
        fd = proc.stdout.fileno()
        while time.monotonic() < deadline:
            if proc.poll() is not None:
                stderr = ""
                if proc.stderr is not None:
                    stderr = proc.stderr.read()
                raise PersistentPrologError(
                    f"worker exited with {proc.returncode}: {stderr.strip()}"
                )
            remaining = max(0.0, deadline - time.monotonic())
            readable, _, _ = select.select([fd], [], [], min(0.1, remaining))
            if not readable:
                continue
            line = proc.stdout.readline()
            if line:
                return line.rstrip("\n")
        self.restart()
        raise PersistentPrologError("worker request timed out")

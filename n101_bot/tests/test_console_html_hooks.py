from __future__ import annotations

from pathlib import Path


HTML = Path(__file__).resolve().parent.parent / "web" / "hermes_gemma_console.html"


def _html() -> str:
    return HTML.read_text(encoding="utf-8")


def test_console_has_research_safe_pair_graph_and_revoice_hooks():
    html = _html()
    assert 'id="pair-graph"' in html
    assert 'id="revoice-output"' in html
    assert 'id="load-canonical-sample"' in html
    assert 'id="analyze-transcript"' in html
    assert 'id="load-workflows"' in html
    assert 'fetch(apiUrl("/api/n103_workflows")' in html
    assert "loadN103Workflows" in html
    assert "async function requestPairGraph" in html
    assert 'fetch(apiUrl("/api/pair_graph")' in html
    assert "async function requestRevoice" in html
    assert 'fetch(apiUrl("/api/revoice")' in html
    assert "renderPairGraph" in html
    assert "requestN103Pipeline" in html
    assert 'fetch(apiUrl("/api/n103_pipeline")' in html
    assert "renderRevoiceControls" in html
    assert "loadCanonicalPairSample" in html


def test_console_revoice_payload_does_not_send_raw_student_work_keys():
    html = _html()
    revoice_function = html.split("async function requestRevoice", 1)[1].split(
        "function renderPairGraph",
        1,
    )[0]
    forbidden = ["raw_text", "actor_id", "source_id", "path", "evidence"]
    for key in forbidden:
        assert key not in revoice_function


def test_console_revoice_sends_selected_model_not_default_sentinel():
    html = _html()
    revoice_function = html.split("async function requestRevoice", 1)[1].split(
        "function renderRevoiceControls",
        1,
    )[0]
    assert 'model: modelInput.value' in revoice_function
    assert 'model: "default"' not in revoice_function


def test_canonical_pair_sample_omits_raw_student_work_fields():
    html = _html()
    sample_block = html.split("const CANONICAL_PAIR_SAMPLE", 1)[1].split("];", 1)[0]
    forbidden = ["raw_text", "actor_id", "source_id", "path", "evidence", '"text"']
    for key in forbidden:
        assert key not in sample_block


def test_pair_lab_uses_metadata_only_graph_workflow():
    html = _html()
    assert 'fetch("/api/pair"' not in html
    assert "metadata-only event packet" in html
    assert "renderQuestionMoves" in html
    assert "question-moves" in html


def test_pair_graph_renderer_marks_edges_as_candidate_traces_not_uptake():
    html = _html()
    assert "function renderPairGraphDebug" in html
    assert "candidate pair rationale graph" in html
    assert "stroke-dasharray" in html
    assert "dashed means no uptake logged" in html
    assert "weight means priority" in html
    assert "not proof of shared validity or mutual recognition" in html

    render_block = html.split("function renderPairGraph(packet)", 1)[1].split(
        "function hasSafeEventHandles",
        1,
    )[0]
    assert "renderPairGraphDebug" in render_block


def test_pair_sample_loader_does_not_embed_raw_student_work():
    html = _html()
    sample_loader = html.split('document.querySelector("#load-sample")', 1)[1].split(
        "function loadCanonicalPairSample",
        1,
    )[0]
    assert "loadCanonicalPairSample()" in sample_loader
    for forbidden in ["JSON.stringify([", "text:", "student:"]:
        assert forbidden not in sample_loader


def test_console_api_calls_work_when_html_is_opened_from_file():
    html = _html()
    assert 'window.location.protocol === "file:"' in html
    assert 'const API_ORIGIN = window.location.protocol === "file:"' in html
    assert "function apiUrl(path)" in html
    for path in [
        "/api/models",
        "/api/reset",
        "/api/chat",
        "/api/n103_workflows",
        "/api/pair_graph",
        "/api/revoice",
    ]:
        assert f'fetch(apiUrl("{path}")' in html
    assert 'fetch("/api/' not in html


def test_chat_submit_routes_discussion_transcripts_to_pairer_before_chat():
    html = _html()
    assert "function looksLikeDiscussionTranscript" in html
    assert "async function routeTranscriptThroughPairer" in html

    submit_block = html.split('form.addEventListener("submit"', 1)[1].split(
        "function renderRecord",
        1,
    )[0]
    assert "looksLikeDiscussionTranscript(text)" in submit_block
    assert "await routeTranscriptThroughPairer(text)" in submit_block
    assert submit_block.index("looksLikeDiscussionTranscript(text)") < submit_block.index(
        'addMessage("user", text)'
    )


def test_console_copy_defaults_to_reallms_not_gemma2b():
    html = _html()
    assert "gemma:2b" not in html
    assert "local Gemma console" not in html
    assert "Ollama" not in html
    assert "REALLMS" in html


def test_console_renderer_status_distinguishes_key_presence_from_validated_readiness():
    html = _html()
    assert '`${renderer} key set`' in html
    assert '`${renderer} ready`' not in html

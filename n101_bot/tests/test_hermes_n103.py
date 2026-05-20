from pathlib import Path

from bridge.hermes_n103 import (
    HermesEvent,
    analyze_event,
    analyze_events,
    load_events,
    recommend_pairs,
)


ROOT = Path(__file__).resolve().parent.parent


def _codes(text):
    analysis = analyze_event(HermesEvent(student="S", text=text))
    return {signal.code for signal in analysis.signals}


def test_detects_inclusive_hierarchy_misconception():
    codes = _codes("A square is not a rectangle because rectangles are longer.")
    assert "square_not_rectangle" in codes


def test_detects_prototype_definition_paradox():
    codes = _codes("The drawing changes but the properties do not.")
    assert "paradox_definition_image" in codes


def test_classifies_pml_stance():
    analysis = analyze_event(
        HermesEvent(student="S", text="I wonder if it could also be a rectangle.")
    )
    assert analysis.stance.mode == "subjective"
    assert analysis.stance.polarity == "expansive"


def test_pairer_bridges_misconception_to_paradox():
    events = [
        HermesEvent(
            student="Alex",
            text="A square is not really a rectangle because rectangles are longer.",
        ),
        HermesEvent(
            student="Bri",
            text="A square can be both a square and a rectangle when definitions are inclusive.",
        ),
        HermesEvent(student="Cam", text="I liked the activity."),
    ]
    profiles = analyze_events(events)
    pairs = recommend_pairs(profiles, exclusive=False)
    top = pairs[0]
    assert {top.student_a, top.student_b} == {"Alex", "Bri"}
    assert "inclusive_shape_hierarchy" in top.topics
    assert any("misconception/paradox" in reason for reason in top.reasons)


def test_loads_json_sample():
    events = load_events(ROOT / "samples" / "n103_geometry_events.json")
    assert len(events) == 6
    assert events[0].student == "Alex"

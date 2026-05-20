from __future__ import annotations

import pytest

from bridge.event_importer import assert_pair_graph_safe, events_from_payload


def test_events_from_payload_parses_speaker_labeled_transcript_in_memory_only():
    events = events_from_payload(
        """
        Alice: A square is not a rectangle.
        Bob: I think it can be a rectangle too.
        """
    )

    assert [event.student for event in events] == ["Alice", "Bob"]
    assert [event.text for event in events] == [
        "A square is not a rectangle.",
        "I think it can be a rectangle too.",
    ]
    assert [event.source for event in events] == ["transcript", "transcript"]
    assert [event.event_id for event in events] == ["2", "3"]


def test_events_from_payload_accepts_json_object_wrappers():
    events = events_from_payload(
        {
            "events": [
                {
                    "speaker": "Student Real Name",
                    "message": "The drawing changed but the property did not.",
                    "source": "/private/raw/forum.csv",
                    "event_id": "raw-1",
                }
            ]
        }
    )

    assert len(events) == 1
    assert events[0].student == "Student Real Name"
    assert events[0].text == "The drawing changed but the property did not."
    assert events[0].source == "/private/raw/forum.csv"
    assert events[0].event_id == "raw-1"


def test_events_from_payload_rejects_unparseable_payload_shape():
    with pytest.raises(ValueError, match="events must be a JSON list"):
        events_from_payload(42)


def test_pair_graph_safety_rejects_raw_text_and_identity_fields():
    with pytest.raises(ValueError, match=r"\$\.events\[0\]\.raw_text"):
        assert_pair_graph_safe({"events": [{"raw_text": "student work"}]})

    with pytest.raises(ValueError, match=r"\$\.events\[0\]\.actor\.actor_id"):
        assert_pair_graph_safe({"events": [{"actor": {"actor_id": "real-student-id"}}]})

    with pytest.raises(ValueError, match=r"\$\.events\[0\]\.source\.path"):
        assert_pair_graph_safe({"events": [{"source": {"path": "/private/raw.csv"}}]})


def test_pair_graph_safety_allows_metadata_only_canonical_event():
    assert_pair_graph_safe(
        {
            "events": [
                {
                    "event_id": "ev_0001",
                    "actor": {"role": "student", "pseudonym": "S01"},
                    "source": {"source_type": "student_writing", "metadata": {"domain": "geometry"}},
                    "symbolic": {"commitments": ["classification_by_prototype"]},
                    "pml": {"mode": "objective", "polarity": "compressive"},
                }
            ]
        }
    )

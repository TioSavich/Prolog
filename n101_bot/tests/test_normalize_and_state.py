"""Tests for the kid-talk normalizer and ZCM dialogue state."""
from bridge.normalize import normalize
from bridge.prolog import state_init, state_step


# ── Normalizer ──

def test_cuz_becomes_because():
    n = normalize("5+8=13 cuz I made a ten")
    assert "because" in n.normalized
    assert "cuz" not in n.normalized


def test_maked_becomes_made():
    n = normalize("I maked a ten")
    assert n.normalized == "I made a ten"
    assert n.changed


def test_idk_expands():
    n = normalize("idk, I just knew it")
    assert "I don't know" in n.normalized


def test_unicode_times_normalizes():
    n = normalize("5 × 4")
    assert "x" in n.normalized
    assert "×" not in n.normalized


def test_finger_counting_annotation():
    n = normalize("I used my fingers")
    assert "[counted from 1]" in n.normalized
    assert "fingers" in n.normalized  # raw phrase preserved in-line


def test_started_at_and_went_annotation():
    n = normalize("I started at 8 and went 9 10 11 12 13")
    assert "started at and counted on" in n.normalized


def test_raw_preserved_in_record():
    n = normalize("cuz i maked a ten")
    assert n.raw == "cuz i maked a ten"


def test_unchanged_when_nothing_to_normalize():
    n = normalize("A quantity is a measurable property.")
    assert not n.changed
    assert n.applied_rules == []


# ── Dialogue state ──

def test_state_init_is_neutral_and_cold():
    s = state_init()
    assert s.t == 0.0
    # both pulls start low
    assert s.a < 0.5
    assert s.v < 0.5


def test_fmst_without_commits_pulls_assessing():
    s0 = state_init()
    s1 = state_step(s0, "FMST", 0)
    assert s1.a > s0.a
    assert s1.t == 0.0  # no commits = no friction


def test_lst_moves_pull_advancing():
    s0 = state_init()
    s1 = state_step(s0, "LST", 0)
    assert s1.v > s1.a


def test_commitments_raise_temperature():
    s0 = state_init()
    s1 = state_step(s0, "FMST", 2)
    assert s1.t > 0.0


def test_catastrophe_fires_on_stuck_advance():
    s = state_init()
    s = state_step(s, "FMST", 0)
    s = state_step(s, "LST", 2)
    s = state_step(s, "LST", 2)
    s = state_step(s, "LST", 1)
    # sticky commitments + advance pull + assessing still up = near cusp
    assert s.near_cusp is True
    assert "cusp" in s.rendered


def test_cold_conversation_is_not_near_cusp():
    s = state_init()
    s = state_step(s, "FMST", 0)
    s = state_step(s, "AQST", 0)
    assert s.near_cusp is False


def test_state_rendered_is_nonempty():
    s = state_step(state_init(), "FMST", 0)
    assert "assessing" in s.rendered
    assert "advancing" in s.rendered
    assert "friction" in s.rendered

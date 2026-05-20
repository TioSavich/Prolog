"""Tests for the K-12 denylist and for the pivot-not-repair behavior.

The denylist gives a probability-0 guarantee on denylisted surface
strings reaching the user-visible text. It is the deterministic
safety floor.
"""
from bridge.denylist import filter_output, contains_denylisted, ALL_PATTERNS


# ── Denylist positive detections ──

def test_filter_catches_common_profanity():
    for bad in ["fuck that", "shit happens", "asshole", "goddamn", "bitch"]:
        assert filter_output(bad).blocked, f"expected block on: {bad!r}"


def test_filter_catches_self_harm_direct():
    for text in [
        "you should just die",
        "kill yourself",
        "I want to kill myself",
        "hurt myself",
        "cut myself",
        "suicide",
        "end your life",
        "end it all",
        "self-harm",
    ]:
        assert filter_output(text).blocked, f"expected block on: {text!r}"


def test_filter_catches_adult_register():
    for text in ["porn", "masturbation", "orgasm", "erection"]:
        assert filter_output(text).blocked, f"expected block on: {text!r}"


# ── Denylist false-positive immunity ──
#
# These look adjacent but are legitimate math/English; blocking them
# would cripple the bot. The patterns should be tight enough to let
# them through.

def test_filter_lets_math_idioms_through():
    for clean in [
        "A quantity is a measurable property.",
        "5 + 8 = 13 because I made a ten",
        "the student killed it on the test",  # idiom — 'killed' without 'self' reference
        "pass the paper",                     # 'ass' inside 'pass'
        "the class laughed",                  # 'ass' inside 'class'
        "what the heck",                      # mild — not on our list
        "what is a quantity?",
    ]:
        assert not filter_output(clean).blocked, f"unexpected block on: {clean!r}"


# ── Filter output shape ──

def test_blocked_returns_fallback_not_original():
    r = filter_output("fuck this")
    assert r.blocked
    assert "fuck" not in r.text  # fallback does not contain the offence
    assert r.original == "fuck this"
    assert len(r.hits) >= 1
    assert r.hits[0].category == "profanity"


def test_clean_text_passes_through_unchanged():
    text = "A quantity is a measurable property."
    r = filter_output(text)
    assert r.text == text
    assert not r.blocked
    assert r.hits == []


def test_contains_denylisted_probe_is_deterministic():
    # every call must return the same result — no randomness
    for _ in range(50):
        assert contains_denylisted("kill yourself") is True
        assert contains_denylisted("a quantity is measurable") is False


# ── Pattern-loading sanity ──

def test_pattern_count_is_reasonable():
    # 24 + whatever the user has in ~/.config/n101_bot/slurs.txt
    assert len(ALL_PATTERNS) >= 24


# ── Pivot-vs-repair semantics (unit-test the concept) ──

def test_pivot_is_distinct_from_repair_in_code():
    # After the pivot refactor, _pivot_to_fmst should exist and _repair should not
    from bridge import hc_bot
    bot_cls = hc_bot.HermeneuticBot
    assert hasattr(bot_cls, "_pivot_to_fmst"), \
        "_pivot_to_fmst must exist — commitment-fire pivots rather than sanitizes"
    assert not hasattr(bot_cls, "_repair"), \
        "_repair must be removed — sanitization of misconception traces is anti-Amy"

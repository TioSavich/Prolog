"""Multi-turn demo that threads the pieces together in one bot session:

    raw kid-talk
       → normalizer (rule-based; no training)
       → Prolog classifier + move grammar
       → Ollama renders Amy-style question or answer
       → Prolog checks commitments (and repairs if needed)
       → ZCM dialogue state updates, rendered as ASCII each turn
       → ledger + history survives across turns (non-monotonic)

The script picks a fast model (gemma:2b) by default — DeepSeek's <think>
scratchpad makes multi-turn demos slow. Pass --model deepseek-r1:14b for
the richer output.

Run:
    .venv/bin/python scripts/demo_zcm.py
    .venv/bin/python scripts/demo_zcm.py --model deepseek-r1:14b
"""
import argparse
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(ROOT))

from bridge.hc_bot import HermeneuticBot


SCRIPT = [
    # Turn 1: bare computation — kid-typo'd — should normalize then fire FMST
    "5+8=  cuz i wanna know",
    # Turn 2: student offers a strategy in colloquial form
    "i maked a ten, took 2 from the 8 and gave it to the 5",
    # Turn 3: still engaged — vocabulary probe
    "what is a quantity?",
    # Turn 4: trap — the teacher reports a student assertion that fires a commitment
    "my student says a number is a quantity",
    # Turn 5: teacher pushes harder
    "they keep saying a value is a quantity and 20 hours per week is a quantity",
]


def main(argv=None):
    parser = argparse.ArgumentParser()
    parser.add_argument("--model", default="gemma:2b")
    parser.add_argument("--temperature", type=float, default=0.2)
    args = parser.parse_args(argv)

    bot = HermeneuticBot(model=args.model)
    print(f"== Hermeneutic calculator demo — model={args.model} ==\n")

    for i, utterance in enumerate(SCRIPT, 1):
        print(f"── turn {i} ──")
        record = bot.ask(utterance, temperature=args.temperature)
        if record.normalization.changed:
            print(f"raw: {record.raw_question}")
            print(f"norm: {record.question}  [{len(record.normalization.applied_rules)} rules]")
        else:
            print(f"said: {record.question}")
        print(f"classified: {record.move.kind} → {record.move.move_tag}")
        if record.detected_terms:
            print(f"terms: {', '.join(record.detected_terms)}")
        print(f"\n{record.final_answer}\n")
        if record.final_commitments:
            print(f"{len(record.final_commitments)} commitment(s) fired:")
            for c in record.final_commitments:
                print(f"  [{c.term}] {c.rule}")
        if record.state_after.rendered:
            print(record.state_after.rendered)
            if record.state_after.near_cusp:
                print("  >>> approaching catastrophe <<<")
        print()

    # End-of-session summary
    print("── session summary ──")
    print(f"turns: {len(bot.session.history)}")
    print(f"ledger (unique rule-term fires): "
          f"{len(bot.session.ledger_rules())}")
    final = bot.session.state
    print(f"final state: A={final.a:.2f} V={final.v:.2f} T={final.t:.2f} "
          f"cusp={final.near_cusp}")


if __name__ == "__main__":
    main()

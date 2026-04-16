# Audit log: <module-name>

Written after [docs/phase5/fact-sheet-<module>.md](fact-sheet-<module>.md). Research goal from the fact-sheet is the cut criterion.

## Cut criterion

> <paste the research goal sentence from the fact-sheet>

## Decisions

| File | Section / line range | Claim | Verdict | Rationale |
|---|---|---|---|---|
| `path/to/file.md` | §N or L12–L40 | One-line paraphrase of the claim | keep \| substantiate \| cut \| commentary | One-line reason, referencing the research goal |

Verdicts:
- **keep** — serves the goal, code-backed.
- **substantiate** — serves the goal, not yet code-backed. Requires follow-up work in the module itself before the claim holds. (Record the follow-up as a one-liner; do not write the code here.)
- **cut** — does not serve the goal OR serves the goal but cannot be substantiated from the code. Relocated via `git mv` to `archive/<topic>/` or deleted.
- **commentary** — makes no empirical claim the code could back up. Kept, with a banner marking the register.

## Relocations

List of files moved to `archive/<topic>/` and why. Each entry one line.

## Deletions

List of files deleted outright (recoverable from git history). Each entry one line with rationale.

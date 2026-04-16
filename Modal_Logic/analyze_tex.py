#!/usr/bin/env python3
"""Analyze the Modal_Logic LaTeX sources for redundancy and structure."""

from __future__ import annotations

import argparse
import hashlib
import json
import re
from collections import Counter, defaultdict
from dataclasses import dataclass, asdict
from datetime import datetime
from pathlib import Path
from typing import Dict, Iterable, List, Tuple

SECTION_COMMANDS = [
    ("section", re.compile(r"\\section\*?\{(?P<title>[^}]*)\}")),
    ("subsection", re.compile(r"\\subsection\*?\{(?P<title>[^}]*)\}")),
    ("subsubsection", re.compile(r"\\subsubsection\*?\{(?P<title>[^}]*)\}")),
    ("paragraph", re.compile(r"\\paragraph\*?\{(?P<title>[^}]*)\}")),
]

NEWCOMMAND_RE = re.compile(r"\\(?:re)?newcommand\*?\{\\([^}]+)\}")
DEF_RE = re.compile(r"\\def\\([^\s{]+)")
ENV_RE = re.compile(r"\\begin\{([A-Za-z*@]+)\}")
LABEL_RE = re.compile(r"\\label\{([^}]+)\}")
CITE_RE = re.compile(r"\\cite\{([^}]+)\}")

PARAGRAPH_BREAK_RE = re.compile(r"\n\s*\n", re.MULTILINE)
WHITESPACE_RE = re.compile(r"\s+")
COMMAND_CLEAN_RE = re.compile(r"\\[a-zA-Z@]+(\[[^\]]*\])?(\{[^}]*\})?")
BRACE_RE = re.compile(r"[{}]")
COMMENT_RE = re.compile(r"(?<!\\)%.*")


@dataclass
class Section:
    level: str
    title: str
    line: int


@dataclass
class ParagraphDigest:
    hash: str
    start_line: int
    preview: str


@dataclass
class FileSummary:
    path: str
    lines: int
    words: int
    sections: List[Section]
    macro_defs: List[str]
    environments: Dict[str, int]
    labels: List[str]
    citations: List[str]
    paragraph_digests: List[ParagraphDigest]


def line_number(text: str, index: int) -> int:
    return text.count("\n", 0, index) + 1


def clean_title(raw: str) -> str:
    cleaned = COMMAND_CLEAN_RE.sub(" ", raw)
    cleaned = BRACE_RE.sub("", cleaned)
    return WHITESPACE_RE.sub(" ", cleaned).strip()


def extract_sections(text: str) -> List[Section]:
    sections: List[Section] = []
    for level, pattern in SECTION_COMMANDS:
        for match in pattern.finditer(text):
            title = clean_title(match.group("title"))
            sections.append(Section(level=level, title=title, line=line_number(text, match.start())))
    sections.sort(key=lambda s: s.line)
    return sections


def extract_macros(text: str) -> List[str]:
    macros = set()
    for pattern in (NEWCOMMAND_RE, DEF_RE):
        for match in pattern.finditer(text):
            macros.add(match.group(1))
    return sorted(macros)


def extract_environments(text: str) -> Dict[str, int]:
    counts: Counter[str] = Counter()
    for match in ENV_RE.finditer(text):
        env = match.group(1)
        if env != "document":
            counts[env] += 1
    return dict(counts)


def extract_labels(pattern: re.Pattern[str], text: str) -> List[str]:
    return [match.group(1) for match in pattern.finditer(text)]


def split_paragraphs(text: str) -> List[Tuple[str, int]]:
    paragraphs: List[Tuple[str, int]] = []
    lines = text.splitlines()
    start = 1
    buffer: List[str] = []
    for idx, line in enumerate(lines, start=1):
        if line.strip() == "":
            if buffer:
                paragraphs.append(("\n".join(buffer), start))
                buffer = []
            start = idx + 1
            continue
        if not buffer:
            start = idx
        buffer.append(line)
    if buffer:
        paragraphs.append(("\n".join(buffer), start))
    return paragraphs


def normalize_paragraph(text: str) -> str:
    no_comments = re.sub(r"(?<!\\)%.*", "", text)
    no_commands = COMMAND_CLEAN_RE.sub(" ", no_comments)
    simplified = BRACE_RE.sub(" ", no_commands)
    return WHITESPACE_RE.sub(" ", simplified).strip().lower()


def paragraph_digests(text: str) -> List[ParagraphDigest]:
    digests: List[ParagraphDigest] = []
    for content, start in split_paragraphs(text):
        normalized = normalize_paragraph(content)
        if len(normalized) < 120 or len(normalized.split()) < 20:
            continue
        digest = hashlib.sha1(normalized.encode("utf-8")).hexdigest()
        preview = WHITESPACE_RE.sub(" ", content).strip()
        preview = preview[:220] + ("…" if len(preview) > 220 else "")
        digests.append(ParagraphDigest(hash=digest, start_line=start, preview=preview))
    return digests


def analyze_file(path: Path) -> FileSummary:
    text = path.read_text(encoding="utf-8", errors="ignore")
    words = sum(len(line.split()) for line in text.splitlines())
    sections = extract_sections(text)
    macros = extract_macros(text)
    environments = extract_environments(text)
    labels = extract_labels(LABEL_RE, text)
    citations = extract_labels(CITE_RE, text)
    digests = paragraph_digests(text)
    return FileSummary(
        path=str(path),
        lines=text.count("\n") + 1,
        words=words,
        sections=sections,
        macro_defs=macros,
        environments=environments,
        labels=labels,
        citations=citations,
        paragraph_digests=digests,
    )


def repeated_items(summary: Iterable[Tuple[str, str, int]]) -> Dict[str, List[Tuple[str, int]]]:
    bucket: Dict[str, List[Tuple[str, int]]] = defaultdict(list)
    for normalized, file_path, line in summary:
        bucket[normalized].append((file_path, line))
    return {k: v for k, v in bucket.items() if len(v) > 1}


def build_markdown_report(files: List[FileSummary], duplicates: List[Dict], args) -> str:
    timestamp = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    lines: List[str] = ["# Modal Logic TeX Analysis", "", f"Generated: {timestamp}", ""]
    for file_summary in files:
        rel_path = Path(file_summary.path).relative_to(args.root)
        lines.append(f"## {rel_path}")
        lines.append("")
        lines.append(f"- Lines: {file_summary.lines}")
        lines.append(f"- Words: {file_summary.words}")
        lines.append(f"- Sections: {len(file_summary.sections)}")
        if file_summary.environments:
            most_common_envs = sorted(file_summary.environments.items(), key=lambda kv: kv[1], reverse=True)[:6]
            env_summary = ", ".join(f"{name} ({count})" for name, count in most_common_envs)
        else:
            env_summary = "None"
        lines.append(f"- Frequent environments: {env_summary}")
        if file_summary.macro_defs:
            lines.append(f"- Defined macros: {', '.join(file_summary.macro_defs[:8])}")
        lines.append("")
        if file_summary.sections:
            lines.append("### Section Outline")
            for section in file_summary.sections[:20]:
                lines.append(f"- ({section.level}) L{section.line}: {section.title}")
            lines.append("")

    if duplicates:
        lines.append("## Repeated Paragraphs Across Files")
        for entry in duplicates[:20]:
            lines.append(f"- Text hash `{entry['hash']}`: {entry['preview']}")
            for occurrence in entry["occurrences"]:
                path = Path(occurrence["path"]).relative_to(args.root)
                lines.append(f"  - {path}: line {occurrence['line']}")
            lines.append("")

    return "\n".join(lines)


def summarize(files: List[FileSummary], args) -> Dict:
    section_occurrences: Dict[str, List[Dict[str, str]]] = defaultdict(list)
    macro_occurrences: Dict[str, List[str]] = defaultdict(list)
    paragraph_occurrences: Dict[str, List[Dict[str, str]]] = defaultdict(list)

    for file_summary in files:
        for section in file_summary.sections:
            normalized = section.title.lower()
            section_occurrences[normalized].append({
                "title": section.title,
                "level": section.level,
                "line": section.line,
                "path": file_summary.path,
            })
        for macro in file_summary.macro_defs:
            macro_occurrences[macro].append(file_summary.path)
        for digest in file_summary.paragraph_digests:
            paragraph_occurrences[digest.hash].append({
                "path": file_summary.path,
                "line": digest.start_line,
                "preview": digest.preview,
            })

    repeated_sections = {
        key: value
        for key, value in section_occurrences.items()
        if len(value) > 1
    }

    repeated_macros = {
        key: value
        for key, value in macro_occurrences.items()
        if len(value) > 1
    }

    repeated_paragraphs = [
        {"hash": key, "occurrences": value, "preview": value[0]["preview"]}
        for key, value in paragraph_occurrences.items()
        if len(value) > 1
    ]

    return {
        "files": [asdict(file) for file in files],
        "repeated_sections": repeated_sections,
        "repeated_macros": repeated_macros,
        "repeated_paragraphs": repeated_paragraphs,
    }


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--root",
        type=Path,
        default=Path(__file__).resolve().parent.parent,
        help="Project root used for relative paths in the report.",
    )
    parser.add_argument(
        "--target",
        type=Path,
        default=Path(__file__).resolve().parent,
        help="Directory containing LaTeX files to analyze.",
    )
    parser.add_argument(
        "--output",
        type=Path,
        default=Path(__file__).resolve().parent / "tex_analysis_report.md",
        help="Markdown report output path.",
    )
    parser.add_argument(
        "--json",
        type=Path,
        default=Path(__file__).resolve().parent / "tex_analysis_report.json",
        help="Optional JSON output path.",
    )
    args = parser.parse_args()

    tex_files = sorted(args.target.glob("*.tex"))
    if not tex_files:
        raise SystemExit(f"No .tex files found in {args.target}")

    summaries = [analyze_file(path) for path in tex_files]
    aggregated = summarize(summaries, args)

    args.output.write_text(build_markdown_report(summaries, aggregated["repeated_paragraphs"], args), encoding="utf-8")
    args.json.write_text(json.dumps(aggregated, indent=2), encoding="utf-8")

    print(f"Analyzed {len(summaries)} LaTeX files.")
    print(f"Markdown report: {args.output}")
    print(f"JSON report: {args.json}")


if __name__ == "__main__":
    main()

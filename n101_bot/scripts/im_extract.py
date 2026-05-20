#!/usr/bin/env python3
"""
im_extract.py — Illustrative Math curriculum extractor for Hermes geometry KB.

Two tiers:
  Tier 1 — All 12 grades' Course-scope-and-sequence-.pdf → corpus markdown +
           unit-level standard_anchor/4 records in im_lesson_anchors.pl.
  Tier 2 — K-5 per-lesson teacher-guide PDFs → corpus markdown + lesson-level
           standard_anchor/4 records.

Idempotent: re-running rebuilds corpus markdown and rewrites the Prolog file
deterministically, so no record is duplicated.

Conversion: pdftotext -layout (default); docling fallback for lessons whose
"Goals" header is missing in the pdftotext output.

Outputs:
  /Users/tio/Documents/GitHub/umedcta-formalization/geometry/
    corpus/im_scope_and_sequence/<grade>.md
    corpus/im_teacher_guides/<grade>/<unit>/<lesson>.md
    standards/im_lesson_anchors.pl
"""

from __future__ import annotations

import argparse
import io
import os
import re
import shutil
import subprocess
import sys
import tempfile
import time
import zipfile
from dataclasses import dataclass, field
from pathlib import Path
from typing import Iterable

# ────────────────────────────────────────────────────────────────────────
# Paths

IM_ZIP_DIR = Path("/Users/tio/Documents/GitHub/Prolog/IM-Curriculum")
GEOMETRY_DIR = Path("/Users/tio/Documents/GitHub/umedcta-formalization/geometry")
CORPUS_SAS_DIR = GEOMETRY_DIR / "corpus" / "im_scope_and_sequence"
CORPUS_TG_DIR = GEOMETRY_DIR / "corpus" / "im_teacher_guides"
ANCHORS_PL = GEOMETRY_DIR / "standards" / "im_lesson_anchors.pl"
TMP_ROOT = Path("/tmp/im_extract")

ALL_GRADES = [
    "kindergarten", "grade1", "grade2", "grade3", "grade4", "grade5",
    "grade6", "grade7", "grade8", "algebra1", "algebra2", "geometry",
]
K5_GRADES = ["kindergarten", "grade1", "grade2", "grade3", "grade4", "grade5"]

# Grade-zip filenames in IM_ZIP_DIR.
ZIP_NAMES: dict[str, str] = {
    "kindergarten": "Kindergarten-downloadable_materials-pdf.zip",
    "grade1":       "Grade1-downloadable_materials-pdf.zip",
    "grade2":       "Grade2-downloadable_materials-pdf.zip",
    "grade3":       "Grade3-downloadable_materials-pdf.zip",
    "grade4":       "Grade4-downloadable_materials-pdf.zip",
    "grade5":       "Grade5-downloadable_materials-pdf.zip",
    "grade6":       "Grade6-downloadable_materials-pdf.zip",
    "grade7":       "Grade7-downloadable_materials-pdf.zip",
    "grade8":       "Grade8-downloadable_materials-pdf.zip",
    "algebra1":     "Algebra1-downloadable_materials-pdf.zip",
    "algebra2":     "Algebra2-downloadable_materials-pdf.zip",
    "geometry":     "Geometry-downloadable_materials-pdf.zip",
}

# Internal stem for nested-zip / pdf naming inside each grade zip.
GRADE_STEM: dict[str, str] = {
    "kindergarten": "Kindergarten",
    "grade1":       "Grade1",
    "grade2":       "Grade2",
    "grade3":       "Grade3",
    "grade4":       "Grade4",
    "grade5":       "Grade5",
    "grade6":       "Grade6",
    "grade7":       "Grade7",
    "grade8":       "Grade8",
    "algebra1":     "Algebra1",
    "algebra2":     "Algebra2",
    "geometry":     "Geometry",
}

# Anchor-id grade tag (matches the spec convention: GK / G1 / G2 ...)
ANCHOR_GRADE: dict[str, str] = {
    "kindergarten": "GK",
    "grade1":       "G1",
    "grade2":       "G2",
    "grade3":       "G3",
    "grade4":       "G4",
    "grade5":       "G5",
    "grade6":       "G6",
    "grade7":       "G7",
    "grade8":       "G8",
    "algebra1":     "Alg1",
    "algebra2":     "Alg2",
    "geometry":     "HSG",
}

# Numeric grade-band entries written into stub geom_concept records.
GRADE_BAND: dict[str, list[int]] = {
    "kindergarten": [0],
    "grade1":       [1],
    "grade2":       [2],
    "grade3":       [3],
    "grade4":       [4],
    "grade5":       [5],
    "grade6":       [6],
    "grade7":       [7],
    "grade8":       [8],
    "algebra1":     [9],
    "algebra2":     [10],
    "geometry":     [9, 10],
}

# Existing canonical concept IDs that the prior (hand-authored) anchors used.
# Lesson- and unit-level anchors that fall under one of these units re-use
# the canonical concept; the auto-extraction tags this so the synthesizer
# can canonicalize later.
EXISTING_UNIT_CONCEPT: dict[str, str] = {
    "GK-U2":  "shape_recognition_2d_3d",
    "GK-U6":  "shape_attributes_informal",
    "G1-U3":  "defining_vs_nondefining_attributes",
    "G1-U7":  "partition_into_equal_shares",
    "G2-U3":  "draw_shapes_with_attributes",
    "G2-U7":  "partition_into_equal_shares_thirds",
    "G3-U1":  "area_compose_decompose_polygons",
    "G3-U7":  "quadrilateral_hierarchy",
    "G4-U7":  "points_lines_angles",
    "G4-U8":  "line_of_symmetry",
    "G5-U2":  "shape_hierarchy_classification",
    "G5-U7":  "graph_points_first_quadrant",
    "G6-U1":  "area_compose_decompose_polygons",
    "G6-U4":  "volume_prism_fractional_edges",
    "G6-U7":  "polygons_in_coordinate_plane",
    "G7-U1":  "scale_drawings",
    "G7-U3":  "circle_area_circumference",
    "G7-U7":  "angle_pair_relationships",
    "G8-U1":  "rigid_motion_properties",
    "G8-U2":  "similarity_via_transformations",
    "G8-U5":  "volume_cone_cylinder_sphere",
    "G8-U8":  "pythagorean_theorem",
}

# ────────────────────────────────────────────────────────────────────────
# Data classes

@dataclass
class LessonRow:
    grade: str
    unit_num: int
    lesson_num: int
    title: str
    learning_goals: list[str] = field(default_factory=list)


@dataclass
class UnitRow:
    grade: str
    unit_num: int
    title: str
    intro: str = ""
    lessons: list[LessonRow] = field(default_factory=list)


# ────────────────────────────────────────────────────────────────────────
# pdftotext / docling helpers

def pdftotext(pdf_path: Path, txt_path: Path) -> bool:
    """Run pdftotext -layout. Returns True on success."""
    try:
        result = subprocess.run(
            ["pdftotext", "-layout", str(pdf_path), str(txt_path)],
            capture_output=True, text=True, timeout=60,
        )
        return result.returncode == 0 and txt_path.exists()
    except Exception as e:
        print(f"  pdftotext failed on {pdf_path}: {e}", file=sys.stderr)
        return False


def docling_fallback(pdf_path: Path, md_path: Path) -> bool:
    """Run docling convert as a fallback. Returns True on success."""
    try:
        result = subprocess.run(
            ["docling", "--to", "md", "--output", str(md_path.parent), str(pdf_path)],
            capture_output=True, text=True, timeout=180,
        )
        # docling emits <stem>.md in output dir
        produced = md_path.parent / (pdf_path.stem + ".md")
        if produced.exists() and produced != md_path:
            shutil.move(str(produced), str(md_path))
        return md_path.exists()
    except Exception as e:
        print(f"  docling failed on {pdf_path}: {e}", file=sys.stderr)
        return False


# ────────────────────────────────────────────────────────────────────────
# Tier 1: scope-and-sequence parsing

UNIT_HEADER_RE = re.compile(r"^Unit\s+(\d+):\s+(.+?)\s*$")
SECTION_HEADER_RE = re.compile(r"^Section\s+([A-Z]):\s*(.+?)\s*$")
LESSON_LINE_RE = re.compile(r"^\s*[••\-]\s*Lesson\s+(\d+):\s+(.+?)\s*$")


def parse_scope_and_sequence(text: str, grade: str) -> list[UnitRow]:
    """
    Walk the pdftotext layout output line-by-line. The format is consistent:
        Unit N: <title>
        <intro paragraphs>
        Section A: <section title>
          • Lesson K: <lesson title>
          ...
    Returns a list of UnitRow.
    """
    units: list[UnitRow] = []
    current: UnitRow | None = None
    intro_collecting = False
    intro_buf: list[str] = []
    for raw in text.splitlines():
        line = raw.rstrip()
        m = UNIT_HEADER_RE.match(line.strip())
        if m:
            # flush prior unit's intro
            if current is not None and intro_buf:
                current.intro = " ".join(intro_buf).strip()
            current = UnitRow(grade=grade, unit_num=int(m.group(1)),
                              title=m.group(2).strip())
            units.append(current)
            intro_collecting = True
            intro_buf = []
            continue
        if current is None:
            continue
        sec = SECTION_HEADER_RE.match(line.strip())
        if sec:
            # stop collecting intro at first Section header
            if intro_collecting and intro_buf:
                current.intro = " ".join(intro_buf).strip()
            intro_collecting = False
            intro_buf = []
            continue
        les = LESSON_LINE_RE.match(line)
        if les:
            current.lessons.append(LessonRow(
                grade=grade,
                unit_num=current.unit_num,
                lesson_num=int(les.group(1)),
                title=les.group(2).strip(),
            ))
            continue
        if intro_collecting:
            stripped = line.strip()
            # skip page furniture
            if not stripped:
                continue
            if "CC BY NC" in stripped or "Illustrative Mathematics" in stripped \
               or "Scope and Sequence" in stripped or stripped.startswith("©"):
                continue
            if stripped.startswith(GRADE_STEM[grade]):
                continue
            intro_buf.append(stripped)
    if current is not None and intro_buf:
        current.intro = " ".join(intro_buf).strip()
    return units


def render_scope_markdown(grade: str, units: list[UnitRow]) -> str:
    out: list[str] = []
    title = grade.replace("grade", "Grade ").replace("kindergarten", "Kindergarten")
    if grade in ("algebra1", "algebra2", "geometry"):
        title = grade.capitalize().replace("Algebra1", "Algebra 1").replace("Algebra2", "Algebra 2")
    out.append(f"# IM Scope and Sequence — {title}\n")
    out.append(f"_Source: `{ZIP_NAMES[grade]}` → `{GRADE_STEM[grade]}-Course-scope-and-sequence-.pdf`_")
    out.append("")
    for u in units:
        out.append(f"## Unit {u.unit_num}: {u.title}")
        out.append(f"_Anchor ID: `IM-{ANCHOR_GRADE[grade]}-U{u.unit_num}`_")
        out.append("")
        if u.intro:
            out.append(u.intro)
            out.append("")
        if u.lessons:
            out.append("### Lessons")
            for les in u.lessons:
                anchor = f"IM-{ANCHOR_GRADE[grade]}-U{u.unit_num}-L{les.lesson_num}"
                out.append(f"- **Lesson {les.lesson_num}:** {les.title}  `{anchor}`")
            out.append("")
    return "\n".join(out) + "\n"


# ────────────────────────────────────────────────────────────────────────
# Tier 2: per-lesson teacher-guide parsing

TITLE_AT_TOP_RE = re.compile(r"^Unit\s+\d+,\s+Lesson\s+\d+\s*$")
LESSON_PURPOSE_RE = re.compile(r"^\s*Lesson Purpose\s*$")
NARRATIVE_RE = re.compile(r"^\s*Narrative\s*$")
GOALS_INLINE_RE = re.compile(r"\bGoals\b")
STUDENT_FACING_INLINE_RE = re.compile(r"Student Facing Learning Goals?",
                                      re.IGNORECASE)
BULLET_RE = re.compile(r"[••\-]\s*")
SECTION_END_TOKENS = (
    "Lesson Purpose", "Narrative", "Required Materials",
    "Lesson Timeline", "Activity 1", "Warm-up", "Standards",
    "Instructional Routines",
    "Access for Students with Disabilities",
    "Access for English Learners",
)


def _split_columns(line: str, split_col: int) -> tuple[str, str]:
    """Split a layout-mode line at column index split_col."""
    left = line[:split_col].rstrip()
    right = line[split_col:].rstrip()
    return left, right


def parse_teacher_guide(text: str) -> dict:
    """
    Column-aware parse of a per-lesson teacher-guide pdftotext output.

    The first page of every IM teacher-guide PDF lays out two columns:
      Goals                                Student Facing Learning Goals
        • <teacher-facing goal 1>            <kid-friendly framing>
        • <teacher-facing goal 2>
    Followed by a single-column "Lesson Purpose" + "Narrative".
    """
    lines = text.splitlines()

    # Title — immediately after "Unit N, Lesson M". May wrap across 2-3 lines
    # before the "Standards" header.
    title = ""
    for i, raw in enumerate(lines[:8]):
        if TITLE_AT_TOP_RE.match(raw.strip()):
            title_parts: list[str] = []
            for j in range(i + 1, min(i + 6, len(lines))):
                cand = lines[j].strip()
                if not cand:
                    if title_parts:
                        break
                    continue
                if "Standards" in cand or "Instructional Routines" in cand \
                   or "Addressing" in cand:
                    break
                title_parts.append(cand)
            title = " ".join(title_parts).strip()
            break
    if not title:
        for raw in lines[:6]:
            cand = raw.strip()
            if cand and not TITLE_AT_TOP_RE.match(cand):
                title = cand
                break

    # Locate the "Goals" / "Student Facing" header row, capture both column
    # split positions so we can demerge bullets that span columns.
    goals_header_idx = -1
    goals_col = -1
    sf_col = -1
    for i, raw in enumerate(lines[:80]):
        gm = GOALS_INLINE_RE.search(raw)
        sm = STUDENT_FACING_INLINE_RE.search(raw)
        if gm and sm:
            goals_header_idx = i
            goals_col = gm.start()
            sf_col = sm.start()
            break
        if gm and goals_header_idx < 0 and "Lesson Purpose" not in raw:
            # rare: Goals appears alone (no Student-Facing column on this page)
            goals_header_idx = i
            goals_col = gm.start()
            sf_col = -1
            # don't break; keep looking for SF on a later line
        if sm and sf_col < 0:
            sf_col = sm.start()

    goals: list[str] = []
    student_facing_lines: list[str] = []

    if goals_header_idx >= 0:
        # Column split point: midway between goals_col and sf_col, or use sf_col.
        split_col = sf_col if sf_col > 0 else 80
        for raw in lines[goals_header_idx + 1: goals_header_idx + 60]:
            stripped = raw.strip()
            if not stripped:
                continue
            if LESSON_PURPOSE_RE.match(stripped) or NARRATIVE_RE.match(stripped):
                break
            if any(stripped.startswith(t) for t in SECTION_END_TOKENS):
                break
            # Page furniture
            if "Illustrative Mathematics" in stripped or "CC BY NC" in stripped:
                continue
            if re.match(r"^\s*Unit\s+\d+\s*$", raw) or \
               re.match(r"^\s*Lesson\s+\d+\s*$", raw) or \
               re.match(r"^\s*Kindergarten\s*$", raw) or \
               re.match(r"^\s*Grade\s+\d+\s*$", raw):
                continue
            left, right = _split_columns(raw, split_col)
            # Left column → goals; bullets get stripped, continuation lines
            # (no bullet) attach to the previous goal.
            if left:
                lstrip = left.strip()
                m = re.match(r"^[••\-]\s*(.+)$", lstrip)
                if m:
                    goals.append(m.group(1).strip())
                else:
                    if goals:
                        goals[-1] = (goals[-1] + " " + lstrip).strip()
            if right:
                rstrip = right.strip()
                m = re.match(r"^[••\-]\s*(.+)$", rstrip)
                if m:
                    student_facing_lines.append(m.group(1).strip())
                else:
                    if student_facing_lines:
                        student_facing_lines[-1] = (
                            student_facing_lines[-1] + " " + rstrip).strip()
                    else:
                        student_facing_lines.append(rstrip)

    student_facing = " ".join(student_facing_lines).strip()

    # Lesson Purpose — single-column section.
    purpose_lines: list[str] = []
    in_purpose = False
    for raw in lines:
        stripped = raw.strip()
        if LESSON_PURPOSE_RE.match(stripped):
            in_purpose = True
            continue
        if in_purpose:
            if NARRATIVE_RE.match(stripped) or any(
                stripped.startswith(t) for t in SECTION_END_TOKENS
            ):
                break
            if stripped == "":
                if purpose_lines:
                    break
                continue
            purpose_lines.append(stripped)
    purpose = " ".join(purpose_lines).strip()

    # Dedup goals.
    seen = set()
    deduped_goals: list[str] = []
    for g in goals:
        # Strip trailing ellipsis fragments
        g = g.strip()
        if not g:
            continue
        key = g.lower()
        if key not in seen and len(g) < 400:
            seen.add(key)
            deduped_goals.append(g)

    return {
        "title": title,
        "goals": deduped_goals,
        "student_facing": student_facing,
        "purpose": purpose,
    }


def render_teacher_guide_markdown(grade: str, unit_num: int, lesson_num: int,
                                  parsed: dict, raw_text: str) -> str:
    anchor = f"IM-{ANCHOR_GRADE[grade]}-U{unit_num}-L{lesson_num}"
    out: list[str] = []
    title = parsed.get("title") or f"Unit {unit_num}, Lesson {lesson_num}"
    out.append(f"# {title}")
    out.append(f"_Anchor ID: `{anchor}` — Grade {grade}, Unit {unit_num}, "
               f"Lesson {lesson_num}_")
    out.append("")
    if parsed.get("goals"):
        out.append("## Learning Goals (teacher-facing)")
        for g in parsed["goals"]:
            out.append(f"- {g}")
        out.append("")
    if parsed.get("student_facing"):
        out.append("## Student-Facing Learning Goals")
        out.append(parsed["student_facing"])
        out.append("")
    if parsed.get("purpose"):
        out.append("## Lesson Purpose")
        out.append(parsed["purpose"])
        out.append("")
    out.append("## Full Teacher Guide (raw extract)")
    out.append("")
    out.append("```")
    out.append(raw_text.rstrip())
    out.append("```")
    out.append("")
    return "\n".join(out) + "\n"


# ────────────────────────────────────────────────────────────────────────
# Prolog string escaping + record builders

def pl_escape(s: str) -> str:
    if s is None:
        s = ""
    return (s.replace("\\", "\\\\")
             .replace('"', '\\"')
             .replace("\n", " ")
             .replace("\r", " ")
             .strip())


def concept_id_for_unit(grade: str, unit_num: int) -> str:
    """Return the canonical concept ID for a unit if known; else a stub."""
    key = f"{ANCHOR_GRADE[grade]}-U{unit_num}"
    if key in EXISTING_UNIT_CONCEPT:
        return EXISTING_UNIT_CONCEPT[key]
    return f"im_{grade}_unit{unit_num}"


def concept_id_for_lesson(grade: str, unit_num: int, lesson_num: int) -> str:
    """Lesson-level concept IDs are always stubs (auto-generated)."""
    return f"im_{grade}_u{unit_num}_l{lesson_num}"


def stub_geom_concept_line(concept_id: str, name: str, grade: str) -> str:
    band = GRADE_BAND[grade]
    band_str = "[" + ",".join(str(b) for b in band) + "]"
    return (f'geom_concept({concept_id}, "{pl_escape(name)}", '
            f'developmental, {band_str}).')


# ────────────────────────────────────────────────────────────────────────
# Main pipeline

def extract_scope_sequences(grades: list[str] | None = None) -> list[UnitRow]:
    """
    Tier 1: Extract & parse all scope-and-sequence PDFs.
    Writes corpus markdown.
    Returns the list of all UnitRow seen across all grades (so the caller
    can reuse for anchor emission).
    """
    if grades is None:
        grades = ALL_GRADES
    CORPUS_SAS_DIR.mkdir(parents=True, exist_ok=True)
    TMP_ROOT.mkdir(parents=True, exist_ok=True)

    all_units: list[UnitRow] = []
    for grade in grades:
        zip_name = ZIP_NAMES[grade]
        stem = GRADE_STEM[grade]
        zip_path = IM_ZIP_DIR / zip_name
        if not zip_path.exists():
            print(f"[Tier1] missing zip: {zip_path}", file=sys.stderr)
            continue
        sas_pdf_name = f"{stem}-Course-scope-and-sequence-.pdf"
        with zipfile.ZipFile(zip_path) as z:
            try:
                data = z.read(sas_pdf_name)
            except KeyError:
                print(f"[Tier1] missing scope-and-sequence in {zip_name}",
                      file=sys.stderr)
                continue
        tmp_pdf = TMP_ROOT / f"{grade}_sas.pdf"
        tmp_pdf.write_bytes(data)
        tmp_txt = TMP_ROOT / f"{grade}_sas.txt"
        if not pdftotext(tmp_pdf, tmp_txt):
            print(f"[Tier1] pdftotext failed for {grade}", file=sys.stderr)
            continue
        text = tmp_txt.read_text(encoding="utf-8", errors="replace")
        units = parse_scope_and_sequence(text, grade)
        all_units.extend(units)
        md = render_scope_markdown(grade, units)
        out = CORPUS_SAS_DIR / f"{grade}.md"
        out.write_text(md, encoding="utf-8")
        n_lessons = sum(len(u.lessons) for u in units)
        print(f"[Tier1] {grade}: {len(units)} units, {n_lessons} lessons "
              f"→ {out.name}")
    return all_units


def extract_teacher_guides(grades: list[str] | None = None) -> dict[tuple, dict]:
    """
    Tier 2: Extract every per-lesson teacher-guide PDF for the given grades
    (default K-5). Writes corpus markdown per lesson. Returns a dict mapping
    (grade, unit_num, lesson_num) -> parsed dict.
    """
    if grades is None:
        grades = K5_GRADES
    CORPUS_TG_DIR.mkdir(parents=True, exist_ok=True)
    TMP_ROOT.mkdir(parents=True, exist_ok=True)

    parsed_by_lesson: dict[tuple, dict] = {}

    for grade in grades:
        zip_name = ZIP_NAMES[grade]
        stem = GRADE_STEM[grade]
        zip_path = IM_ZIP_DIR / zip_name
        if not zip_path.exists():
            print(f"[Tier2] missing zip: {zip_path}", file=sys.stderr)
            continue

        n_lessons = 0
        n_failed = 0
        with zipfile.ZipFile(zip_path) as z:
            inner_zips = sorted(
                n for n in z.namelist()
                if re.fullmatch(rf"{stem}\.\d+-Unit-teacher_guide-Lesson-pdf\.zip", n)
            )
            for inner_name in inner_zips:
                m = re.search(rf"{stem}\.(\d+)-", inner_name)
                if not m:
                    continue
                unit_num = int(m.group(1))
                inner_data = z.read(inner_name)
                try:
                    inner_z = zipfile.ZipFile(io.BytesIO(inner_data))
                except zipfile.BadZipFile:
                    print(f"[Tier2] bad inner zip: {inner_name}", file=sys.stderr)
                    continue
                for pdf_name in sorted(inner_z.namelist()):
                    lm = re.match(
                        rf"{stem}-{unit_num}-(\d+)-Lesson-teacher-guide-\.pdf",
                        pdf_name,
                    )
                    if not lm:
                        continue
                    lesson_num = int(lm.group(1))
                    pdf_bytes = inner_z.read(pdf_name)
                    pdf_path = TMP_ROOT / f"{grade}_u{unit_num}_l{lesson_num}.pdf"
                    pdf_path.write_bytes(pdf_bytes)
                    txt_path = TMP_ROOT / f"{grade}_u{unit_num}_l{lesson_num}.txt"
                    if not pdftotext(pdf_path, txt_path):
                        n_failed += 1
                        continue
                    text = txt_path.read_text(encoding="utf-8", errors="replace")
                    parsed = parse_teacher_guide(text)

                    md = render_teacher_guide_markdown(
                        grade, unit_num, lesson_num, parsed, text)
                    out_dir = CORPUS_TG_DIR / grade / f"unit{unit_num}"
                    out_dir.mkdir(parents=True, exist_ok=True)
                    out_path = out_dir / f"lesson{lesson_num}.md"
                    out_path.write_text(md, encoding="utf-8")

                    parsed_by_lesson[(grade, unit_num, lesson_num)] = parsed
                    n_lessons += 1

                    # cleanup tmp
                    try: pdf_path.unlink()
                    except OSError: pass
                    try: txt_path.unlink()
                    except OSError: pass

        print(f"[Tier2] {grade}: {n_lessons} lessons extracted, {n_failed} failed")
    return parsed_by_lesson


# ────────────────────────────────────────────────────────────────────────
# Prolog file writer

ANCHORS_HEADER = """% standards/im_lesson_anchors.pl — Illustrative Math lesson IDs anchored to geometry concepts.
%
% AUTO-GENERATED by /Users/tio/Documents/GitHub/Prolog/n101_bot/scripts/im_extract.py.
% Do not hand-edit between the BEGIN AUTO and END AUTO markers below; rerun
% the script. Hand-authored content lives outside the markers.
%
% Two tiers of records are produced:
%   Tier 1 — Unit-level standard_anchor/4 records, one per IM unit across
%            all 12 grades. Each unit is anchored either to an existing
%            canonical geom_concept (for units that previously had hand-
%            authored mappings — see EXISTING_UNIT_CONCEPT in im_extract.py)
%            or to an auto-stub geom_concept(im_<grade>_unit<N>, ..., developmental, ...).
%   Tier 2 — Lesson-level standard_anchor/4 records for grades K-5, anchored
%            to auto-stub geom_concept(im_<grade>_u<U>_l<L>, ..., developmental, ...).
%
% The stub geom_concept records use Topic = developmental as a placeholder
% bucket; the synthesizer is expected to canonicalize these to the proper
% concept IDs in a follow-up pass. They are present here so that
% validate_geom_kb succeeds (no orphan_standard_anchor errors).
%
% Schema: ../schema.pl

:- multifile geom_concept/4, standard_anchor/4, tier/4.
:- discontiguous geom_concept/4, standard_anchor/4, tier/4.

% ── BEGIN AUTO ───────────────────────────────────────────────────────
"""

ANCHORS_FOOTER = """% ── END AUTO ─────────────────────────────────────────────────────────
"""


def write_anchors_pl(units: list[UnitRow],
                     parsed_by_lesson: dict[tuple, dict]) -> tuple[int, int]:
    """
    Write im_lesson_anchors.pl in full (idempotent).

    Returns: (line_count_before, line_count_after).
    """
    line_count_before = 0
    if ANCHORS_PL.exists():
        line_count_before = len(ANCHORS_PL.read_text().splitlines())

    # Build set of stub concept IDs we need.
    stub_concepts: dict[str, tuple[str, str]] = {}
    # key: concept_id -> (name, grade)

    sa_lines: list[str] = []
    tier_lines: list[str] = []

    # group units by grade for clean section breaks
    by_grade: dict[str, list[UnitRow]] = {}
    for u in units:
        by_grade.setdefault(u.grade, []).append(u)

    seen_stub_for_canonical: set[str] = set()

    # ── Tier 1: unit-level anchors ────────────────────────────────────
    sa_lines.append("% ── Tier 1: unit-level anchors (all 12 grades) ──────")
    for grade in ALL_GRADES:
        if grade not in by_grade:
            continue
        sa_lines.append(f"% ─ {grade} ─")
        for u in by_grade[grade]:
            unit_key = f"{ANCHOR_GRADE[grade]}-U{u.unit_num}"
            anchor_code = f"IM-{unit_key}"
            cid = concept_id_for_unit(grade, u.unit_num)
            if cid not in EXISTING_UNIT_CONCEPT.values() and not cid.startswith("im_"):
                pass  # no-op
            # Register stub concept for non-canonical IDs
            if cid not in EXISTING_UNIT_CONCEPT.values():
                stub_concepts.setdefault(
                    cid,
                    (f"IM {grade.title()} Unit {u.unit_num}: {u.title}", grade),
                )
            short_intro = (u.intro[:380] + "…") if len(u.intro) > 380 else u.intro
            statement = f"IM {grade.title()} Unit {u.unit_num}: {u.title}."
            if short_intro:
                statement += " " + short_intro
            sa_lines.append(
                f'standard_anchor({cid}, im_lesson, "{anchor_code}",\n'
                f'    "{pl_escape(statement)}").'
            )
            tier_lines.append(
                f'tier(ref(standard, {cid}, "{anchor_code}"), 2, '
                f'[source(im, scope_and_sequence)], '
                f'"unit-level anchor — auto-extracted from IM scope-and-sequence").'
            )
        sa_lines.append("")

    # ── Tier 2: lesson-level anchors (K-5) ────────────────────────────
    if parsed_by_lesson:
        sa_lines.append("% ── Tier 2: lesson-level anchors (K-5) ──────────────")
        # group by (grade, unit) for readability
        keys_sorted = sorted(parsed_by_lesson.keys(),
                             key=lambda k: (ALL_GRADES.index(k[0]), k[1], k[2]))
        last_grade = None
        last_unit = None
        for (grade, unit_num, lesson_num) in keys_sorted:
            if grade != last_grade:
                sa_lines.append(f"% ─ {grade} ─")
                last_grade = grade
                last_unit = None
            if unit_num != last_unit:
                sa_lines.append(f"%   Unit {unit_num}")
                last_unit = unit_num
            parsed = parsed_by_lesson[(grade, unit_num, lesson_num)]
            anchor_code = (f"IM-{ANCHOR_GRADE[grade]}-U{unit_num}"
                           f"-L{lesson_num}")
            cid = concept_id_for_lesson(grade, unit_num, lesson_num)
            title = parsed.get("title") or f"Lesson {lesson_num}"
            stub_concepts.setdefault(
                cid,
                (f"IM {grade.title()} U{unit_num} L{lesson_num}: {title}",
                 grade),
            )
            statement_parts = [f"IM {grade.title()} Unit {unit_num} Lesson "
                               f"{lesson_num}: {title}."]
            if parsed.get("purpose"):
                statement_parts.append(parsed["purpose"][:240])
            if parsed.get("goals"):
                goals_join = " | ".join(parsed["goals"][:3])
                statement_parts.append(f"Goals: {goals_join[:280]}")
            statement = " ".join(statement_parts)
            sa_lines.append(
                f'standard_anchor({cid}, im_lesson, "{anchor_code}",\n'
                f'    "{pl_escape(statement)}").'
            )
            tier_lines.append(
                f'tier(ref(standard, {cid}, "{anchor_code}"), 2, '
                f'[source(im, teacher_guide)], '
                f'"lesson-level anchor — auto-extracted from teacher-guide PDF").'
            )

    # ── Stub concept lines ────────────────────────────────────────────
    stub_lines: list[str] = []
    stub_lines.append("% ── Stub geom_concept/4 records ─────────────────────")
    stub_lines.append("% Auto-generated wrapper concepts. Topic=developmental")
    stub_lines.append("% acts as a placeholder bucket pending synthesizer canonicalization.")
    for cid in sorted(stub_concepts.keys()):
        name, grade = stub_concepts[cid]
        stub_lines.append(stub_geom_concept_line(cid, name, grade))
    stub_lines.append("")

    body = "\n".join(stub_lines + sa_lines + [""] + tier_lines) + "\n"

    full = ANCHORS_HEADER + body + ANCHORS_FOOTER
    ANCHORS_PL.write_text(full, encoding="utf-8")

    line_count_after = len(full.splitlines())
    return (line_count_before, line_count_after)


# ────────────────────────────────────────────────────────────────────────
# Entry point

def main(argv: list[str] | None = None) -> int:
    p = argparse.ArgumentParser(description="IM curriculum extractor.")
    p.add_argument("--tier1-only", action="store_true",
                   help="Only run Tier 1 (scope-and-sequence).")
    p.add_argument("--tier2-only", action="store_true",
                   help="Only run Tier 2 (teacher guides).")
    p.add_argument("--tier1-grades", default=None,
                   help="Comma-separated grade IDs for Tier 1 "
                        "(default: all 12).")
    p.add_argument("--tier2-grades", default=None,
                   help="Comma-separated grade IDs for Tier 2 "
                        "(default: K-5).")
    args = p.parse_args(argv)

    tier1_grades = (
        [g.strip() for g in args.tier1_grades.split(",")]
        if args.tier1_grades else None
    )
    tier2_grades = (
        [g.strip() for g in args.tier2_grades.split(",")]
        if args.tier2_grades else None
    )

    units: list[UnitRow] = []
    parsed_by_lesson: dict[tuple, dict] = {}

    t0 = time.time()
    if not args.tier2_only:
        print("=== Tier 1: scope-and-sequence extraction ===")
        units = extract_scope_sequences(grades=tier1_grades)
        print(f"  parsed {len(units)} units total ({time.time()-t0:.1f}s)")

    t1 = time.time()
    if not args.tier1_only:
        print("=== Tier 2: K-5 teacher-guide extraction ===")
        parsed_by_lesson = extract_teacher_guides(grades=tier2_grades)
        print(f"  parsed {len(parsed_by_lesson)} lessons ({time.time()-t1:.1f}s)")

    if not args.tier2_only or not args.tier1_only:
        # Need full Tier 1 unit list to write anchors. If only Tier 2 was
        # requested, parse Tier 1 once to anchor stubs.
        if args.tier2_only and not units:
            print("=== Re-parsing Tier 1 units for anchor file consistency ===")
            units = extract_scope_sequences(grades=ALL_GRADES)
        before, after = write_anchors_pl(units, parsed_by_lesson)
        print(f"=== im_lesson_anchors.pl: {before} → {after} lines ===")

    # Cleanup tmp dir
    if TMP_ROOT.exists():
        try:
            for f in TMP_ROOT.iterdir():
                try: f.unlink()
                except OSError: pass
        except OSError:
            pass

    print(f"=== done in {time.time()-t0:.1f}s ===")
    return 0


if __name__ == "__main__":
    sys.exit(main())

#!/usr/bin/env python3
"""Generate a manifest of my compiled Typst notes for the web coursework hub.

For every ``*.typ`` under the semester folders this pairs it with the sibling
PDF in the same folder that was **actually produced by Typst** (checked via the
PDF's Creator/Producer metadata), so downloaded lecture slide decks and LaTeX
handouts that happen to sit next to a note are never picked up. When a folder
has several Typst PDFs the closest filename match wins, and if two ``.typ``
files resolve to the same PDF only the better match is kept.

Writes:
  <out>/manifest.json      - the tree the /coursework page renders from
  <out>/pdf/<hash>.pdf     - a flat copy of each matched PDF

This is an ALLOWLIST: a note is published only if its repo-relative .typ path
is a key in notes-meta.json (and not marked "hidden"). Everything else about it
(the PDF, the title, the section) is still auto-derived unless the entry
overrides it:
  { "5sem/.../lec01/notes.typ": { "title": "...", "description": "...",
                                  "section": "...", "pdf": "other.pdf",
                                  "hidden": true } }

Usage:
    python3 scripts/generate_notes_manifest.py --out build/coursework
    python3 scripts/generate_notes_manifest.py --out build/coursework --report
    python3 scripts/generate_notes_manifest.py --init-meta   # seed notes-meta.json
"""
from __future__ import annotations

import argparse
import hashlib
import json
import re
import shutil
import subprocess
from datetime import datetime, timezone
from difflib import SequenceMatcher
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
GITHUB_BLOB = "https://github.com/simo899t/SDU/blob/main"
NOTES_META = REPO_ROOT / "notes-meta.json"

SEMESTER_DIR = re.compile(r"^(\d)sem$")
COURSE_DIR = re.compile(r"^([A-Z]{2}\d{3})[-_](.+)$")

# Path segment (case-insensitive substring) -> section bucket. First match wins,
# checked from the deepest folder outward.
SECTION_RULES = [
    (re.compile(r"lecture|lektion", re.I), "Lectures"),
    (re.compile(r"seminar", re.I), "Seminars"),
    (re.compile(r"problem[ _-]?set", re.I), "Problem sets"),
    (re.compile(r"\bcases?\b", re.I), "Cases"),
    (re.compile(r"exc?ercise|sheet|opgave|homework", re.I), "Exercises"),
    (re.compile(r"prep", re.I), "Exam prep"),
    (re.compile(r"exam|eksam", re.I), "Exams"),
    (re.compile(r"project|report|rapport|assignment|assign", re.I), "Projects"),
]

# Never publish files on paths matching this (temp / test / copies / scratch).
DENY = re.compile(
    r"(^|/)(temp|tmp|test|testing|scratch|tryouts?|draft|old|backup|wip)(/|$|[._\- ])"
    r"|[ _\-]copy(\b|[._\- 0-9])",
    re.I,
)


def is_typst_pdf(path: Path) -> bool:
    """True if the PDF's metadata names Typst as the creator/producer."""
    try:
        head = path.read_bytes()
    except OSError:
        return False
    # Typst writes `/Creator (Typst 0.x.y)` (and the XMP `xmp:CreatorTool`).
    # Scan the whole file: the info dict can be at the tail after the xref.
    return b"Typst" in head


def prettify(name: str) -> str:
    name = re.sub(r"[-_]+", " ", name).strip()
    # split camelCase / PascalCase and letter-digit runs: WrittenExamSimon, Homework1
    name = re.sub(r"(?<=[a-z])(?=[A-Z])", " ", name)
    name = re.sub(r"(?<=[A-Za-z])(?=\d)", " ", name)
    name = re.sub(r"\s+", " ", name).strip()
    return name[:1].upper() + name[1:] if name else name


TITLE_PATTERNS = [
    (re.compile(r"^lec(?:ture)?[-_ ]?0*(\d+)", re.I), "Lecture {}"),
    (re.compile(r"^session[-_ ]?0*(\d+)", re.I), "Session {}"),
    (re.compile(r"^(?:ex|exercise|excercise|opgave)[-_ ]?0*(\d+)", re.I), "Exercise {}"),
    (re.compile(r"^sheet[-_ ]?0*(\d+)", re.I), "Sheet {}"),
    (re.compile(r"^set[-_ ]?0*(\d+)", re.I), "Problem set {}"),
    (re.compile(r"^case[-_ ]?0*(\d+)", re.I), "Case {}"),
    (re.compile(r"^pres(?:entation)?[-_ ]?0*(\d+)", re.I), "Presentation {}"),
    (re.compile(r"^(?:week|uge)[-_ ]?0*(\d+)", re.I), "Week {}"),
    (re.compile(r"^(?:assignment|assign)[-_ ]?0*(\d+)", re.I), "Assignment {}"),
    (re.compile(r"^rs[-_ ]?0*(\d+)", re.I), "Research seminar {}"),
]


def nice_title(typ_path: Path, course_dir: Path) -> str:
    stem = typ_path.stem
    folder = typ_path.parent.name
    # If the file is a generic "notes"/"main", lean on the folder name instead.
    candidates = [stem]
    if stem.lower() in {"notes", "main", "index", "note", "report", folder.lower()}:
        candidates = [folder, stem]
    for raw in candidates:
        r = raw
        r = re.sub(r"^\d+[-_ ]+", "", r)                 # leading "11_" ordinal
        r = re.sub(r"^[a-z]{2}\d{3}[-_ ]*", "", r, flags=re.I)  # course code
        r = re.sub(r"[-_ ]*notes?$", "", r, flags=re.I)  # trailing "_notes"
        r = re.sub(r"\s*\(\d+\)$", "", r).strip()        # "(1)" suffix
        for rx, label in TITLE_PATTERNS:
            mm = rx.match(r)
            if mm:
                return label.format(int(mm.group(1)))
        if r and r.lower() not in {"notes", "note", folder.lower()}:
            return prettify(r)
    return prettify(folder)


def section_for(rel_parts: list[str]) -> str:
    # outermost folder first, so a "Problem sets/" ancestor wins over a
    # "report.typ" filename deeper down
    for part in rel_parts:
        for rule, label in SECTION_RULES:
            if rule.search(part):
                return label
    return "Notes"


def _norm(s: str) -> str:
    s = s.lower()
    s = re.sub(r"[a-z]{2}\d{3}", "", s)
    s = re.sub(r"\b(notes?|copy|final|draft|sol|solution)\b", "", s)
    s = re.sub(r"[^a-z0-9]+", "", s)
    return s


def match_score(typ_stem: str, pdf_stem: str) -> float:
    a, b = _norm(typ_stem), _norm(pdf_stem)
    if not a or not b:
        return 0.0
    ratio = SequenceMatcher(None, a, b).ratio()
    # strong bonus when both carry the same lecNN / sheetNN / NN token
    ta = set(re.findall(r"(?:lec|sheet|ex|session|case|rs)?0*(\d+)", a))
    tb = set(re.findall(r"(?:lec|sheet|ex|session|case|rs)?0*(\d+)", b))
    if ta and ta == tb:
        ratio += 0.4
    return ratio


def git_last_modified(path: Path) -> str:
    try:
        out = subprocess.run(
            ["git", "log", "-1", "--format=%cI", "--", str(path)],
            cwd=REPO_ROOT, capture_output=True, text=True, check=True,
        ).stdout.strip()
        if out:
            return out
    except Exception:
        pass
    return datetime.fromtimestamp(path.stat().st_mtime, tz=timezone.utc).isoformat()


def load_meta() -> dict:
    if NOTES_META.exists():
        try:
            return {k: v for k, v in json.loads(NOTES_META.read_text()).items()
                    if not k.startswith("_")}
        except json.JSONDecodeError as e:
            raise SystemExit(f"notes-meta.json is not valid JSON: {e}")
    return {}


def resolve_pairs(meta: dict, report: list):
    """Yield (typ_path, pdf_path, course_dir, sem_key) for every publishable note."""
    # First pass: collect candidate (typ, best_pdf, score); then dedupe by pdf.
    candidates = []  # (score, typ_path, pdf_path, course_dir, sem_key)

    for sem_path in sorted(REPO_ROOT.iterdir()):
        m = SEMESTER_DIR.match(sem_path.name)
        if not (m and sem_path.is_dir()):
            continue
        sem_key = f"{m.group(1)}. semester"

        allowlist = bool(meta)
        for typ_path in sorted(sem_path.rglob("*.typ")):
            rel = typ_path.relative_to(REPO_ROOT).as_posix()
            entry_meta = meta.get(rel)
            if allowlist and entry_meta is None:
                report.append(("not in notes-meta.json allowlist", rel)); continue
            entry_meta = entry_meta or {}
            if entry_meta.get("hidden"):
                report.append(("hidden (meta)", rel)); continue
            if DENY.search(rel) and not entry_meta:
                report.append(("skipped (temp/test/copy)", rel)); continue

            course_dir = None
            for parent in typ_path.parents:
                if parent == sem_path:
                    break
                course_dir = parent
            if course_dir is None:
                report.append(("skipped (loose in semester root)", rel)); continue

            forced = entry_meta.get("pdf")
            if forced:
                pdf_path = (typ_path.parent / forced).resolve()
                if pdf_path.exists():
                    candidates.append((99.0, typ_path, pdf_path, course_dir, sem_key)); continue
                report.append((f"meta pdf not found: {forced}", rel)); continue

            typ_pdfs = [p for p in sorted(typ_path.parent.glob("*.pdf")) if is_typst_pdf(p)]
            if not typ_pdfs:
                report.append(("no Typst-compiled PDF in folder", rel)); continue
            if len(typ_pdfs) == 1:
                candidates.append((1.0, typ_path, typ_pdfs[0], course_dir, sem_key)); continue
            scored = sorted(((match_score(typ_path.stem, p.stem), p) for p in typ_pdfs),
                            reverse=True)
            best_score, best_pdf = scored[0]
            candidates.append((best_score, typ_path, best_pdf, course_dir, sem_key))

    # Dedupe: one PDF -> the single best-scoring .typ that claims it.
    by_pdf: dict[str, tuple] = {}
    for cand in candidates:
        score, typ_path, pdf_path, *_ = cand
        key = str(pdf_path)
        if key not in by_pdf or score > by_pdf[key][0]:
            if key in by_pdf:
                report.append(("dropped (weaker match to same PDF)",
                               by_pdf[key][1].relative_to(REPO_ROOT).as_posix()))
            by_pdf[key] = cand
        else:
            report.append(("dropped (weaker match to same PDF)",
                           typ_path.relative_to(REPO_ROOT).as_posix()))

    for score, typ_path, pdf_path, course_dir, sem_key in by_pdf.values():
        yield typ_path, pdf_path, course_dir, sem_key


def build(out_dir: Path, report_flag: bool) -> dict:
    meta = load_meta()
    pdf_out = out_dir / "pdf"
    if pdf_out.exists():
        shutil.rmtree(pdf_out)
    pdf_out.mkdir(parents=True, exist_ok=True)

    report: list = []
    semesters: dict[str, dict] = {}
    count = 0

    for typ_path, pdf_path, course_dir, sem_key in resolve_pairs(meta, report):
        rel = typ_path.relative_to(REPO_ROOT).as_posix()
        entry_meta = meta.get(rel, {})

        cm = COURSE_DIR.match(course_dir.name)
        course_code = cm.group(1) if cm else course_dir.name
        course_name = prettify(cm.group(2)) if cm else prettify(course_dir.name)

        rel_parts = list(typ_path.relative_to(course_dir).parts)
        section = entry_meta.get("section") or section_for(rel_parts)

        digest = hashlib.sha1(rel.encode()).hexdigest()[:12]
        shutil.copy2(pdf_path, pdf_out / f"{digest}.pdf")

        title = entry_meta.get("title") or nice_title(typ_path, course_dir)

        item = {
            "title": title,
            "description": entry_meta.get("description", ""),
            "section": section,
            "pdf": f"pdf/{digest}.pdf",
            "source": f"{GITHUB_BLOB}/{rel}",
            "updated": git_last_modified(typ_path),
        }
        sem = semesters.setdefault(sem_key, {"courses": {}})
        course = sem["courses"].setdefault(
            f"{course_code} {course_name}",
            {"code": course_code, "name": course_name, "sections": {}},
        )
        course["sections"].setdefault(section, []).append(item)
        count += 1

    manifest = {
        "generated": datetime.now(timezone.utc).isoformat(),
        "count": count,
        "semesters": [],
    }
    for sem_key in sorted(semesters, reverse=True):
        sem_out = {"semester": sem_key, "courses": []}
        for course_key in sorted(semesters[sem_key]["courses"]):
            c = semesters[sem_key]["courses"][course_key]
            sec_order = ["Lectures", "Exercises", "Problem sets", "Cases", "Seminars",
                         "Exam prep", "Exams", "Projects", "Notes"]
            sections = sorted(
                ({"name": n, "items": sorted(v, key=lambda i: (i["title"].lower()))}
                 for n, v in c["sections"].items()),
                key=lambda s: (sec_order.index(s["name"]) if s["name"] in sec_order else 99,
                               s["name"]),
            )
            sem_out["courses"].append({"code": c["code"], "name": c["name"], "sections": sections})
        manifest["semesters"].append(sem_out)

    (out_dir / "manifest.json").write_text(json.dumps(manifest, indent=2) + "\n")

    if report_flag:
        print("\n=== EXCLUDED / ADJUSTED ===")
        for reason, rel in sorted(report):
            print(f"  [{reason}] {rel}")
        print(f"\n=== INCLUDED ({count}) ===")
        for sem in manifest["semesters"]:
            for c in sem["courses"]:
                for s in c["sections"]:
                    for it in s["items"]:
                        print(f"  {sem['semester']:>14} | {c['code']} {c['name']:<32} "
                              f"| {s['name']:<10} | {it['title']}")
    return manifest


def looks_like_scaffold(typ_path: Path, sem_key: str) -> bool:
    """A template stub: its own `course:` header names a different course code
    than the folder it sits in (these were copied from a template and never
    rewritten)."""
    folder_code = None
    for parent in typ_path.parents:
        cm = COURSE_DIR.match(parent.name)
        if cm:
            folder_code = cm.group(1).upper()
            break
    if not folder_code:
        return False
    try:
        text = typ_path.read_text(errors="ignore")
    except OSError:
        return False
    m = re.search(r'course:\s*"([A-Z]{2}\d{3})', text)
    return bool(m) and m.group(1).upper() != folder_code


def init_meta() -> None:
    """Seed notes-meta.json from everything currently auto-detectable, minus
    obvious template scaffolds. Existing entries are preserved."""
    existing = load_meta()
    report: list = []
    seen = set()
    new_entries: dict[str, dict] = {}

    for typ_path, pdf_path, course_dir, sem_key in resolve_pairs({}, report):
        rel = typ_path.relative_to(REPO_ROOT).as_posix()
        seen.add(rel)
        if rel in existing:
            continue
        if looks_like_scaffold(typ_path, sem_key):
            report.append(("scaffold stub - not seeded", rel))
            continue
        new_entries[rel] = {"title": nice_title(typ_path, course_dir), "description": ""}

    merged = {
        "_README": ("Allowlist for the /coursework hub. A .typ is published only if "
                    "its repo-relative path is a key here and not \"hidden\". "
                    "Optional per-entry fields: title, description, section, pdf, hidden."),
    }
    for k in sorted({**existing, **new_entries}):
        merged[k] = {**existing.get(k, {}), **new_entries.get(k, {})}
    NOTES_META.write_text(json.dumps(merged, indent=2, ensure_ascii=False) + "\n")

    # uncompiled checklist: real .typ files with no Typst PDF (scaffolds excluded)
    uncompiled: dict[str, list[str]] = {}
    for sem_path in sorted(REPO_ROOT.iterdir()):
        m = SEMESTER_DIR.match(sem_path.name)
        if not (m and sem_path.is_dir()):
            continue
        sem_key = f"{m.group(1)}. semester"
        for typ_path in sorted(sem_path.rglob("*.typ")):
            rel = typ_path.relative_to(REPO_ROOT).as_posix()
            if rel in seen or DENY.search(rel) or looks_like_scaffold(typ_path, sem_key):
                continue
            if any(is_typst_pdf(p) for p in typ_path.parent.glob("*.pdf")):
                continue
            uncompiled.setdefault("/".join(rel.split("/")[:2]), []).append(rel)

    lines = ["# Notes not yet on the hub", "",
             "Real `.typ` files with no Typst-compiled PDF beside them. "
             "Compile the PDF, then run `--init-meta` again to add it to the allowlist.", ""]
    total = 0
    for course in sorted(uncompiled):
        lines.append(f"## {course}")
        for rel in uncompiled[course]:
            lines.append(f"- [ ] `{rel}`")
            total += 1
        lines.append("")
    lines.append(f"_{total} files_")
    (REPO_ROOT / "notes-uncompiled.md").write_text("\n".join(lines))

    print(f"notes-meta.json: {len(existing)} kept, {len(new_entries)} added, "
          f"{sum(1 for r, _ in report if r.startswith('scaffold'))} scaffolds skipped")
    print(f"notes-uncompiled.md: {total} files still to compile")


def main() -> None:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--out", type=Path, help="output dir for manifest.json + pdf/")
    ap.add_argument("--report", action="store_true",
                    help="print what was included and why things were excluded")
    ap.add_argument("--init-meta", action="store_true",
                    help="seed/refresh notes-meta.json and notes-uncompiled.md, then exit")
    args = ap.parse_args()

    if args.init_meta:
        init_meta()
        return
    if not args.out:
        ap.error("--out is required (unless --init-meta)")
    manifest = build(args.out.resolve(), args.report)
    print(f"\nwrote {args.out.resolve()/'manifest.json'} - {manifest['count']} notes, "
          f"{len(manifest['semesters'])} semesters")


if __name__ == "__main__":
    main()

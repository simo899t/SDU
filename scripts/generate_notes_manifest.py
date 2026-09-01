#!/usr/bin/env python3
"""Generate a manifest of all compiled notes and copy their PDFs for the web hub.

Walks the semester folders, pairs every ``*.typ`` source with a sibling
``*.pdf`` of the same stem, and writes:

  <out>/manifest.json      - the tree the coursework page renders from
  <out>/pdf/<hash>.pdf     - a flat copy of each compiled PDF

Only .typ files that have a compiled sibling PDF are published, so drafts and
scratch files never leak. Descriptions come from an optional notes-meta.json
at the repo root (see NOTES_META below); everything else is derived from the
path.

Usage:
    python3 scripts/generate_notes_manifest.py --out build/coursework
    python3 scripts/generate_notes_manifest.py --out ../simo899t.github.io/public/coursework
"""
from __future__ import annotations

import argparse
import hashlib
import json
import re
import shutil
import subprocess
from datetime import datetime, timezone
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
GITHUB_BLOB = "https://github.com/simo899t/SDU/blob/main"

SEMESTER_DIR = re.compile(r"^(\d)sem$")
COURSE_DIR = re.compile(r"^([A-Z]{2}\d{3})[-_](.+)$")

# Path segments (case-insensitive substring) -> section bucket. First match wins.
SECTION_RULES = [
    (re.compile(r"lecture", re.I), "Lectures"),
    (re.compile(r"e?xercise", re.I), "Exercises"),
    (re.compile(r"exam|eksam", re.I), "Exams"),
    (re.compile(r"project|report|rapport|assignment|assign", re.I), "Projects"),
    (re.compile(r"prep", re.I), "Exam prep"),
]

# Never publish these, matched against the repo-relative POSIX path.
DENY = re.compile(r"(^|/)(temp|tmp|test|testing|scratch|tryouts?|draft)(/|$|\.)", re.I)

NOTES_META = REPO_ROOT / "notes-meta.json"


def prettify(name: str) -> str:
    name = re.sub(r"[-_]+", " ", name).strip()
    name = re.sub(r"\s+", " ", name)
    return name[:1].upper() + name[1:] if name else name


def nice_title(stem: str, folder: str) -> str:
    """Turn 'ai508-lec01_notes' / 'lec01' / 'ex3' into 'Lecture 1' etc."""
    raw = stem
    if raw.lower() in {"notes", "main", "index", folder.lower()}:
        raw = folder
    # drop a leading course-code token like 'ai508' or 'dm549'
    raw = re.sub(r"^[a-z]{2}\d{3}[-_ ]*", "", raw, flags=re.I)
    # drop trailing '_notes' / '-notes'
    raw = re.sub(r"[-_ ]*notes?$", "", raw, flags=re.I)
    patterns = [
        (re.compile(r"^lec\w*[-_ ]?0*(\d+)$", re.I), "Lecture {}"),
        (re.compile(r"^(?:ex|exercise|excercise)[-_ ]?0*(\d+)$", re.I), "Exercise {}"),
        (re.compile(r"^case[-_ ]?0*(\d+)$", re.I), "Case {}"),
        (re.compile(r"^(?:week|uge)[-_ ]?0*(\d+)$", re.I), "Week {}"),
        (re.compile(r"^(?:assignment|assign)[-_ ]?0*(\d+)$", re.I), "Assignment {}"),
    ]
    for rx, label in patterns:
        m = rx.match(raw.strip())
        if m:
            return label.format(int(m.group(1)))
    return prettify(raw) or prettify(folder)


def section_for(rel_parts: list[str]) -> str:
    for part in rel_parts:
        for rule, label in SECTION_RULES:
            if rule.search(part):
                return label
    return "Other"


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
    ts = datetime.fromtimestamp(path.stat().st_mtime, tz=timezone.utc)
    return ts.isoformat()


def load_meta() -> dict:
    if NOTES_META.exists():
        try:
            return json.loads(NOTES_META.read_text())
        except json.JSONDecodeError as e:
            raise SystemExit(f"notes-meta.json is not valid JSON: {e}")
    return {}


def build(out_dir: Path) -> dict:
    meta = load_meta()
    pdf_out = out_dir / "pdf"
    pdf_out.mkdir(parents=True, exist_ok=True)

    semesters: dict[str, dict] = {}
    count = 0

    for sem_path in sorted(REPO_ROOT.iterdir()):
        m = SEMESTER_DIR.match(sem_path.name)
        if not m or not sem_path.is_dir():
            continue
        sem_key = f"{m.group(1)}. semester"

        for typ_path in sorted(sem_path.rglob("*.typ")):
            pdf_path = typ_path.with_suffix(".pdf")
            if not pdf_path.exists():
                continue
            rel = typ_path.relative_to(REPO_ROOT).as_posix()
            if DENY.search(rel):
                continue
            entry_meta = meta.get(rel, {})
            if entry_meta.get("hidden"):
                continue

            course_dir = None
            for parent in typ_path.parents:
                if parent == sem_path:
                    break
                course_dir = parent
            if course_dir is None:
                continue
            cm = COURSE_DIR.match(course_dir.name)
            course_code = cm.group(1) if cm else course_dir.name
            course_name = prettify(cm.group(2)) if cm else prettify(course_dir.name)

            rel_parts = typ_path.relative_to(course_dir).parts
            section = section_for(list(rel_parts))

            digest = hashlib.sha1(rel.encode()).hexdigest()[:12]
            shutil.copy2(pdf_path, pdf_out / f"{digest}.pdf")

            title = entry_meta.get("title") or nice_title(typ_path.stem, typ_path.parent.name)

            item = {
                "title": title,
                "description": entry_meta.get("description", ""),
                "section": section,
                "pdf": f"pdf/{digest}.pdf",
                "source": f"{GITHUB_BLOB}/{rel}",
                "updated": git_last_modified(typ_path),
            }

            sem = semesters.setdefault(sem_key, {"semester": sem_key, "courses": {}})
            course = sem["courses"].setdefault(
                f"{course_code} {course_name}",
                {"code": course_code, "name": course_name, "sections": {}},
            )
            course["sections"].setdefault(section, []).append(item)
            count += 1

    # dict -> sorted lists for stable output
    manifest = {
        "generated": datetime.now(timezone.utc).isoformat(),
        "count": count,
        "semesters": [],
    }
    for sem_key in sorted(semesters, reverse=True):
        sem = semesters[sem_key]
        sem_out = {"semester": sem_key, "courses": []}
        for course_key in sorted(sem["courses"]):
            c = sem["courses"][course_key]
            sections = [
                {"name": name, "items": sorted(items, key=lambda i: i["title"].lower())}
                for name, items in sorted(c["sections"].items())
            ]
            sem_out["courses"].append(
                {"code": c["code"], "name": c["name"], "sections": sections}
            )
        manifest["semesters"].append(sem_out)

    (out_dir / "manifest.json").write_text(json.dumps(manifest, indent=2) + "\n")
    return manifest


def main() -> None:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--out", required=True, type=Path,
                    help="output directory (manifest.json + pdf/ are written here)")
    args = ap.parse_args()
    out_dir = args.out.resolve()
    manifest = build(out_dir)
    print(f"wrote {out_dir/'manifest.json'} - {manifest['count']} notes, "
          f"{len(manifest['semesters'])} semesters")


if __name__ == "__main__":
    main()

#!/usr/bin/env python3
"""Build the /coursework hub for simo899t.github.io from a single hand-authored file.

``coursework.yml`` at the repo root is the ONLY source of truth. Nothing is
auto-detected: you lay out semesters -> courses -> sections -> items exactly as
they should appear on the page. Each item is either

    - title: "Lecture 1"
      pdf: 5sem/AI509-.../lec01/notes.pdf     # repo-relative path to a PDF
      source: 5sem/AI509-.../lec01/notes.typ  # optional; repo path -> GitHub blob URL, or a full URL
      description: "..."                       # optional
      updated: 2026-09-01                      # optional; omitted -> git history of the pdf

or an external link instead of a bundled PDF

    - title: "Reading group repo"
      url: https://github.com/simo899t/SDU/tree/main/5sem/...

Writes:
  <out>/manifest.json      - the tree the coursework.astro page renders
  <out>/pdf/<hash>.pdf     - a flat copy of each bundled PDF

A `pdf:` that does not exist on disk aborts the build (so broken paths fail CI).

Usage:
    python3 scripts/build_coursework.py --out build/coursework
    python3 scripts/build_coursework.py --check      # validate only, no output
"""
from __future__ import annotations

import argparse
import hashlib
import json
import shutil
import subprocess
import sys
from datetime import datetime, timezone
from pathlib import Path

import yaml

from urllib.parse import quote

REPO_ROOT = Path(__file__).resolve().parent.parent
SPEC_FILE = REPO_ROOT / "coursework.yml"
BLOB = "https://github.com/simo899t/SDU/blob/main"
TREE = "https://github.com/simo899t/SDU/tree/main"


def _gh(base: str, rel: str) -> str:
    return f"{base}/{quote(rel, safe='/')}"


class SpecError(Exception):
    pass


def _is_url(s: str) -> bool:
    return s.startswith("http://") or s.startswith("https://")


def _git_last_modified(rel: str) -> str:
    path = REPO_ROOT / rel
    try:
        out = subprocess.run(
            ["git", "log", "-1", "--format=%cI", "--", rel],
            cwd=REPO_ROOT, capture_output=True, text=True, check=True,
        ).stdout.strip()
        if out:
            return out
    except Exception:
        pass
    try:
        return datetime.fromtimestamp(path.stat().st_mtime, tz=timezone.utc).isoformat()
    except OSError:
        return ""


def _need(d: dict, key: str, where: str):
    if key not in d or d[key] in (None, ""):
        raise SpecError(f"{where}: missing required field '{key}'")
    return d[key]


def build(out_dir: Path | None) -> dict:
    if not SPEC_FILE.exists():
        raise SpecError(f"{SPEC_FILE} not found")
    spec = yaml.safe_load(SPEC_FILE.read_text()) or {}

    pdf_out = None
    if out_dir is not None:
        pdf_out = out_dir / "pdf"
        if pdf_out.exists():
            shutil.rmtree(pdf_out)
        pdf_out.mkdir(parents=True, exist_ok=True)

    errors: list[str] = []
    count = 0
    seen_pdf: dict[str, str] = {}
    semesters_out = []

    for si, sem in enumerate(spec.get("semesters") or []):
        where_s = f"semesters[{si}]"
        sem_name = _need(sem, "name", where_s)
        courses_out = []
        for ci, course in enumerate(sem.get("courses") or []):
            where_c = f"{where_s}.courses[{ci}]"
            course_out = {
                "code": course.get("code", ""),
                "name": _need(course, "name", where_c),
                "sections": [],
            }
            if course.get("url"):
                course_out["url"] = course["url"]
            for xi, section in enumerate(course.get("sections") or []):
                where_x = f"{where_c}.sections[{xi}]"
                sec_out = {"name": _need(section, "name", where_x), "items": []}
                for ii, item in enumerate(section.get("items") or []):
                    where_i = f"{where_x}.items[{ii}]"
                    try:
                        sec_out["items"].append(
                            _build_item(item, where_i, pdf_out, seen_pdf)
                        )
                        count += 1
                    except SpecError as e:
                        errors.append(str(e))
                if sec_out["items"]:
                    course_out["sections"].append(sec_out)
            if course_out["sections"]:
                courses_out.append(course_out)
        if courses_out:
            semesters_out.append({"semester": sem_name, "courses": courses_out})

    if errors:
        raise SpecError("coursework.yml has problems:\n  - " + "\n  - ".join(errors))

    manifest = {
        "generated": datetime.now(timezone.utc).isoformat(),
        "count": count,
        "intro": (spec.get("intro") or "").strip(),
        "semesters": semesters_out,
    }
    if out_dir is not None:
        (out_dir / "manifest.json").write_text(json.dumps(manifest, indent=2) + "\n")
    return manifest


def _build_item(item: dict, where: str, pdf_out: Path | None, seen_pdf: dict) -> dict:
    title = _need(item, "title", where)
    out = {
        "title": title,
        "description": item.get("description", "") or "",
    }

    pdf_rel = item.get("pdf")
    url = item.get("url")
    src = item.get("source")
    if pdf_rel and url:
        raise SpecError(f"{where} ({title!r}): set only one of 'pdf' or 'url'")
    if not pdf_rel and not url and not src:
        raise SpecError(f"{where} ({title!r}): needs one of 'pdf', 'url' or 'source'")

    # source link: a repo path becomes a GitHub blob URL, a full URL passes through
    if src:
        out["source"] = src if _is_url(src) else _gh(BLOB, src)

    if not pdf_rel and not url:
        # source-only entry: a note whose PDF isn't compiled/committed yet
        out["updated"] = str(item["updated"]) if item.get("updated") else ""
        return out

    if url:
        out["url"] = url
        if not src and _is_url(url):
            out["source"] = url
        out["updated"] = item.get("updated", "") or ""
        return out

    src_path = REPO_ROOT / pdf_rel
    if not src_path.is_file():
        raise SpecError(f"{where} ({title!r}): pdf not found: {pdf_rel}")
    digest = hashlib.sha1(pdf_rel.encode()).hexdigest()[:12]
    if digest in seen_pdf and seen_pdf[digest] != pdf_rel:
        raise SpecError(f"{where}: hash collision {digest} ({pdf_rel} / {seen_pdf[digest]})")
    seen_pdf[digest] = pdf_rel
    if pdf_out is not None:
        shutil.copy2(src_path, pdf_out / f"{digest}.pdf")
    out["pdf"] = f"pdf/{digest}.pdf"
    out["updated"] = str(item["updated"]) if item.get("updated") else _git_last_modified(pdf_rel)

    # every bundled PDF gets a "source" link even when the spec omits one:
    # the sibling .typ if there is one, otherwise the containing folder on GitHub.
    if "source" not in out:
        typ_rel = str(Path(pdf_rel).with_suffix(".typ"))
        if (REPO_ROOT / typ_rel).is_file():
            out["source"] = _gh(BLOB, typ_rel)
        else:
            out["source"] = _gh(TREE, str(Path(pdf_rel).parent))
    return out


def main() -> None:
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--out", type=Path, help="output dir for manifest.json + pdf/")
    ap.add_argument("--check", action="store_true",
                    help="validate coursework.yml without writing anything")
    args = ap.parse_args()

    if not args.check and not args.out:
        ap.error("--out is required (unless --check)")

    try:
        manifest = build(None if args.check else args.out.resolve())
    except SpecError as e:
        print(f"error: {e}", file=sys.stderr)
        raise SystemExit(1)

    sems = len(manifest["semesters"])
    if args.check:
        print(f"coursework.yml OK - {manifest['count']} items, {sems} semesters")
    else:
        print(f"wrote {args.out.resolve()/'manifest.json'} - "
              f"{manifest['count']} items, {sems} semesters")


if __name__ == "__main__":
    main()

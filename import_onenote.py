#!/usr/bin/env python3
"""
OneNote → Typst importer for SDU notes.

Usage:
  python import_onenote.py <folder_of_docx_files> [options]

Options:
  --course   Course name (e.g. "DM500 - Discrete Mathematics")
  --author   Author name (default: "Simon Holm")
  --sem      Semester folder (default: "1sem")
  --date     Default date for all notes (dd/mm/yyyy). If omitted, tries to
             extract from filename, else leaves blank.

Example:
  python import_onenote.py ~/Downloads/onenote_export --course "DM500 - Discrete Mathematics"

Requirements:
  pip install python-docx
  brew install pandoc
"""

import argparse
import re
import shutil
import subprocess
import sys
from datetime import datetime
from pathlib import Path

TEMPLATE_PATH_FROM_SDU = "temp/temp.typ"
SDU_ROOT = Path(__file__).parent / "Documents" / "SDU"
AUTHOR_DEFAULT = "Simon Holm"


def check_pandoc():
    if not shutil.which("pandoc"):
        sys.exit("Error: pandoc not found. Install with: brew install pandoc")


def relative_template_path(output_file: Path) -> str:
    """Compute relative path from output_file's dir to temp/temp.typ."""
    template_abs = SDU_ROOT / TEMPLATE_PATH_FROM_SDU
    rel = Path(output_file.parent).resolve()
    try:
        return str(Path(template_abs).relative_to(rel.root))
    except Exception:
        pass
    # Use os.path.relpath
    import os
    return os.path.relpath(template_abs, output_file.parent)


def extract_date_from_name(name: str) -> str:
    """Try to pull a date from filename like 2024-01-15 or 15-01-2024."""
    patterns = [
        r"(\d{4})[._-](\d{2})[._-](\d{2})",   # yyyy-mm-dd
        r"(\d{2})[._-](\d{2})[._-](\d{4})",   # dd-mm-yyyy
    ]
    for pat in patterns:
        m = re.search(pat, name)
        if m:
            groups = m.groups()
            try:
                if len(groups[0]) == 4:
                    d = datetime(int(groups[0]), int(groups[1]), int(groups[2]))
                else:
                    d = datetime(int(groups[2]), int(groups[1]), int(groups[0]))
                return d.strftime("%d/%m/%Y")
            except ValueError:
                pass
    return ""


def docx_to_typst_body(docx_path: Path) -> str:
    """Convert a .docx file to Typst markup using pandoc."""
    result = subprocess.run(
        ["pandoc", str(docx_path), "-t", "typst", "--wrap=none"],
        capture_output=True,
        text=True,
    )
    if result.returncode != 0:
        print(f"  [warn] pandoc error for {docx_path.name}: {result.stderr.strip()}")
    return result.stdout


def build_typst_file(title: str, course: str, author: str, date: str, template_rel: str, body: str) -> str:
    header = f"""\
#let title = "{title}"
#let course = "{course}"
#let date = "{date}"

#import "{template_rel}": *

#note(
  title: title,
  course: course,
  date: date,
)

#pagebreak()
// content starts here

"""
    return header + body


def slugify(name: str) -> str:
    """Convert a note title to a safe directory/file name."""
    name = re.sub(r"[^\w\s-]", "", name)
    name = re.sub(r"[\s_]+", "-", name.strip())
    return name.lower()


def main():
    parser = argparse.ArgumentParser(description="Import OneNote .docx exports into SDU Typst notes")
    parser.add_argument("input", help="Folder containing .docx files exported from OneNote")
    parser.add_argument("--course", required=True, help='Course name, e.g. "DM500 - Discrete Mathematics"')
    parser.add_argument("--author", default=AUTHOR_DEFAULT, help="Author name")
    parser.add_argument("--sem", default="1sem", help="Semester folder (default: 1sem)")
    parser.add_argument("--date", default="", help="Default date (dd/mm/yyyy) if not found in filename")
    args = parser.parse_args()

    check_pandoc()

    input_dir = Path(args.input).expanduser().resolve()
    if not input_dir.is_dir():
        sys.exit(f"Error: '{input_dir}' is not a directory")

    docx_files = sorted(input_dir.glob("*.docx"))
    if not docx_files:
        sys.exit(f"No .docx files found in {input_dir}")

    course_slug = slugify(args.course)
    output_base = SDU_ROOT / args.sem / course_slug

    print(f"Found {len(docx_files)} .docx file(s)")
    print(f"Output base: {output_base}")
    print()

    for docx in docx_files:
        stem = docx.stem  # filename without extension
        title = stem.replace("_", " ").replace("-", " ").strip()
        date = args.date or extract_date_from_name(stem)

        note_slug = slugify(stem)
        out_dir = output_base / note_slug
        out_dir.mkdir(parents=True, exist_ok=True)
        out_file = out_dir / f"{note_slug}.typ"

        template_rel = relative_template_path(out_file)

        print(f"  {docx.name}")
        print(f"    → {out_file.relative_to(SDU_ROOT.parent.parent)}")

        body = docx_to_typst_body(docx)
        content = build_typst_file(title, args.course, args.author, date, template_rel, body)

        out_file.write_text(content, encoding="utf-8")
        print(f"    ✓ {len(body.splitlines())} lines of content")

    print()
    print(f"Done — {len(docx_files)} note(s) written to {output_base}")


if __name__ == "__main__":
    main()
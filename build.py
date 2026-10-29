#!/usr/bin/env python3
"""Build the Cloudflare Pages output (dist/) for dmsr-data.

- copies only the numbered HTML files (hierarchy preserved)
- ships ONE jats-preview.css at the site root
- adds a _redirects rule so every <dir>/jats-preview.css request is served
  from that single file (the HTML files need no editing)
- generates manifest.json used by the file-browser UI (site/index.html)

Cloudflare settings:  Build command: python3 build.py   Output dir: dist
Set the env var DATA_DIR if the numbered folders are not at the repo root
(e.g. DATA_DIR=data/03_labor).
"""
import hashlib
import json
import os
import re
import shutil
import sys
from pathlib import Path

# Source files contain malformed absolute links such as "https:/host/path"
# (single slash). Over http(s) browsers read those as site-relative paths, so
# repair them to "https://host/path" while copying.
BAD_SCHEME = re.compile(rb"\b(https?):/(?!/)")
fixed_files = 0

ROOT = Path(os.environ.get("DATA_DIR", ".")).resolve()
OUT = Path("dist")
SITE = Path(__file__).resolve().parent / "site"
CSS_NAME = "jats-preview.css"
LIMIT = 20_000
RANGE = re.compile(r"^\d{5}-\d{5}$")
DOC = re.compile(r"^(\d{5})\.html$")


def dirs(parent):
    return sorted(p for p in parent.iterdir() if p.is_dir() and RANGE.match(p.name))


def copy_fixed(src, dst):
    global fixed_files
    data = src.read_bytes()
    new = BAD_SCHEME.sub(rb"\1://", data)
    if new != data:
        fixed_files += 1
    dst.write_bytes(new)


def main():
    if OUT.exists():
        shutil.rmtree(OUT)
    OUT.mkdir()

    manifest, css_src, css_hashes = {}, None, set()
    for g in dirs(ROOT):
        for s in dirs(g):
            nums = []
            for f in sorted(s.iterdir()):
                m = DOC.match(f.name)
                if m:
                    dest = OUT / g.name / s.name
                    dest.mkdir(parents=True, exist_ok=True)
                    copy_fixed(f, dest / f.name)
                    nums.append(int(m.group(1)))
                elif f.name == CSS_NAME:
                    css_hashes.add(hashlib.md5(f.read_bytes()).hexdigest())
                    css_src = css_src or f
            if nums:
                manifest.setdefault(g.name, {})[s.name] = nums

    if css_src is None:
        sys.exit(f"No {CSS_NAME} found under {ROOT}")
    if len(css_hashes) > 1:
        print(f"WARNING: {len(css_hashes)} different versions of {CSS_NAME} found; using {css_src}")

    shutil.copyfile(css_src, OUT / CSS_NAME)
    for item in SITE.iterdir():
        shutil.copy2(item, OUT / item.name)
    (OUT / "manifest.json").write_text(json.dumps(manifest, separators=(",", ":")))
    (OUT / "_redirects").write_text(f"/:group/:sub/{CSS_NAME} /{CSS_NAME} 200\n")

    total = sum(1 for p in OUT.rglob("*") if p.is_file())
    docs = sum(len(v) for g in manifest.values() for v in g.values())
    print(f"{docs} documents, {total} files in dist/ (limit {LIMIT}, headroom {LIMIT - total})")
    print(f"Repaired malformed links in {fixed_files} files")
    if total > LIMIT:
        sys.exit("Over the Cloudflare Pages file limit")


if __name__ == "__main__":
    main()

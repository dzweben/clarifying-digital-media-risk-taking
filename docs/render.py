#!/usr/bin/env python3
"""Render the knitted pipeline markdown into docs/index.html.

Usage, from the repo root:

    cd study-2 && Rscript -e 'knitr::knit("../docs/deft_study2_pipeline.Rmd", output="../docs/pipeline.md")'
    python3 docs/render.py

Requires mistune. The Rmd is the source of truth; this only wraps the knitted
markdown in the page shell.
"""

import html
import pathlib
import re
import sys

import mistune

HERE = pathlib.Path(__file__).parent
MD = HERE / "pipeline.md"
OUT = HERE / "index.html"

CSS = """
:root{
  --bg:#fdfdfc; --fg:#1c1c1a; --muted:#6b6b66; --faint:#8b8b85;
  --rule:#e4e3df; --rule-strong:#c9c8c2;
  --link:#1a4d8f; --link-hover:#0f3565;
  --code-bg:#f5f5f3; --code-fg:#2a2a28; --out-fg:#55554f;
  --tbl-head:#f5f5f3;
}
@media (prefers-color-scheme: dark){
  :root:not([data-theme="light"]){
    --bg:#16171a; --fg:#e4e4e1; --muted:#9a9a94; --faint:#7d7d77;
    --rule:#2b2d31; --rule-strong:#3d3f45;
    --link:#7fa9de; --link-hover:#a3c4ec;
    --code-bg:#1d1f23; --code-fg:#d4d4d0; --out-fg:#a5a59f;
    --tbl-head:#1d1f23;
  }
}
:root[data-theme="dark"]{
  --bg:#16171a; --fg:#e4e4e1; --muted:#9a9a94; --faint:#7d7d77;
  --rule:#2b2d31; --rule-strong:#3d3f45;
  --link:#7fa9de; --link-hover:#a3c4ec;
  --code-bg:#1d1f23; --code-fg:#d4d4d0; --out-fg:#a5a59f;
  --tbl-head:#1d1f23;
}

*{box-sizing:border-box}
html{-webkit-text-size-adjust:100%}
body{
  margin:0; background:var(--bg); color:var(--fg);
  font:16.5px/1.7 "Charter","Bitstream Charter","Sitka Text",Cambria,Georgia,serif;
}
.wrap{display:grid; grid-template-columns:232px minmax(0,1fr); gap:60px;
      max-width:1160px; margin:0 auto; padding:0 20px}

nav{position:sticky; top:0; align-self:start; max-height:100vh; overflow-y:auto;
    padding:52px 0; font-family:-apple-system,BlinkMacSystemFont,"Segoe UI",Helvetica,sans-serif;
    font-size:13px; line-height:1.45}
nav a{display:block; color:var(--muted); text-decoration:none; padding:4px 0;
      border-left:2px solid transparent; padding-left:10px; margin-left:-12px}
nav a:visited{color:var(--muted)}
nav a:hover{color:var(--fg); border-left-color:var(--rule-strong)}
nav .lvl2{padding-left:22px; font-size:12.5px}
nav .navtitle{display:block; color:var(--faint); font-size:11px; letter-spacing:.1em;
              text-transform:uppercase; margin:0 0 12px; font-weight:600}

main{padding:52px 0 140px; min-width:0}
h1{font-size:27px; line-height:1.3; margin:56px 0 16px; letter-spacing:-.005em;
   padding-bottom:10px; border-bottom:1px solid var(--rule); font-weight:600}
h1:first-child{margin-top:0}
h2{font-size:20px; margin:42px 0 12px; font-weight:600; letter-spacing:-.005em}
h3{font-size:17px; margin:32px 0 10px; font-weight:600}
p{margin:0 0 16px; max-width:72ch}
ul,ol{max-width:72ch; padding-left:24px; margin:0 0 16px}
li{margin-bottom:6px}
strong{font-weight:650}

a{color:var(--link); text-decoration:none; border-bottom:1px solid rgba(26,77,143,.28)}
a:visited{color:var(--link)}
a:hover{color:var(--link-hover); border-bottom-color:currentColor}
@media (prefers-color-scheme: dark){
  :root:not([data-theme="light"]) a{border-bottom-color:rgba(127,169,222,.3)}
}

blockquote{margin:0 0 20px; padding:2px 0 2px 18px; border-left:2px solid var(--rule-strong);
           color:var(--muted); font-size:15px; max-width:72ch}
blockquote p{margin-bottom:6px}

code{font-family:ui-monospace,SFMono-Regular,Menlo,Consolas,monospace;
     font-size:.85em; background:var(--code-bg); padding:2px 5px; border-radius:3px}
pre{background:var(--code-bg); color:var(--code-fg); padding:16px 18px; border-radius:5px;
    overflow-x:auto; border:1px solid var(--rule); margin:0 0 20px;
    font-size:13px; line-height:1.6;
    font-family:ui-monospace,SFMono-Regular,Menlo,Consolas,monospace}
pre code{background:none; padding:0; font-size:inherit}
pre.output{background:transparent; border:none; border-left:2px solid var(--rule);
           border-radius:0; color:var(--out-fg); padding:2px 0 2px 16px; margin:-8px 0 20px}

table{border-collapse:collapse; width:100%; margin:0 0 22px; font-size:13.5px;
      font-family:-apple-system,BlinkMacSystemFont,"Segoe UI",Helvetica,sans-serif}
th,td{text-align:left; padding:7px 14px 7px 0; border-bottom:1px solid var(--rule);
      vertical-align:top}
th{font-weight:600; border-bottom:1px solid var(--rule-strong); white-space:nowrap}
tbody tr:last-child td{border-bottom:1px solid var(--rule-strong)}
td code{white-space:nowrap; font-size:.9em}

.masthead{border-bottom:1px solid var(--rule-strong); padding-bottom:22px; margin-bottom:34px}
.masthead h1{border:none; margin:0 0 6px; padding:0; font-size:30px}
.masthead .sub{color:var(--muted); font-size:14px; margin:0;
               font-family:-apple-system,BlinkMacSystemFont,"Segoe UI",Helvetica,sans-serif}
.dl{margin:30px 0 0; padding:16px 18px; border:1px solid var(--rule); border-radius:5px;
    font-family:-apple-system,BlinkMacSystemFont,"Segoe UI",Helvetica,sans-serif; font-size:14px}
.dl p{margin:0}
footer{margin-top:80px; padding-top:20px; border-top:1px solid var(--rule);
       color:var(--muted); font-size:13px; max-width:72ch;
       font-family:-apple-system,BlinkMacSystemFont,"Segoe UI",Helvetica,sans-serif}

@media (max-width:860px){
  .wrap{grid-template-columns:1fr; gap:0}
  nav{position:static; max-height:none; padding:26px 0 18px;
      border-bottom:1px solid var(--rule)}
  main{padding-top:26px}
  .masthead h1{font-size:24px}
  h1{font-size:22px}
}
"""


def slugify(text: str) -> str:
    s = re.sub(r"<[^>]+>", "", text)
    s = html.unescape(s)
    s = re.sub(r"[^\w\s-]", "", s).strip().lower()
    return re.sub(r"[\s_]+", "-", s) or "section"


class Renderer(mistune.HTMLRenderer):
    """Adds heading ids and marks knitr output blocks."""

    def __init__(self):
        super().__init__(escape=False)
        self.toc = []

    def heading(self, text, level, **attrs):
        hid = slugify(text)
        if level in (1, 2):
            self.toc.append((level, hid, re.sub(r"<[^>]+>", "", text)))
        return f'<h{level} id="{hid}">{text}</h{level}>\n'

    def block_code(self, code, info=None):
        esc = html.escape(code)
        # knitr writes chunk output as a plain block whose lines start with "#>"
        if info is None and code.lstrip().startswith("#>"):
            stripped = "\n".join(
                re.sub(r"^#>\s?", "", ln) for ln in code.rstrip("\n").split("\n")
            )
            return f'<pre class="output"><code>{html.escape(stripped)}</code></pre>\n'
        return f"<pre><code>{esc}</code></pre>\n"


def main() -> int:
    if not MD.exists():
        print(f"missing {MD} -- knit the Rmd first", file=sys.stderr)
        return 1

    md_text = MD.read_text()

    # Drop the YAML header knitr leaves behind.
    md_text = re.sub(r"\A---\n.*?\n---\n", "", md_text, flags=re.S)

    renderer = Renderer()
    convert = mistune.create_markdown(renderer=renderer, plugins=["table", "strikethrough"])
    body = convert(md_text)

    nav_items = [
        '<span class="navtitle">Contents</span>',
        '<a href="#top">Overview</a>',
    ]
    for level, hid, label in renderer.toc:
        cls = "" if level == 1 else ' class="lvl2"'
        nav_items.append(f'<a href="#{hid}"{cls}>{html.escape(label)}</a>')
    nav = "\n  ".join(nav_items)

    page = f"""<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>DEFT Study 2 Pipeline</title>
<style>{CSS}</style>
</head>
<body>
<div class="wrap">
<nav>
  {nav}
</nav>
<main id="top">

<div class="masthead">
  <h1>DEFT Study 2 analysis pipeline</h1>
  <p class="sub">Zweben et al. (2026), <em>Technology in Society</em> 88, 103491</p>
</div>

<div class="dl">
<p>This page is the knitted output of
<a href="deft_study2_pipeline.Rmd">deft_study2_pipeline.Rmd</a> &mdash; download that
file and run it on your own data.</p>
</div>

{body}

<footer>
<p>Generated from <code>deft_study2_pipeline.Rmd</code>. Repository:
<a href="https://github.com/dzweben/clarifying-digital-media-risk-taking">dzweben/clarifying-digital-media-risk-taking</a></p>
</footer>

</main>
</div>
</body>
</html>
"""
    OUT.write_text(page)
    print(f"wrote {OUT} ({len(page):,} bytes, {len(renderer.toc)} headings)")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

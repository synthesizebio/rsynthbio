#!/usr/bin/env python3

from __future__ import annotations

import json
import re
import shutil
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
OUTPUT_DIR = ROOT / "docs-external"
REPO_PREFIX = "/rsynthbio"

VIGNETTE_PAGES = [
    {
        "source": ROOT / "vignettes" / "getting-started.Rmd",
        "target": OUTPUT_DIR / "getting-started.mdx",
        "title": "Getting started",
        "description": "Authenticate, list models, and make your first prediction with rsynthbio.",
        "remove_sections": ["How to install", "Session info", "Additional Resources"],
    },
    {
        "source": ROOT / "vignettes" / "available-metadata.Rmd",
        "target": OUTPUT_DIR / "available-metadata.mdx",
        "title": "Available metadata",
        "description": "Browse and download the metadata vocabularies that each Synthesize Bio model accepts.",
        "remove_sections": [],
    },
    {
        "source": ROOT / "vignettes" / "baseline.Rmd",
        "target": OUTPUT_DIR / "models" / "baseline.mdx",
        "title": "Baseline models",
        "description": "Generate synthetic gene expression data from metadata alone.",
        "remove_sections": [],
    },
    {
        "source": ROOT / "vignettes" / "reference-conditioning.Rmd",
        "target": OUTPUT_DIR / "models" / "reference-conditioning.mdx",
        "title": "Reference conditioning",
        "description": "Generate expression data conditioned on a real reference sample.",
        "remove_sections": [],
    },
    {
        "source": ROOT / "vignettes" / "metadata-prediction.Rmd",
        "target": OUTPUT_DIR / "models" / "metadata-prediction.mdx",
        "title": "Metadata prediction",
        "description": "Infer biological metadata from observed expression data.",
        "remove_sections": [],
    },
]

REFERENCE_GROUPS = {
    "Predictions": [
        "predict_query",
        "get_example_query",
        "list_models",
    ],
    "Authentication": [
        "set_synthesize_token",
        "load_synthesize_token_from_keyring",
        "has_synthesize_token",
        "clear_synthesize_token",
    ],
    "Constants": [
        "API_BASE_URL",
        "DEFAULT_TIMEOUT",
        "DEFAULT_POLL_INTERVAL_SECONDS",
        "DEFAULT_POLL_TIMEOUT_SECONDS",
    ],
}

LINK_REPLACEMENTS = {
    "baseline.html": f"{REPO_PREFIX}/models/baseline",
    "reference-conditioning.html": f"{REPO_PREFIX}/models/reference-conditioning",
    "metadata-prediction.html": f"{REPO_PREFIX}/models/metadata-prediction",
    "getting-started.html": f"{REPO_PREFIX}/getting-started",
    "available-metadata.html": f"{REPO_PREFIX}/available-metadata",
}


def read_text(path: Path) -> str:
    return path.read_text(encoding="utf-8")


def write_text(path: Path, content: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(content.rstrip() + "\n", encoding="utf-8")


def normalize_newlines(text: str) -> str:
    return text.replace("\r\n", "\n").replace("\r", "\n")


def strip_frontmatter(text: str) -> str:
    text = normalize_newlines(text)
    if text.startswith("---\n"):
        end = text.find("\n---\n", 4)
        if end != -1:
            return text[end + 5 :]
    return text


def remove_setup_chunk(text: str) -> str:
    pattern = re.compile(
        r"```{r[^}]*include\s*=\s*FALSE[^}]*}\n.*?knitr::opts_chunk\$set\(.*?\n```\n*",
        re.DOTALL,
    )
    return re.sub(pattern, "", text, count=1)


def remove_section(text: str, heading: str) -> str:
    pattern = re.compile(
        rf"\n## {re.escape(heading)}\n.*?(?=\n## [^\n]+\n|\Z)",
        re.DOTALL,
    )
    return re.sub(pattern, "\n", text)


def convert_code_fences(text: str) -> str:
    return re.sub(r"```{r[^}]*}", "```r", text)


def replace_links(text: str) -> str:
    for source, target in LINK_REPLACEMENTS.items():
        text = text.replace(f"({source})", f"({target})")
    return text


def replace_note_blocks(text: str) -> str:
    pattern = re.compile(r"^> \*\*Note:\*\* (.+)$", re.MULTILINE)

    def repl(match: re.Match[str]) -> str:
        return f"<Note>\n  {match.group(1).strip()}\n</Note>"

    return re.sub(pattern, repl, text)


def cleanup_vignette_text(text: str) -> str:
    text = re.sub(r"\n{3,}", "\n\n", text).strip()
    text = text.replace("Alternatively, you can AI generate datasets from our", "To generate datasets without code, use our")
    return text


def frontmatter(title: str, description: str) -> str:
    return f'---\ntitle: "{title}"\ndescription: "{description}"\n---\n'


def convert_vignette(source: Path, title: str, description: str, remove_sections: list[str]) -> str:
    text = read_text(source)
    text = strip_frontmatter(text)
    text = remove_setup_chunk(text)
    for section in remove_sections:
        text = remove_section(text, section)
    text = convert_code_fences(text)
    text = replace_links(text)
    text = replace_note_blocks(text)
    text = cleanup_vignette_text(text)
    return frontmatter(title, description) + "\n" + text


def read_balanced(text: str, open_index: int) -> tuple[str, int]:
    if text[open_index] != "{":
        raise ValueError("Expected opening brace")
    depth = 1
    i = open_index + 1
    while i < len(text):
        char = text[i]
        if char == "{":
            depth += 1
        elif char == "}":
            depth -= 1
            if depth == 0:
                return text[open_index + 1 : i], i + 1
        i += 1
    raise ValueError("Unbalanced braces")


def extract_macro(text: str, command: str) -> str:
    token = "\\" + command + "{"
    index = text.find(token)
    if index == -1:
        return ""
    content, _ = read_balanced(text, index + len(token) - 1)
    return content.strip()


def replace_command(text: str, command: str, formatter) -> str:
    token = "\\" + command + "{"
    result: list[str] = []
    i = 0
    while i < len(text):
        index = text.find(token, i)
        if index == -1:
            result.append(text[i:])
            break
        result.append(text[i:index])
        content, end = read_balanced(text, index + len(token) - 1)
        result.append(formatter(content))
        i = end
    return "".join(result)


def replace_href(text: str) -> str:
    token = r"\href{"
    result: list[str] = []
    i = 0
    while i < len(text):
        index = text.find(token, i)
        if index == -1:
            result.append(text[i:])
            break
        result.append(text[i:index])
        url, next_index = read_balanced(text, index + len(token) - 1)
        if next_index >= len(text) or text[next_index] != "{":
            result.append(text[index:next_index])
            i = next_index
            continue
        label, end = read_balanced(text, next_index)
        result.append(f"[{latex_to_markdown(label)}]({latex_to_markdown(url)})")
        i = end
    return "".join(result)


def extract_items(block: str, named: bool) -> list[tuple[str, str]]:
    items: list[tuple[str, str]] = []
    token = r"\item{"
    i = 0
    while i < len(block):
        index = block.find(token, i)
        if index == -1:
            break
        first, next_index = read_balanced(block, index + len(token) - 1)
        if named:
            if next_index >= len(block) or block[next_index] != "{":
                break
            second, end = read_balanced(block, next_index)
            items.append((first.strip(), second.strip()))
            i = end
        else:
            items.append(("", first.strip()))
            i = next_index
    return items


def convert_item_block(block: str) -> str:
    bullets = extract_items(block, named=False)
    if not bullets:
        return latex_to_markdown(block)
    lines = [f"- {latex_to_markdown(item).strip()}" for _, item in bullets]
    return "\n".join(lines)


def latex_to_markdown(text: str) -> str:
    text = normalize_newlines(text).strip()
    if not text:
        return ""
    text = replace_href(text)
    text = replace_command(text, "dontrun", lambda content: latex_to_markdown(content))
    text = replace_command(text, "code", lambda content: f"`{latex_to_markdown(content)}`")
    text = replace_command(text, "link", lambda content: f"`{latex_to_markdown(content)}`")
    text = replace_command(text, "pkg", lambda content: f"`{latex_to_markdown(content)}`")
    text = replace_command(text, "env", lambda content: f"`{latex_to_markdown(content)}`")
    text = replace_command(text, "kbd", lambda content: f"`{latex_to_markdown(content)}`")
    text = replace_command(text, "samp", lambda content: f"`{latex_to_markdown(content)}`")
    text = replace_command(text, "eqn", lambda content: f"`{latex_to_markdown(content)}`")
    text = replace_command(text, "strong", lambda content: f"**{latex_to_markdown(content)}**")
    text = replace_command(text, "emph", lambda content: f"*{latex_to_markdown(content)}*")
    text = replace_command(text, "url", lambda content: latex_to_markdown(content))
    text = replace_command(text, "itemize", convert_item_block)
    text = replace_command(text, "enumerate", convert_item_block)
    text = text.replace(r"\cr", "\n")
    text = text.replace(r"\dots", "...")
    text = text.replace(r"\%", "%")
    text = re.sub(r"\\[a-zA-Z]+", "", text)
    text = re.sub(r"[{}]", "", text)
    text = re.sub(r"\n{3,}", "\n\n", text)
    return text.strip()


def parse_arguments(block: str) -> list[tuple[str, str]]:
    return [(name, latex_to_markdown(description)) for name, description in extract_items(block, named=True)]


def exported_symbols() -> list[str]:
    exports = []
    namespace = read_text(ROOT / "NAMESPACE")
    for line in namespace.splitlines():
        match = re.match(r"export\(([^)]+)\)", line.strip())
        if match:
            exports.append(match.group(1))
    return exports


def symbol_doc(symbol: str) -> dict[str, object]:
    raw = read_text(ROOT / "man" / f"{symbol}.Rd")
    source_match = re.search(r"Please edit documentation in ([^\n]+)", raw)
    source_path = source_match.group(1).strip() if source_match else "R/"
    title = latex_to_markdown(extract_macro(raw, "title")) or symbol
    usage = latex_to_markdown(extract_macro(raw, "usage"))
    description = latex_to_markdown(extract_macro(raw, "description"))
    value = latex_to_markdown(extract_macro(raw, "value"))
    examples = latex_to_markdown(extract_macro(raw, "examples"))
    arguments = parse_arguments(extract_macro(raw, "arguments"))
    return {
        "symbol": symbol,
        "title": title,
        "usage": usage,
        "description": description,
        "value": value,
        "examples": examples,
        "arguments": arguments,
        "source_path": source_path,
    }


def build_docs_json(symbols: list[str]) -> dict[str, object]:
    pages: list[object] = [
        "index",
        "installation",
        "getting-started",
        "available-metadata",
        {
            "group": "Models",
            "pages": [
                "models/baseline",
                "models/reference-conditioning",
                "models/metadata-prediction",
            ],
        },
        {
            "group": "Reference",
            "pages": [
                "reference",
                *[
                    {
                        "group": group_name,
                        "pages": [f"reference/{symbol}" for symbol in group_symbols if symbol in symbols],
                    }
                    for group_name, group_symbols in REFERENCE_GROUPS.items()
                ],
            ],
        },
        "license",
    ]
    return {
        "name": "rsynthbio",
        "description": "Mintlify navigation for the rsynthbio R SDK docs.",
        "navigation": {
            "groups": [
                {
                    "group": "R SDK",
                    "pages": pages,
                }
            ]
        },
    }


def build_index_page() -> str:
    return frontmatter(
        "R SDK",
        "rsynthbio — the official R client for the Synthesize Bio platform.",
    ) + f"""

`rsynthbio` is an R package that provides a convenient interface to the [Synthesize Bio](https://www.synthesize.bio/) API. It lets you generate realistic gene expression data for specified biological conditions, work with reference samples, and predict metadata from observed expression, all from R.

If you'd prefer 1-click dataset generation and analysis, try the [web platform](https://app.synthesize.bio/datasets/).

<CardGroup cols={{2}}>
  <Card title="Installation" icon="download" href="{REPO_PREFIX}/installation">
    Install from CRAN, GitHub, or a local source build.
  </Card>
  <Card title="Getting started" icon="rocket" href="{REPO_PREFIX}/getting-started">
    Authenticate, list models, and make your first prediction.
  </Card>
  <Card title="Baseline models" icon="dna" href="{REPO_PREFIX}/models/baseline">
    Generate synthetic expression from metadata alone.
  </Card>
  <Card title="Reference conditioning" icon="microscope" href="{REPO_PREFIX}/models/reference-conditioning">
    Anchor generation to a real reference sample.
  </Card>
  <Card title="Metadata prediction" icon="magnifying-glass" href="{REPO_PREFIX}/models/metadata-prediction">
    Infer biological metadata from observed expression.
  </Card>
  <Card title="Function reference" icon="code" href="{REPO_PREFIX}/reference">
    Generated from the package help pages in this repo.
  </Card>
</CardGroup>

## Quickstart

```r
library(rsynthbio)

set_synthesize_token(use_keyring = TRUE)

query <- get_example_query(model_id = "gem-1-bulk")$example_query
result <- predict_query(query, model_id = "gem-1-bulk")

metadata <- result$metadata
expression <- result$expression
```

## Source and support

- [Source code](https://github.com/synthesizebio/rsynthbio)
- [CRAN package page](https://cran.r-project.org/package=rsynthbio)
- Email [support@synthesize.bio](mailto:support@synthesize.bio)
"""


def build_installation_page() -> str:
    return frontmatter(
        "Installation",
        "Install rsynthbio from CRAN, GitHub, or a local source build.",
    ) + f"""

## Prerequisites

To use `rsynthbio`, first create an account at [app.synthesize.bio](https://app.synthesize.bio/).

## From CRAN

The recommended way to install:

```r
install.packages("rsynthbio")
```

## Development version from GitHub

For the latest unreleased changes:

```r
if (!("remotes" %in% installed.packages())) {{
  install.packages("remotes")
}}
remotes::install_github("synthesizebio/rsynthbio")
```

## Verify the install

```r
library(rsynthbio)

packageVersion("rsynthbio")
```

<Card title="Next: Get started" icon="arrow-right" href="{REPO_PREFIX}/getting-started">
  Authenticate and make your first prediction.
</Card>
"""


def build_reference_index(docs: dict[str, dict[str, object]]) -> str:
    lines = [
        frontmatter(
            "Function reference",
            "Generated reference pages for exported rsynthbio functions and constants.",
        ),
        "",
        "`rsynthbio` ships its function-level docs with the package itself. The pages below are generated from the package help files in `man/` so the shared docs site can mirror the current exported reference.",
        "",
        "<CardGroup cols={2}>",
        '  <Card title="In R" icon="terminal">',
        "    ```r",
        "    ?predict_query",
        "    ?set_synthesize_token",
        '    help(package = "rsynthbio")',
        "    ```",
        "  </Card>",
        '  <Card title="CRAN reference manual (PDF)" icon="file-pdf" href="https://cran.r-project.org/web/packages/rsynthbio/rsynthbio.pdf">',
        "    The full reference for the latest CRAN release, in one file.",
        "  </Card>",
        "</CardGroup>",
        "",
    ]
    for group_name, symbols in REFERENCE_GROUPS.items():
        lines.append(f"## {group_name}")
        lines.append("")
        for symbol in symbols:
            if symbol not in docs:
                continue
            title = str(docs[symbol]["title"])
            description = str(docs[symbol]["description"]).splitlines()[0]
            lines.append(f'- [`{symbol}`]({REPO_PREFIX}/reference/{symbol}) — {title}. {description}')
        lines.append("")
    return "\n".join(lines).strip() + "\n"


def build_reference_page(doc: dict[str, object]) -> str:
    lines = [
        frontmatter(
            str(doc["symbol"]),
            f'{doc["title"]}.',
        ),
        "",
        str(doc["description"]),
        "",
    ]
    if doc["usage"]:
        lines.extend(
            [
                "## Usage",
                "",
                "```r",
                str(doc["usage"]),
                "```",
                "",
            ]
        )
    arguments = list(doc["arguments"])
    if arguments:
        lines.extend(["## Arguments", ""])
        for name, description in arguments:
            lines.append(f"- **`{name}`**: {description}")
        lines.append("")
    if doc["value"]:
        lines.extend(["## Returns", "", str(doc["value"]), ""])
    if doc["examples"]:
        lines.extend(["## Examples", "", "```r", str(doc["examples"]), "```", ""])
    lines.extend(
        [
            "## Source",
            "",
            f'Generated from [`{doc["source_path"]}`](https://github.com/synthesizebio/rsynthbio/blob/main/{doc["source_path"]}) and the package help files in `man/`.',
        ]
    )
    return "\n".join(lines).strip() + "\n"


def build_license_page() -> str:
    license_text = read_text(ROOT / "LICENSE").strip()
    return frontmatter(
        "License",
        "rsynthbio is licensed under the MIT License.",
    ) + f"""

`rsynthbio` is licensed under the MIT License.

```text
{license_text}
```
"""


def regenerate_docs() -> None:
    if OUTPUT_DIR.exists():
        shutil.rmtree(OUTPUT_DIR)
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    exports = exported_symbols()
    docs = {symbol: symbol_doc(symbol) for symbol in exports}

    write_text(OUTPUT_DIR / "docs.json", json.dumps(build_docs_json(exports), indent=2))
    write_text(OUTPUT_DIR / "index.mdx", build_index_page())
    write_text(OUTPUT_DIR / "installation.mdx", build_installation_page())
    write_text(OUTPUT_DIR / "reference.mdx", build_reference_index(docs))
    write_text(OUTPUT_DIR / "license.mdx", build_license_page())

    for page in VIGNETTE_PAGES:
        content = convert_vignette(
            source=page["source"],
            title=page["title"],
            description=page["description"],
            remove_sections=page["remove_sections"],
        )
        write_text(page["target"], content)

    for symbol, doc in docs.items():
        write_text(OUTPUT_DIR / "reference" / f"{symbol}.mdx", build_reference_page(doc))


if __name__ == "__main__":
    regenerate_docs()

import shutil
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
MANUSCRIPT_DIR = REPO_ROOT / "manuscript"
DOCS_DIR = REPO_ROOT / "docs"

HTML_TARGETS = {
    MANUSCRIPT_DIR / "main.html": DOCS_DIR / "index.html",
    MANUSCRIPT_DIR / "supplementary.html": DOCS_DIR / "supplementary.html",
}


def publish_html_outputs() -> None:
    DOCS_DIR.mkdir(exist_ok=True)
    (DOCS_DIR / ".nojekyll").touch()

    for source_path, target_path in HTML_TARGETS.items():
        if source_path.exists():
            shutil.move(str(source_path), str(target_path))


if __name__ == "__main__":
    publish_html_outputs()

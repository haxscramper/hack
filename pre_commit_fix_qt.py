#!/usr/bin/env python
import subprocess
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent
PROJECT_DIR = REPO_ROOT / "testing" / "python"
QT_CORE = PROJECT_DIR / ".venv/lib/python3.13/site-packages/PySide6/QtCore.pyi"
QT_WIDGETS = PROJECT_DIR / ".venv/lib/python3.13/site-packages/PySide6/QtWidgets.pyi"


def main() -> int:
    targets = sys.argv[1:]
    if not targets:
        return 0

    rc = 0
    for target in targets:
        rc = (subprocess.run(
            [
                "uv",
                "run",
                "--project",
                str(PROJECT_DIR),
                "testing/python/standalone_scripts/fix_ai_qt.py",
                str(QT_CORE),
                str(QT_WIDGETS),
                "--target",
                target,
            ],
            cwd=REPO_ROOT,
        ).returncode or rc)
    return rc


if __name__ == "__main__":
    sys.exit(main())

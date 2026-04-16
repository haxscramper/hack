from beartype import beartype
from pathlib import Path
import image_tagger.config as config


def scan_images(root_dir: Path) -> list[Path]:
    res = []
    for p in root_dir.rglob("*"):
        if not p.is_file():
            continue
        if p.suffix.lower() not in config.config.IMAGE_EXTENSIONS:
            continue

        excluded = False
        for excl in config.config.excluded_directories:
            try:
                if Path(excl) in p.parents or p == Path(excl):
                    excluded = True
                    break
            except Exception:
                pass

        if not excluded:
            res.append(p)

    return sorted(res)

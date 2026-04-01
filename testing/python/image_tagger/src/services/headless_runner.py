import logging
import os
import urllib.request
from pathlib import Path

from db.session import init_db, make_session_factory
from db.repository import Repository
from config import SQLITE_FILENAME, CHROMA_DIRNAME
from .scanner import scan_images
from .annotation_service import AnnotationService
from .chroma_store import ChromaDescriptionStore
from .wd_tagger import WdTagger
from .ollama_tagger import OllamaTagger


def get_xdg_cache_dir() -> Path:
    xdg_cache_home = os.environ.get("XDG_CACHE_HOME")
    if xdg_cache_home:
        return Path(xdg_cache_home) / "haxscramper_tagger" / "models"
    return Path.home() / ".cache" / "haxscramper_tagger" / "models"


def ensure_file_exists(filename: Path, url: str) -> None:
    if not filename.exists():
        logging.info(f"File {filename} not found. Downloading from Hugging Face...")
        urllib.request.urlretrieve(url, str(filename))
        logging.info(f"Successfully downloaded {filename}")
    else:
        logging.info(f"File {filename} already exists locally. Skipping download.")


def run_headless(
    root_dir: Path,
):
    logging.info(f"Starting headless run for directory: {root_dir}")
    root_dir = root_dir.resolve()
    sqlite_path = root_dir / SQLITE_FILENAME
    chroma_path = root_dir / CHROMA_DIRNAME
    logging.debug(f"SQLite path: {sqlite_path}, Chroma path: {chroma_path}")

    # Setup WD Tagger model paths
    repo_url = "https://huggingface.co/SmilingWolf/wd-vit-tagger-v3/resolve/main/"
    cache_dir = get_xdg_cache_dir()
    cache_dir.mkdir(parents=True, exist_ok=True)

    wd_model_path = cache_dir / "model.onnx"
    wd_tags_csv = cache_dir / "selected_tags.csv"

    ensure_file_exists(wd_model_path, repo_url + "model.onnx")
    ensure_file_exists(wd_tags_csv, repo_url + "selected_tags.csv")

    engine = init_db(sqlite_path)
    Session = make_session_factory(engine)
    session = Session()

    try:
        repo = Repository(session)
        chroma_store = ChromaDescriptionStore(chroma_path)

        logging.info(f"Initializing WdTagger with model {wd_model_path}")
        wd_tagger = WdTagger(wd_model_path, wd_tags_csv)

        logging.info("Initializing Ollama tagger")
        ollama_tagger = OllamaTagger()

        service = AnnotationService(
            repository=repo,
            chroma_store=chroma_store,
            wd_tagger=wd_tagger,
            ollama_tagger=ollama_tagger,
        )

        image_count = 0
        for image_path in scan_images(root_dir):
            service.annotate_image(root_dir, image_path)
            image_count += 1

        logging.info(f"Headless run completed. Annotated {image_count} images.")
    finally:
        session.close()

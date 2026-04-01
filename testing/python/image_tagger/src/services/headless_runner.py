from pathlib import Path

from image_tagger.db.session import init_db, make_session_factory
from image_tagger.db.repository import Repository
from image_tagger.config import SQLITE_FILENAME, CHROMA_DIRNAME
from .scanner import scan_images
from .annotation_service import AnnotationService
from .chroma_store import ChromaDescriptionStore
from .wd_tagger import WdTagger
from .ollama_tagger import OllamaTagger


def run_headless(
    root_dir: Path,
    wd_model_path: Path | None = None,
    wd_tags_csv: Path | None = None,
    use_ollama: bool = True,
):
    root_dir = root_dir.resolve()
    sqlite_path = root_dir / SQLITE_FILENAME
    chroma_path = root_dir / CHROMA_DIRNAME

    engine = init_db(sqlite_path)
    Session = make_session_factory(engine)
    session = Session()

    repo = Repository(session)
    chroma_store = ChromaDescriptionStore(chroma_path)

    wd_tagger = None
    if wd_model_path and wd_tags_csv:
        wd_tagger = WdTagger(wd_model_path, wd_tags_csv)

    ollama_tagger = OllamaTagger() if use_ollama else None

    service = AnnotationService(
        repository=repo,
        chroma_store=chroma_store,
        wd_tagger=wd_tagger,
        ollama_tagger=ollama_tagger,
    )

    for image_path in scan_images(root_dir):
        service.annotate_image(root_dir, image_path)

    session.close()

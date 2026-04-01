from pathlib import Path
import click
from PySide6.QtWidgets import QApplication

from image_tagger.config import SQLITE_FILENAME, CHROMA_DIRNAME
from image_tagger.db.session import init_db, make_session_factory
from image_tagger.db.repository import Repository
from image_tagger.gui.main_window import MainWindow
from image_tagger.services.headless_runner import run_headless


@click.group()
def cli():
    pass


@cli.command()
@click.argument("root_dir", type=click.Path(exists=True, file_okay=False, path_type=Path))
def gui(root_dir: Path):
    root_dir = root_dir.resolve()
    engine = init_db(root_dir / SQLITE_FILENAME)
    session_factory = make_session_factory(engine)
    session = session_factory()
    repo = Repository(session)

    app = QApplication([])
    window = MainWindow(root_dir, repo)
    window.show()
    app.exec()
    session.close()


@cli.command()
@click.argument("root_dir", type=click.Path(exists=True, file_okay=False, path_type=Path))
@click.option("--wd-model", type=click.Path(exists=True, dir_okay=False, path_type=Path), default=None)
@click.option("--wd-tags-csv", type=click.Path(exists=True, dir_okay=False, path_type=Path), default=None)
@click.option("--use-ollama/--no-ollama", default=True)
def headless(root_dir: Path, wd_model: Path | None, wd_tags_csv: Path | None, use_ollama: bool):
    run_headless(
        root_dir=root_dir,
        wd_model_path=wd_model,
        wd_tags_csv=wd_tags_csv,
        use_ollama=use_ollama,
    )

from beartype import beartype
import logging
from pathlib import Path
import click
from PySide6.QtWidgets import QApplication

import image_tagger.config as config
from image_tagger.db.session import init_db, make_session_factory
from image_tagger.db.repository import Repository
from image_tagger.gui.main_window import MainWindow
from image_tagger.gui.state_models import AppState
from image_tagger.services.headless_runner import run_headless


@click.group()
def cli():
    pass


def _state_file_path(root_dir: Path) -> Path:
    return root_dir / "app_state.json"


def _load_app_state(root_dir: Path) -> AppState | None:
    state_file = _state_file_path(root_dir)
    if state_file.exists():
        try:
            return AppState.model_validate_json(state_file.read_text())
        except Exception as e:
            logging.warning(f"Failed to load app state from {state_file}: {e}")
    return None


def _save_app_state(root_dir: Path, state: AppState) -> None:
    state_file = _state_file_path(root_dir)
    try:
        state_file.write_text(state.model_dump_json(indent=4))
    except Exception as e:
        logging.warning(f"Failed to save app state to {state_file}: {e}")


@cli.command()
@click.argument(
    "root_dir", type=click.Path(exists=True, file_okay=False, path_type=Path)
)
def gui(root_dir: Path):
    logging.info(f"Starting GUI for directory: {root_dir}")
    root_dir = root_dir.resolve()
    config.init_config(root_dir)

    engine = init_db(root_dir / config.config.SQLITE_FILENAME)
    session_factory = make_session_factory(engine)
    session = session_factory()
    try:
        repo = Repository(session)

        app = QApplication([])
        window = MainWindow(root_dir, repo)

        state = _load_app_state(root_dir)
        if state is not None:
            window.set_state(state)

        window.show()
        app.exec()

        _save_app_state(root_dir, window.get_state())
    finally:
        session.close()
    logging.info("GUI closed")


@cli.command()
@click.argument(
    "root_dir", type=click.Path(exists=True, file_okay=False, path_type=Path)
)
@click.argument("enable_wd_tagger", type=click.BOOL, default=True)
@click.argument("enable_joycaption_rule_tags", type=click.BOOL, default=False)
@click.argument("enable_joycaption_description", type=click.BOOL, default=False)
def headless(
    root_dir: Path,
    enable_wd_tagger: bool,
    enable_joycaption_rule_tags: bool,
    enable_joycaption_description: bool,
):
    logging.info(f"Running in headless mode with root_dir={root_dir}")
    root_dir = root_dir.resolve()
    config.init_config(root_dir)

    run_headless(
        root_dir=root_dir,
        enable_wd_tagger=enable_wd_tagger,
        enable_joycaption_rule_tags=enable_joycaption_rule_tags,
        enable_joycaption_description=enable_joycaption_description,
    )

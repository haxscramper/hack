import logging
from pathlib import Path
import click
from PySide6.QtWidgets import QApplication

import config
from db.session import init_db, make_session_factory
from db.repository import Repository
from gui.main_window import MainWindow
from services.headless_runner import run_headless


@click.group()
def cli():
    pass


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
        window.show()
        app.exec()
    finally:
        session.close()
    logging.info("GUI closed")


@cli.command()
@click.argument(
    "root_dir", type=click.Path(exists=True, file_okay=False, path_type=Path)
)
def headless(root_dir: Path):
    logging.info(f"Running in headless mode with root_dir={root_dir}")
    root_dir = root_dir.resolve()
    config.init_config(root_dir)

    run_headless(
        root_dir=root_dir,
    )

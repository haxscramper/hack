import logging
from pathlib import Path
import click

from config import AppConfig
from ocr_pipeline import run_headless_pipeline

# Setup logging
logging.basicConfig(
    level=logging.INFO, 
    format='%(asctime)s | %(levelname)s | %(message)s',
    datefmt='%H:%M:%S'
)
logger = logging.getLogger(__name__)

@click.group()
def cli():
    """PDF Converter and OCR Tool."""
    pass

@cli.command()
@click.option('--config', '-c', type=click.Path(exists=True, path_type=Path), default=Path("config.json"), help="Path to config file.")
def headless(config: Path):
    """Run the headless OCR pipeline using the configuration file."""
    logger.info(f"Starting headless pipeline with config: {config}")
    app_config = AppConfig.load_from_json(config)
    run_headless_pipeline(app_config)

@cli.command()
@click.option('--config', '-c', type=click.Path(exists=True, path_type=Path), default=Path("config.json"), help="Path to config file.")
def gui(config: Path):
    """Launch the PySide6 GUI."""
    logger.info(f"Starting GUI with config: {config}")
    # GUI launch logic will go here
    print("GUI implementation pending...")

if __name__ == '__main__':
    cli()

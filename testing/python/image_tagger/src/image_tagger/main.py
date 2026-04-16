from beartype import beartype
import logging
from image_tagger.cli.commands import cli

logging.basicConfig(
    level=logging.INFO, format="%(levelname)s %(filename)s:%(lineno)d: %(message)s"
)


def main():
    logging.info("Application starting")
    cli()
    logging.info("Application exiting")


if __name__ == "__main__":
    main()

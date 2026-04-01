import logging
from cli.commands import cli

logging.basicConfig(
    level=logging.INFO, format="%(asctime)s - %(name)s - %(levelname)s - %(message)s"
)


def main():
    logging.info("Application starting")
    cli()
    logging.info("Application exiting")


if __name__ == "__main__":
    main()

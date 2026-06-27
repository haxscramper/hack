import logging

from beartype import beartype


@beartype
def configure_logging() -> None:
    logging.basicConfig(
        level=logging.DEBUG,
        format="%(levelname)s %(name)s %(filename)s:%(lineno)d: %(message)s",
    )

    for name in logging.root.manager.loggerDict:
        if name.startswith("dagster"):
            logging.getLogger(name).setLevel(logging.WARNING)

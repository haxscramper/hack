#!/usr/bin/env python

from argparse import ArgumentParser
from pathlib import Path
from beartype import beartype

from custom_input.application import Application
from custom_input.config import load_devices
from custom_input.actions import registry


@beartype
def main() -> None:
    parser = ArgumentParser()
    parser.add_argument(
        "config",
        nargs="?",
        type=Path,
        default=Path("custom_input.yaml"),
    )
    arguments = parser.parse_args()

    devices = load_devices(arguments.config)
    application = Application(devices)
    application.run()


if __name__ == "__main__":
    main()

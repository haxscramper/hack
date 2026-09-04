#!/usr/bin/env python

import grp
import os
import pwd
import sys
from pathlib import Path

from evdev import InputDevice, categorize
from loguru import logger


def log_device_options() -> None:
    username = pwd.getpwuid(os.getuid()).pw_name
    input_group = grp.getgrnam("input")
    active_group_ids = {os.getgid(), *os.getgroups()}

    if input_group.gr_gid not in active_group_ids:
        logger.info(
            f"Add the current user to the input group: sudo usermod -aG input {username}"
        )
        logger.info(
            "Log out and log back in for the group change to take effect.")

    devices = sorted(Path("/dev/input/by-id").glob("*-event-kbd"))

    if not devices:
        logger.info(
            "No keyboard input devices were found in /dev/input/by-id.")
    else:
        logger.info("Possible input device arguments:")
        for device in devices:
            logger.info(f"  {device}")
            logger.info(f"  Inspect events with: evtest {device}")


if len(sys.argv) < 2:
    log_device_options()
    raise SystemExit(1)

device = InputDevice(sys.argv[1])

with device.grab_context():
    for event in device.read_loop():
        logger.info(f"{categorize(event)}")

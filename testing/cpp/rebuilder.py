#!/usr/bin/env python
#
import sys
import subprocess
import readchar
import rich_click as click
import select
import termios
import tty
from pathlib import Path
from threading import Thread
import os
from typing import *

from watchdog.observers import Observer
from watchdog.events import FileSystemEventHandler
from PyQt5.QtCore import QObject, pyqtSignal, QCoreApplication, QThread, pyqtSlot

from pathspec import PathSpec
from pathspec.patterns import GitWildMatchPattern

# Replace this with your desired directories to watch
directories_to_watch = [os.getcwd()]

import logging
from rich.logging import RichHandler

logging.basicConfig(
    level="NOTSET",
    format="%(message)s",
    datefmt="[%X]",
    handlers=[
        RichHandler(
            rich_tracebacks=True,
            markup=True,
            enable_link_path=False,
            show_time=False,
        )
    ],
)

for name in logging.root.manager.loggerDict:
    logger = logging.getLogger(name)
    logger.setLevel(logging.WARNING)

    log = logging.getLogger("rich")
    log.setLevel(logging.DEBUG)

    log = logging.getLogger("rich")
    log.setLevel(logging.DEBUG)

log = logging.getLogger("rich")


class CommandExecutor(QObject):
    started = pyqtSignal()
    finished = pyqtSignal()
    error_occurred = pyqtSignal(str)
    exit_app = pyqtSignal()
    execute = pyqtSignal()

    def __init__(self, commands):
        super().__init__()
        self.commands = commands
        self.current_process = None

    @pyqtSlot()
    def execute_commands(self):
        self.started.emit()
        for command in self.commands:
            self.current_process = subprocess.Popen(command, stdout=sys.stdout, stderr=sys.stderr)

            try:
                return_code = self.current_process.wait()
            except KeyboardInterrupt:
                self.current_process.terminate()
                self.error_occurred.emit(f"Interrupted command: {' '.join(command)}")
                break

            if return_code != 0:
                self.error_occurred.emit(f"Failed command: {' '.join(command)}")
                break
        self.finished.emit()

    @pyqtSlot()
    def kill_current_command(self):
        if self.current_process and self.current_process.poll() is None:
            self.current_process.terminate()
            self.error_occurred.emit(f"Killed command: {' '.join(self.current_process.args)}")

    @pyqtSlot()
    def restart_commands(self):
        log.warning("Requested command restart")
        self.kill_current_command()
        self.execute_commands()


class CustomFileSystemEventHandler(FileSystemEventHandler):
    def __init__(self, executor):
        super().__init__()
        self.executor = executor

    def on_any_event(self, event):
        if event.event_type in ["modified", "created", "deleted"]:
            log.info(f"Directory event {event}")
            log.info(f"{type(event.event_type)}")
            self.executor.execute.emit()


class DirectoryWatcher(QObject):

    directory_changed = pyqtSignal()

    def __init__(self, directories):
        super().__init__()
        self.directories = directories
        self.event_handler = FileSystemEventHandler()
        self.event_handler.on_modified = self.on_modified
        self.observer = Observer()

        # Load ignore rules from .gitignore and .fdignore files
        self.ignore_patterns = self.load_ignore_patterns()

    def load_ignore_patterns(self):
        patterns = []
        for file_name in ['.gitignore', '.fdignore']:
            try:
                with open(file_name, 'r') as f:
                    file_patterns = f.read().splitlines()
                patterns.extend(file_patterns)
            except FileNotFoundError:
                pass

        return PathSpec.from_lines(GitWildMatchPattern, patterns)

    def start(self):
        for directory in self.directories:
            self.observer.schedule(self.event_handler, directory, recursive=True)
        self.observer.start()

    def stop(self):
        self.observer.stop()
        self.observer.join()

    def on_modified(self, event):
        if not event.is_directory and not self.ignore_patterns.match_file(event.src_path):
            self.directory_changed.emit()

def set_raw_mode(file_descriptor):
    original_settings = termios.tcgetattr(file_descriptor)
    tty.setraw(file_descriptor)
    return original_settings

def restore_mode(file_descriptor, original_settings):
    termios.tcsetattr(file_descriptor, termios.TCSADRAIN, original_settings)

def watch_space_key(executor):
    file_descriptor = sys.stdin.fileno()
    # original_settings = set_raw_mode(file_descriptor)

    try:
        while True:
            key = readchar.readkey()
            log.info(f"Key: [{key}, {ord(key)}]")
            if key == ' ':
                executor.execute.emit()


    except KeyboardInterrupt:
        log.debug("Interrup acceped, exiting")
        executor.exit_app.emit()  # Emit exit_app signal

    finally:
        pass
        # restore_mode(file_descriptor, original_settings)

def start_main_loop(commands: List[List[str]]):
    log.info("Starting listener")
    app = QCoreApplication([sys.argv[0]])
    # Replace this with the shell commands you want to execute
    executor = CommandExecutor(commands)
    executor.exit_app.connect(app.quit)
    executor.execute.connect(executor.restart_commands)

    # Watch directories for filesystem events
    dir_watcher = Thread(
        target=watch_directories, args=(executor,), daemon=True
    )
    dir_watcher.start()

    # Watch for space key press in the terminal
    space_key_watcher = Thread(
        target=watch_space_key, args=(executor,), daemon=True
    )
    space_key_watcher.start()

    sys.exit(app.exec_())



@click.group()
def cli():
    pass

@cli.command("cpp")
def exec_cpp():
    print("executing CPP")

def main():
    cli()


if __name__ == "__main__":
    main()

#!/usr/bin/env python
from conans import ConanFile

from conans.client.output import ConanOutput

from typing import List

import toml
import os
import sys

class WithWrap(object):
    def __init__(self, arg):
        self.arg = arg

    def __enter__(self):
        return self.arg

    def __exit__(self, exception_type, exception_value, traceback):
        pass

def configError(msg: str) -> None:
    print("error", msg)

def configLog(msg: str) -> None:
    print("log", msg)

def configWarn(msg: str) -> None:
    print("wrn", msg)

class FileCopyConfig(object):
    fileGlob: str = ""
    destination: str = ""
    def __init__(self, glob: str, dest: str):
        self.fileGlob = glob
        self.destination = dest

class TomlConfPackage(ConanFile):
    conf = {}
    "Internal variable - parsed toml configuration file"

    packaging_rules: List[FileCopyConfig] = []
    "List of files that need to be copied into generated package"

    name: str = ""
    "Name of the package to be exported"
    version: str = ""
    "Version of the package"
    user: str = ""
    "User name to use when uploading"
    channel: str = ""
    "Channel to use when uploading"

    header_folder: str = ""
    "Location of header files in exported project"
    library_folder: str = ""
    "Location of the library files (.a) in exported project"
    libtype: str = ""
    "Type of the library (static/shared/header-only/simple-linked etc.)"

    def __init__(self, output, runner,
                 display_name = "", user = None, channel = None):

        super().__init__(output, runner, display_name, user, channel)
        if hasattr(self, "conffile"):
            configLog(f"Using custom config file {self.conffile}")
            self.read_config(self.conffile)
        else:
            configWarn("Using default configurat file 'conffile.toml'")
            self.read_config("conffile.toml")

    def debugrun(self, command: str) -> None:
        print("run:", command)
        print("cwd:", os.getcwd())
        self.run(command)
        print("---")

    def read_config(self, path):
        self.conf = toml.load(path)

        with WithWrap(self.conf["pckgconfig"]) as pconf:
            if "name" in pconf:
                self.name = pconf["name"]
            else:
                configError("Missing 'name' from package configuration")

            if "version" in pconf:
                self.version = pconf["version"]
            else:
                configError("Missing 'version' from package configuration")

            # TODO Add description, settings, options etc.

        with WithWrap(self.conf["buildconfig"]) as bconf:
            if "copy_to_build" in bconf:
                self.exports_sources = bconf["copy_to_build"]
                print("Files copied to build", self.exports_sources)

            if "build_system" in bconf:
                system = bconf["build_system"]
                if system in [ "qmake" ]:
                    self.generators = system
                    configLog(f"Using {system} for building")
                else:
                    configError(f"Unknown build system {system}")


        with WithWrap(self.conf["exportconfig"]) as econf:
            ### --- Header folder configuration ---
            if "header_folder" in econf:
                self.header_folder = econf["header_folder"]
                configLog(f"Using customized header folder: '{self.header_folder}'")
            else:
                self.header_folder = "include/" + self.name
                configWarn(
                    f"Using default name for header folder: '{self.header_folder}'")

            if "header_globs" in econf:
                for item in econf["header_globs"]:
                    self.packaging_rules.append(
                        FileCopyConfig(glob = item, dest = self.header_folder))

            else:
                configWarn(
                    "Missing library globs - library won't export header files")


            ### --- Library folder configuration ---
            if "library_folder" in econf:
                self.library_folder = econf["library_folder"]
                configLog(f"Using customized library folder: '{self.library_folder}'")
            else:
                self.library_folder = "lib"
                configWarn(
                    f"Using default name for library folder: '{self.library_folder}'")

            if "library_globs" in econf:
                for item in econf["library_globs"]:
                    self.packaging_rules.append(
                        FileCopyConfig(glob = item, dest = self.library_folder))

            elif self.libtype in [ "static", "shared" ]:
                configWarn("No library files exported for static library")

    def package(self):
        for item in self.packaging_rules:
            self.copy(item.fileGlob, dst=item.destination)

    def build(self):
        found_qmake = False
        print("content of the build directory:")
        for root, dirs, files in os.walk("."):
            for name in files:
                print(name)
                if name.endswith(".pro"):
                    found_qmake = True


        if found_qmake:
            self.debugrun("qmake")
        else:
            conanfile_dir = os.path.dirname(os.getcwd())
            # os.path.dirname(os.path.realpath(__file__))
            self.debugrun(
                "qmake " + os.path.join(conanfile_dir, self.name + ".pro"))

        self.run("make --quiet -j4")

    def package_info(self):
        self.cpp_info.libs = [self.name]


if __name__ == '__main__':
    output = ConanOutput(sys.stdout)
    test = TomlConfPackage(output = output, runner = None)

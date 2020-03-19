#!/usr/bin/env python

from conans import ConanFile

common = __import__("conan_toml")

# if __name__ == '__main__':
#     common = __import__("conan_toml")
# else:
#     from conans import python_requires
#     common = python_requires("conan_toml/0.1@demo/testing")

class ConanTomlExample(common.TomlConfPackage):
    pass

if __name__ == '__main__':
    from conans.client.output import ConanOutput
    import sys

    output = ConanOutput(sys.stdout)
    test = ConanTomlExample(output = output, runner = None)

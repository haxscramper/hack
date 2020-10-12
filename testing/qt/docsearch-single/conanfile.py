common = __import__("conan_toml")
class ConanToml(common.TomlConfPackage):
    config_copy = """[meta]
language = "c++"

[pkgconfig]
name = "fuzzywidget"
version = "0.2"
user = "author"
channel = "testing"
package_type = "library"

[buildconfig]
package_manager = "conan"
conan_build_folder = "build"
conan_reqs_folder = "conan"
build_system = "qmake"
copy_to_build = [ "*.hpp", "*.cpp", "*.pro" ]
libtype = "static"

[exportconfig]
library_globs = [ "*.a" ]
header_folder = "include/fuzzywidget"
header_globs = [ "*.hpp" ]
    
"""
  
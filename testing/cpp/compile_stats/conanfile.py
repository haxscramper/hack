from conan import ConanFile
from conan.tools.cmake import cmake_layout


class ExampleRecipe(ConanFile):
    settings = "os", "compiler", "build_type", "arch"
    generators = "CMakeDeps", "CMakeToolchain"

    def requirements(self):
        self.requires("rapidjson/1.1.0")
        self.requires("foonathan-lexy/2025.05.0")
        self.requires("fmt/12.1.0")
        self.requires("cpptrace/1.0.4")
        self.requires("quill/11.1.0")
        self.requires("duckdb/1.4.3")

    def layout(self):
        cmake_layout(self)

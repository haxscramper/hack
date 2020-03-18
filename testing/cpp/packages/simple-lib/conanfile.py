from conans import ConanFile, CMake, tools


class SimplelibConan(ConanFile):
    name = "simple-lib"
    version = "0.1"
    generators = "cmake"
    exports_sources = "*.cpp", "*.hpp", "CMakeLists.txt"

    def source(self):
        pass

    def build(self):
        cmake = CMake(self)
        cmake.configure()
        cmake.build()

    def package(self):
        self.copy("*.hpp", dst="include")
        self.copy("*.a", dst="lib")

    def package_info(self):
        self.cpp_info.libs = ["simple-lib"]


from conans import ConanFile, CMake, tools


class SimplelibConan(ConanFile):
    name = "simple-lib"
    version = "0.1"
    generators = "cmake"

    def source(self):
        pass

    def build(self):
        cmake = CMake(self)
        cmake.configure(source_folder=".")
        cmake.build()

    def package(self):
        self.copy("*.hpp", dst="include")
        self.copy("*.a", dst="lib")


    def package_info(self):
        self.cpp_info.libs = ["hello"]


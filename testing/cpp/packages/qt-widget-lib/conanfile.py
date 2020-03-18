from conans import ConanFile, tools
import os

class SimplelibConan(ConanFile):
    name = "qt-widget-lib"
    version = "0.1"
    generators = "qmake"
    # Set of files that should be exported together with the recipe and will be
    # available to generate the package.
    exports_sources = "*.cpp", "*.hpp", "*.pro"

    def debugrun(self, command: str) -> None:
        print("run:", command)
        print("cwd:", os.getcwd())
        self.run(command)
        print("---")

    def build(self):
        print("Current directory is", os.getcwd())
        found_qmake = False
        for root, dirs, files in os.walk("."):
            for name in files:
                if name.endswith(".pro"):
                    found_qmake = True

        conanfile_dir = os.path.dirname(os.path.realpath(__file__))

        if found_qmake:
            # Looks like we are doing `conan create`
            self.debugrun("qmake")
        else:
            # Fuck, we need to manually specify path to the conan root because
            # now we are certainly doing `conan build` - isn't it wonderful
            # that I have to use all this weird-ass bullshit to just find out
            # how to build my package?
            self.debugrun("qmake " + os.path.join(conanfile_dir, "qt-widget-lib.pro"))

        self.debugrun("make --quiet -j12")

    def package(self):
        self.copy("*.hpp", dst="include/libwidget")
        self.copy("*.a", dst="lib")

    def package_info(self):
        self.cpp_info.libs = ["qt-widget-lib"]


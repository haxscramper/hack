from ctypes import *
import os, sys

dir = os.path.dirname(__file__)
if sys.platform == "win32":
  libName = "genny_main.dll"
elif sys.platform == "darwin":
  libName = "libgenny_main.dylib"
else:
  libName = "libgenny_main.so"
dll = cdll.LoadLibrary(os.path.join(dir, libName))

class genny_mainError(Exception):
    pass

def print_text():
    dll.genny_main_print_text()

dll.genny_main_print_text.argtypes = []
dll.genny_main_print_text.restype = None

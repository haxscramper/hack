from ctypes import *
import os, sys

dir = os.path.dirname(sys.modules["genny_main"].__file__)
if sys.platform == "win32":
  libName = "genny_main.dll"
elif sys.platform == "darwin":
  libName = "libgenny_main.dylib"
else:
  libName = "libgenny_main.so"
dll = cdll.LoadLibrary(os.path.join(dir, libName))

class genny_mainError(Exception):
    pass

class Obj(Structure): # python:211
    _fields_ = [
        ("field", c_longlong) # python:219
    ]

    def __init__(self, field): # python:254
        self.field = field

    def __eq__(self, obj): # python:262
        return self.field == obj.field

    def get_data(self): # python:138
        result = dll.genny_main_obj_get_data(self)
        return result

dll.genny_main_obj_get_data.argtypes = [Obj]
dll.genny_main_obj_get_data.restype = c_longlong


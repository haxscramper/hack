#include <Python.h>
#include <frameobject.h>

// HACK because python API does not allow for custom user data passing, so
// I have to
static PyModuleDef module;
static PyObject*   PyInit_sapr(void) {
    return PyModule_Create(&module);
}

void eval(const char* text) {
    PyRun_SimpleString(text);
}

static PyObject* triggerFromPy(PyObject* self, PyObject* args) {
    puts("Function called from the python code");

    if (self == nullptr) {
        puts("SELF IS NIL");
    } else {
        printf("SELF: %s\n", _PyUnicode_AsString(PyObject_Repr(self)));

    }

    printf("ARGS: %s\n", _PyUnicode_AsString(PyObject_Repr(args)));

    PyThreadState* ts    = PyThreadState_Get();
    PyFrameObject* frame = ts->frame;
    while (frame != 0) {
        char const* filename = _PyUnicode_AsString(
            frame->f_code->co_filename);
        char const* name = _PyUnicode_AsString(frame->f_code->co_name);
        printf("called: filename=%s, name=%s\n", filename, name);
        frame = frame->f_back;
    }

    return Py_None;
}

/// Wrapper around dynamically allocated array of python methods. Used to
/// properly manage addition of new methods to the list. Use of std::vector
/// was not possible due to potential data block reallocations, which
/// cannot be accounted for in the Python API
struct MethodTable {
    PyMethodDef* methods;
    int          idx  = 0;
    const int    size = 256; /// Hardcoded array size, cannot be changed
        /// dynamically due to aforementioned constraints
        /// on reallocation.
    inline MethodTable() {
        methods = new PyMethodDef[size]();
    }
    /// Add new method to the method table
    inline void add(PyMethodDef def) {
        methods[idx] = def;
        ++idx;
    }
    inline ~MethodTable() {
        delete[] methods;
    }
};

int main() {
    MethodTable table;
    PyModuleDef sapr = {
        PyModuleDef_HEAD_INIT,
        "sapr",
        NULL,
        -1,
        // List of methods is null-terminated, which means I can
        // dynamically add to it at runtime.
        table.methods,
        NULL,
        NULL,
        NULL,
        NULL,
    };
    module = sapr;

    PyImport_AppendInittab("sapr", &PyInit_sapr);
    Py_Initialize();

    table.add(PyMethodDef{
        "trigger",
        triggerFromPy,
        METH_VARARGS,
        "Trigger function from python"});

    eval("import sapr");
    eval("sapr.trigger(12, 3, 4)");
    Py_Finalize();
}

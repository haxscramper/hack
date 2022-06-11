#include <Python.h>
#include <frameobject.h>
#include <vector>


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

    void* capsule = PyCapsule_Import(
        "sapr.__capsule", // MODULE.CAPSULE name
        0                 // This can be set to 0 almost all the time
    );
    if (capsule != nullptr) {
        printf("Capsule internal pointer: '%s'\n", (char*)capsule);
    }

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
    PyMethodDef* add(PyMethodDef def) {
        methods[idx] = def;
        auto pos     = idx;
        ++idx;
        return &methods[pos];
    }
    inline ~MethodTable() {
        delete[] methods;
    }
};

// HACK because python API does not allow for custom user data passing, so
// I have to
static PyModuleDef module;
static PyObject*   saprModule;
static PyObject*   PyInit_sapr(void) {
    saprModule = PyModule_Create(&module);
    return saprModule;
}

int main() {
    PyModuleDef sapr = {
        PyModuleDef_HEAD_INIT,
        "sapr",
        NULL,
        -1,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
    };
    module = sapr;

    PyImport_AppendInittab("sapr", &PyInit_sapr);
    Py_Initialize();
    eval("import sapr");

    // Provide user-defined data to the module, so wrapped functions could
    // properly import the necessary parts.
    PyModule_AddObject(
        saprModule,
        "__capsule",
        PyCapsule_New(
            (void*)"user data", // User pointer to `void*`
            "sapr.__capsule", // Capsule name must have the fully qualified
                              // path to it, even though it can be
                              // constructed from module again.
            nullptr));


    std::vector<PyMethodDef> methods;

    methods.push_back(PyMethodDef{
        "trigger",
        triggerFromPy,
        METH_VARARGS,
        "Trigger function from python"});

    PyModule_AddObject(
        saprModule,
        "trigger",
        (PyObject*)PyCFunction_New(&methods[0], nullptr));

    if (false) {
        // Test whether Python API can handle relocation (it can't)
        auto start      = &methods[0];
        auto iterations = 0;
        while (start == &methods[0]) {
            ++iterations;
            methods.push_back(PyMethodDef{});
        }

        printf(
            "Method relocation happened after %d iterations\n",
            iterations);
        fflush(stdout);
    }

    eval("sapr.trigger(12, 3, 4)");
    Py_Finalize();
}

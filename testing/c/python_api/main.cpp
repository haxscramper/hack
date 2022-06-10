#include <Python.h>

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
    return Py_None;
}

struct MethodTable {
    PyMethodDef* methods;
    int          idx = 0;
    const int    size = 256;
    MethodTable() {
        methods = new PyMethodDef[size]();
    }
    void add(PyMethodDef def) {
        methods[idx] = def;
        ++idx;
    }
    ~MethodTable() {
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
    eval("sapr.trigger()");
    Py_Finalize();
}

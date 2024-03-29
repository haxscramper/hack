#include <Python.h>
#include <frameobject.h>
#include <vector>
#include <QString>
#include <QDebug>


void eval(const char* text) { PyRun_SimpleString(text); }

QString qrepr(PyObject* obj) {
    return _PyUnicode_AsString(PyObject_Str(obj));
}

static PyObject* triggerFromPy(PyObject* self, PyObject* args) {
    puts("Function called from the python code");

    if (self == nullptr) {
        puts("SELF IS NIL");
    } else {
        printf("SELF: %s\n", _PyUnicode_AsString(PyObject_Repr(self)));
        auto data = (char*)PyLong_AsLongLong(self);
        qDebug() << "Input data: " << data;
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

static PyObject* qdebug_impl(PyObject*, PyObject* args) {
    qDebug().noquote() << "QDEBUG:" << qrepr(args);
    Py_RETURN_NONE;
}

// HACK because python API does not allow for custom user data passing, so
// I have to
static PyModuleDef module;
static PyObject*   saprModule;
static PyObject*   PyInit_sapr(void) {
    saprModule = PyModule_Create(&module);
    return saprModule;
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
    inline MethodTable() { methods = new PyMethodDef[size](); }
    /// Add new method to the method table
    PyMethodDef* add(PyMethodDef def) {
        methods[idx] = def;
        auto pos     = idx;
        ++idx;
        return &methods[pos];
    }
    inline ~MethodTable() { delete[] methods; }


    void add(
        const char* name,
        PyObject*   impl(PyObject* self, PyObject* args)) {
        auto value = "'self' value to pass in the function new";
        PyModule_AddObject(
            saprModule,
            name,
            (PyObject*)PyCFunction_New(
                add(PyMethodDef{
                    "trigger",
                    impl,
                    METH_VARARGS,
                    "Trigger function from python"}),
                PyLong_FromLongLong((long long)value)));
    }
};


void stmt(const QString& stmt) {
    PyRun_SimpleString(stmt.toLocal8Bit().data());
}

PyObject* expr(const QString& expr) {
    PyObject* code = Py_CompileString(
        expr.toLocal8Bit().data(), "test", Py_eval_input);
    PyObject* main_module = PyImport_AddModule("__main__");
    PyObject* global_dict = PyModule_GetDict(main_module);
    PyObject* local_dict  = PyDict_New();
    PyObject* obj         = PyEval_EvalCode(code, global_dict, local_dict);
    if (PyErr_Occurred()) { PyErr_Print(); }
    return obj;
}

void print(PyObject* it) {
    printf("PRINT: %s\n", _PyUnicode_AsString(PyObject_Str(it)));
}


int main() {
    qputenv("QT_ASSUME_STDERR_HAS_CONSOLE", "1");
    MethodTable table;
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

    table.add("trigger", triggerFromPy);
    table.add("qdebug_str", qdebug_impl);

    stmt(R"(
sapr.qdebug_str("Called debug function")

def float_value_afterchange():
    sapr.qdebug_str("Calling qdebug from the python code")
    sapr.qdebug_str("Calling qdebug from the python code")
    sapr.qdebug_str("Calling qdebug from the python code")

    return True

sapr.qdebug_str("After debug")

sapr.qdebug_str("After debug")
)");


    print(expr("12 + 12"));
    eval("sapr.trigger(12, 3, 4)");
    stmt("float_value_afterchange()");
    Py_Finalize();
}

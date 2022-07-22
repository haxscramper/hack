#include <Python.h>

static void print_subinterp(void) {
    // Get the state of the subinterpreter
    PyThreadState*      ts     = PyThreadState_Get();
    PyInterpreterState* interp = ts->interp;
    int64_t             id     = PyInterpreterState_GetID(interp);
    printf(
        "interp %" PRId64 " <0x%" PRIXPTR ">, thread state <0x%" PRIXPTR
        "> ",
        id,
        (uintptr_t)interp,
        (uintptr_t)ts);
    fflush(stdout);
    PyRun_SimpleString(
        "import sys;"
        "print('PYTHON: id(modules) =', id(sys.modules));"
        "sys.stdout.flush()");
}

static int test_repeated_init_and_subinterpreters(void) {
    // "This data structure represents the state of a single thread. The
    // only public data member is interp (PyInterpreterState*), which
    // points to this thread’s interpreter state."
    PyThreadState*   mainstate;
    PyThreadState*   substate;
    PyGILState_STATE gilstate;

    for (int i = 1; i <= 2; i++) {
        printf("--- Pass %d ---\n", i);
        // Initialize the whole 'embedded python' library
        Py_Initialize();
        // Get main thread state. GIL is held
        mainstate = PyThreadState_Get();

        // Release the current thread state and GIL. GIL is no longer being
        // held
        PyEval_ReleaseThread(mainstate);

        // "Ensure that the current thread is ready to call the Python C
        // API regardless of the current state of Python, or of the global
        // interpreter lock."
        gilstate = PyGILState_Ensure();
        print_subinterp();
        // "Swap the current thread state with the thread state given by
        // the argument tstate, which may be NULL. The global interpreter
        // lock must be held and is not released."
        PyThreadState_Swap(NULL);

        for (int j = 0; j < 2; j++) {
            // Create new sub-interpreter thread state. Set the current
            // thread state as a new interpreter and return the created
            // thread as well.
            substate = Py_NewInterpreter();
            // Reset the current interpreter thread
            PyThreadState_Swap(NULL);
            // Set the subinterpreter thread state
            PyThreadState_Swap(substate);
            // Currently in the new interpreter thread state
            print_subinterp();
            // "Destroy the (sub-)interpreter represented by the given
            // thread state. The given thread state must be the current
            // thread state. When the call returns, the current thread
            // state is NULL."
            Py_EndInterpreter(substate);
        }

        PyThreadState_Swap(mainstate);
        print_subinterp();
        PyGILState_Release(gilstate);

        PyEval_RestoreThread(mainstate);
        Py_Finalize();
    }
    return 0;
}

int main() { test_repeated_init_and_subinterpreters(); }

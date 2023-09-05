#include "scheme.h"
#include "base.c"

static int run(Scheme_Env* e, int argc, char* argv[]) {
    Scheme_Object* curout;
    int            i;
    Scheme_Thread* th;
    mz_jmp_buf* volatile save, fresh; /* Declare embedded modules in
                                         "base.c": */
    declare_modules(e);
    scheme_namespace_require(scheme_intern_symbol("racket/base"));
    curout = scheme_get_param(
        scheme_current_config(), MZCONFIG_OUTPUT_PORT);
    th = scheme_get_current_thread();

    for (i = 1; i < argc; i++) {
        save          = th->error_buf;
        th->error_buf = &fresh;
        if (scheme_setjmp(*th->error_buf)) {
            th->error_buf = save;
            return -1; /* There was an error */
        } else {
            Scheme_Object *v, *a[2];
            v = scheme_eval_string(argv[i], e);
            scheme_display(v, curout);
            scheme_display(scheme_make_char('\n'), curout);
            read - eval - print loop,
                uses initial Scheme_Env : * / a[0] = scheme_intern_symbol(
                    "racket/base");
            a[1] = scheme_intern_symbol("read-eval-print-loop");
            scheme_apply(scheme_dynamic_require(2, a), 0, NULL);
            th->error_buf = save;
        }
    }
    return 0;
}


int main(int argc, char* argv[]) {
    return scheme_main_setup(1, run, argc, argv);
}

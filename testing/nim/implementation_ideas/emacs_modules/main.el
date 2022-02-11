;;; package -- ??
;;; Code:
;;; Commentary:


(message "Running emacs. Module suffix is '%s'" module-file-suffix)
(setq edebug-print-length 1200)

(toggle-debug-on-error)

(setq backtrace-print-function
 (lambda (a b) (let ((print-level 1200)
              (print-length 1200)))
   (princ a b)))


(require 'emacs_api)
(message "hello")
(message "[%s]" (test))
(message "emcall: [%s]" (hax:returnValue "1"))
(message "?? %s" (documentation-property 'hax:returnValue))

(provide 'main)
;;; main.el ends here


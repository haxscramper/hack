;;; package -- ??
;;; Code:
;;; Commentary:

(toggle-debug-on-error)

(setq backtrace-print-function
 (lambda (a b) (let ((print-level 1200)
              (print-length 1200)))
   (princ a b)))


(require 'module)
(message "hello")
(message "[%s]" (test))
(message "emcall: [%s]" (hax:bind 1))

(provide 'main)
;;; main.el ends here


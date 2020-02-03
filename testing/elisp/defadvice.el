:;exec emacs -batch -l "$0" -f main "$@"

(toggle-debug-on-error)

(defun test-function (arg)
  (message "function called with ('%s')" arg)
  arg)

(advice-add
 'test-function
 :before (lambda (arg)
           (message "before: '%s'" arg)))

(advice-add
 'test-function
 :after (lambda (arg)
          (message "after: %s'" arg)))

(advice-add
 'test-function
 :filter-args (lambda (arg)
                (message "args: '%s'" arg)
                '("different argument")))

(defun main ()
  (test-function "starting argument")
  )

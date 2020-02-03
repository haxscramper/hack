:;exec emacs -batch -l "$0" -f main "$@"

(toggle-debug-on-error)

(defvar test-string "20:12\n1231\n19:20\n12")

(defun hax/format-from-dynalist (input-string)
  (with-temp-buffer
    (insert input-string)
    (fill-region (point-min) (point-max))
    (insert (let ((str (buffer-string)))
              (erase-buffer)
              (replace-regexp-in-string
               (rx ;; bol
                (group (= 2 digit)) ":"
                (group (= 2 digit)) (* space)
                ;; (group (* (not (any "\n"))))
                ;; eol
                )
               "\n\n@time:\\1:\\2;\n\\3\n"
               str)))
    (buffer-string)))

(defun main () (message (hax/format-from-dynalist test-string)))

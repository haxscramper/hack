:;exec emacs -batch -l "$0" -f main "$@"

(toggle-debug-on-error)

(add-to-list 'load-path "~/.config/hax-software/emacs")

(require 'org-element)
(require 'elisp-format)

(defun parse-org-file (file-name)
  (with-temp-buffer
    (insert-file-contents file-name)
    (org-mode)
    (org-element-parse-buffer)))

(defun main ()
  (let ((tree (parse-org-file "todo_graph.org"))
        (dump-file "tree.tmp.el"))
    (with-temp-file dump-file
      (insert (with-output-to-string (pp tree)))
      (emacs-lisp-mode)
      (elisp-format-buffer)
      (delete-trailing-whitespace)
      )
    (org-element-map tree 'headline
      (lambda (h)
        (let* ((name "dd")
               (deps (org-element-property :DEPENDS h))
               (id (org-element-property :CUSTOM_ID h)))
          (message "Item %s depends on %s" name deps))))))

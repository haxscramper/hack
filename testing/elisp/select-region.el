:;exec emacs -batch -l "$0" -f main "$@"


(add-to-list 'load-path "~/.config/hax-software/emacs")
(add-to-list 'load-path "~/.emacs.d/elpa/develop/")
(add-to-list 'load-path "~/.emacs.d/elpa/develop/s-20180406.808/")
(require 's)

(setq hl-todo-keyword-faces
      (quote
       (("TODO" . "#dc752f")
        ("NEXT" . "#dc752f")
        ("THEM" . "#2d9574")
        ("PROG" . "#4f97d7")
        ("OKAY" . "#4f97d7")
        ("REVIEW" . "#4f97d7")
        ("IDEA" . "#4f97d7")
        ("REFACTOR" . "#4f97d7")
        ("DONT" . "#f2241f")
        ("DOC" . "#f2241f")
        ("FAIL" . "#f2241f")
        ("TEST" . "#f2241f")
        ("WARNING" . "#f2241f")
        ("IMPLEMENT" . "#f2241f")
        ("DONE" . "#86dc2f")
        ("NOTE" . "#b1951d")
        ("QUESTION" . "#b1951d")
        ("KLUDGE" . "#b1951d")
        ("HACK" . "#b1951d")
        ("TEMP" . "#9932cc")
        ("FIXME" . "#dc752f")
        ("XXX" . "#a52a2a")
        ("XXXX" . "#b22222"))))


(defun main()
  (with-temp-buffer
    (insert (s-join
             ","
             (mapcar (lambda (hl-kwd) (car hl-kwd))
                     hl-todo-keyword-faces)))
    (insert "
  # TODO add proc for checking if futher recursion is not needed (trim
  # down arbitrary branches from tree)
  # TODO return `Option[OutTree]` from map function. Support both
  # versions: return-all and return-option (NOTE: can be determined
  # using typeof `op`)
  # NOTE `subnodeCall` does not feel intuitive - injecting current
  # node into scope will be better.
  # TEST - either write unit tests or chec if existing ones cover this
  #      explicitly
  # IDEA TEST write example/test for mapping tree to DFS sequence
  # TODO predicate to check if item has subnodes or not.
  # TEST predicated for subnodes checking
")

    (shell-command-on-region
     (point-min) (point-max) "./format_comment.nim.bin"
     t t)
    (message (buffer-substring (point-min) (point-max)))))

(defun get-time-to-deadlines ()
  (org-element-map
      (with-temp-buffer
        (insert-file-contents (concat hax/file::org-dir "/distant.org"))
        (org-mode)
        (org-element-parse-buffer))
      'headline
    (lambda (h)
      (let*
          ((deadline (org-element-property :deadline h))
           (headline (org-element-property :raw-value h))
           (timestamp (plist-get deadline 'timestamp))
           )
        (list (cons :headline headline)
              (cons :days-until (plist-get timestamp :raw-value))
              )
        ))))

(with-output-to-temp-buffer "*org-parsing*" (princ (get-time-to-deadlines)))

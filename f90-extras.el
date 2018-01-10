(defun f90-fix-formatting ()
  "Fix some "
  (interactive)
  (save-excursion
    ;; Fix indentation
    (f90-indent-subprogram)

    ;; Now only operate on this procedure
    (f90-mark-subprogram)
    (narrow-to-region (region-beginning) (region-end))
    (deactivate-mark)

    ;; Fix uppercase keywords
    (f90-mark-subprogram)
    (f90-downcase-region-keywords (region-beginning) (region-end))
    (deactivate-mark)

    ;; Fix spaces around equals
    (f90-beginning-of-subprogram)
    (while (re-search-forward "\\([^=*>/ ]\\)\\([=+*/-]\\)\\([^=* ]\\)" nil t)
      (replace-match "\\1 \\2 \\3"))

    ;; Fix spaces after commas
    (f90-beginning-of-subprogram)
    (while (re-search-forward ",\\b" nil t)
      (replace-match ", "))

    ;; Fix multiple spaces before double-colon
    (f90-beginning-of-subprogram)
    (while (re-search-forward " \\{2,\\}::" nil t)
      (replace-match " ::"))

    ;; Fix spaces around if statements
    (f90-beginning-of-subprogram)
    (while (re-search-forward "if\\((.*?)\\)then" nil t)
      (replace-match "if \\1 then"))

    ;; Fix bad comments
    (f90-beginning-of-subprogram)
    (while (search-forward "!#" nil t)
      (replace-match "!" nil t))

    ;; Back to whole buffer
    (widen)))

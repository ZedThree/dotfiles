(defun f90-fix-formatting ()
  "Fix some "
  (interactive)
  (save-excursion
    ;; Make sure procedure header is indented correctly
    (f90-beginning-of-subprogram)
    (f90-indent-line)

    ;; Now only operate on this procedure
    (forward-line)
    (narrow-to-defun)

    ;; Fix indentation
    (f90-indent-subprogram)

    ;; Fix uppercase keywords
    (f90-mark-subprogram)
    (f90-downcase-region-keywords (region-beginning) (region-end))

    ;; Fix spaces around equals
    (beginning-of-buffer)
    (while (re-search-forward "\\([^=*>/ ]\\)\\([=+*/-]\\)\\([^=* ]\\)" nil t)
      (replace-match "\\1 \\2 \\3"))

    ;; Fix spaces after commas
    (beginning-of-buffer)
    (while (re-search-forward ",\\b" nil t)
      (replace-match ", "))

    ;; Fix multiple spaces before double-colon
    (beginning-of-buffer)
    (while (re-search-forward " \\{2,\\}::" nil t)
      (replace-match " ::"))

    ;; Fix spaces around if statements
    (beginning-of-buffer)
    (while (re-search-forward "if\\((.*?)\\)then" nil t)
      (replace-match "if \\1 then"))

    ;; Fix bad comments
    (beginning-of-buffer)
    (while (search-forward "!#" nil t)
      (replace-match "!" nil t))

    ;; Back to whole buffer
    (widen)))

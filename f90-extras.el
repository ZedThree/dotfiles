(defun f90-fix-formatting-region (start end)
  "Fix some common Fortran formatting problems in region"
  (interactive "r")
  (save-excursion
    ;; Fix indentation
    (f90-indent-region start end)

    ;; Now only operate on this procedure
    (narrow-to-region start end)

    ;; Fix uppercase keywords
    (f90-downcase-region-keywords start end)

    ;; Remove unnecessary whitespace
    (whitespace-cleanup-region start end)

    ;; Fix spaces around equals
    (beginning-of-buffer)
    (while (re-search-forward "\\([^=*>/ ]\\)\\([-=+*/<>]\\|[<>/=]=\\)\\([^=* ]\\)" nil t)
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

(defun f90-fix-formatting-procedure ()
  "Fix some common Fortran formatting problems in current procedure"
  (interactive)
  (save-excursion
    (let ((start (progn
                   (f90-beginning-of-subprogram)
                   (point)))
          (end (progn
                 (f90-end-of-subprogram)
                 (point))))
      (f90-fix-formatting-region start end))))

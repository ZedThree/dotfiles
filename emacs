;; colour schemes
;;(add-to-list 'load-path "/home/peter/emacs_colours/sellout-emacs-color-theme-solarized-26260c0/")
(add-to-list 'load-path "~/emacs_colours/")
(add-to-list 'load-path "~/emacs_colours/color-theme-6.6.0/")

;; start in savehist mode
(savehist-mode 1)

;; matlab-mode stuff
(autoload 'matlab-mode "~/matlab.el" "Enter Matlab mode." t)
(setq auto-mode-alist (cons '("\\.m\\'" . matlab-mode) auto-mode-alist))
(autoload 'matlab-shell "~/matlab.el" "Interactive Matlab mode." t)
(setq auto-mode-alist (cons '("\\.F90$" . f90-mode) auto-mode-alist))
(defun my-matlab-mode-hook ()  (setq fill-column 90))
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(delete-selection-mode nil)
 '(font-latex-match-reference-keywords (quote (("Cref" "{") ("cref" "{") ("autoref" "{"))))
 '(inhibit-startup-screen t)
 '(linum-format "%d ")
 '(mark-even-if-inactive t)
 '(reb-re-syntax (quote string))
 '(safe-local-variable-values (quote ((TeX-master . t) (TeX-master . "thesis"))))
 '(scroll-bar-mode (quote right))
 '(transient-mark-mode 1))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(linum ((t (:inherit (shadow default) :foreground "black")))))
(setq read-file-name-completion-ignore-case t)
(setq compile-command "pdflatex *.tex")

(eval-after-load "tex"'(TeX-add-style-hook "beamer" 'my-beamer-mode))

(setq TeX-region "regionsje")
(defun my-beamer-mode ()
"My adds on for when in beamer."

;; when in a Beamer file I want to use pdflatex.
;; Thanks to Ralf Angeli for this.
(TeX-PDF-mode 1) ;turn on PDF mode.

;; Tell reftex to treat \lecture and \frametitle as section commands
;; so that C-c = gives you a list of frametitles and you can easily
;; navigate around the list of frames.
;; If you change reftex-section-level, reftex needs to be reset so that
;; reftex-section-regexp is correctly remade.
(require 'reftex)
(set (make-local-variable 'reftex-section-levels)
'(("lecture" . 1) ("frametitle" . 2)))
(reftex-reset-mode)

;; add some extra functions.
(define-key LaTeX-mode-map "\C-cf" 'beamer-template-frame)
(define-key LaTeX-mode-map "\C-\M-x" 'tex-frame)
)

(defun tex-frame ()
"Run pdflatex on current frame. 
Frame must be declared as an environment."
(interactive)
(let (beg)
(save-excursion
(search-backward "\\begin{frame}")
(setq beg (point))
(forward-char 1)
(LaTeX-find-matching-end)
(TeX-pin-region beg (point))
(letf (( (symbol-function 'TeX-command-query) (lambda (x) "LaTeX")))
(TeX-command-region))
)
))


(defun beamer-template-frame ()
"Create a simple template and move point to after \\frametitle."
(interactive)
(latex-insert-block "frame")
(insert "\\frametitle{}")
(backward-char 1))

(define-key minibuffer-local-map
  [f3] (lambda () (interactive) 
       (insert (buffer-name (current-buffer-not-mini)))))

(defun current-buffer-not-mini ()
  "Return current-buffer if current buffer is not the *mini-buffer*
  else return buffer before minibuf is activated."
  (if (not (window-minibuffer-p)) (current-buffer)
      (if (eq (get-lru-window) (next-window))
	    (window-buffer (previous-window)) (window-buffer (next-window)))))

;; Start auctex automatically.
(load "auctex.el" nil t t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

;; reftex
(require 'tex-site)
(autoload 'reftex-mode "reftex" "RefTeX Minor Mode" t)
(autoload 'turn-on-reftex "reftex" "RefTeX Minor Mode" nil)
(autoload 'reftex-citation "reftex-cite" "Make citation" nil)
(autoload 'reftex-index-phrase-mode "reftex-index" "Phrase Mode" t)
(add-hook 'latex-mode-hook 'turn-on-reftex) ; with Emacs latex mode
;; (add-hook 'reftex-load-hook 'imenu-add-menubar-index)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
;; changes \ref to \cref when inserting a reference
(defun reftex-format-cref (label def-fmt)
  (format "\\cref{%s}" label))
(setq reftex-format-ref-function 'reftex-format-cref)

;; Spell-checking on the fly
(add-hook 'LaTeX-mode-hook 'flyspell-mode)

;; linum mode
(require 'linum)
(global-linum-mode)
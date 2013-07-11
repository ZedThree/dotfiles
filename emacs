;; colour schemes
(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/Tools/emacs-color-theme-solarized")
(require 'color-theme)
(require 'color-theme-solarized)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-solarized-dark)))

;; start in savehist mode
(savehist-mode 1)

;; Display line and column numbers on the status line
(setq line-number-mode   t)
(setq column-number-mode t)

;; Turn off menu-bar and tool-bar
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Always end a file with a newline
(setq require-final-newline t)

;; Ordinarily emacs jumps by half a page when scrolling - reduce this to 1 line
(setq scroll-step 1)

;; When saving files, set execute permission if #! is in first line.
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; When saving files, delete any trailing whitespace.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Set MajorMode preferences based on filenames
(setq auto-mode-alist
      (append
       '(("\\emacs\\'" . emacs-lisp-mode)
	 ("\\.F90\\'"  . f90-mode)
	 ("\\.f03\\'"  . f90-mode)
	 ("\\.m\\'"    . matlab-mode))
       auto-mode-alist))

;; Follow symlinks
(setq vc-follow-symlinks nil)

;; matlab-mode stuff
(autoload 'matlab-mode "~/.emacs.d/matlab.el" "Enter Matlab mode." t)
(autoload 'matlab-shell "~/emacs.d/matlab.el" "Interactive Matlab mode." t)
(defun my-matlab-mode-hook ()  (setq fill-column 90))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(delete-selection-mode nil)
 '(f90-auto-keyword-case (quote upcase-word))
 '(font-latex-match-reference-keywords (quote (("Cref" "{") ("cref" "{") ("autoref" "{"))))
 '(inhibit-startup-screen t)
 '(linum-format "%d ")
 '(mark-even-if-inactive t)
 '(org-agenda-files (quote ("/home/peter/Dropbox/orgmode/work.org" "/home/peter/Dropbox/orgmode/mylife.org")))
 '(org-agenda-ndays 7)
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-start-on-weekday nil)
 '(org-deadline-warning-days 14)
 '(org-default-notes-file "~/Dropbox/orgmode/notes.org")
 '(org-fast-tag-selection-single-key (quote expert))
 '(org-remember-store-without-prompt t)
 '(org-remember-templates (quote ((116 "* TODO %?
  %u" "~/Dropbox/orgmode/mylife.org" "Tasks") (110 "* %u %?" "~/Dropbox/orgmode/notes.org" "Notes"))))
 '(org-reverse-note-order t)
 '(reb-re-syntax (quote string))
 '(remember-annotation-functions (quote (org-remember-annotation)))
 '(remember-handler-functions (quote (org-remember-handler)))
 '(safe-local-variable-values (quote ((TeX-master . t) (TeX-master . "thesis"))))
 '(scroll-bar-mode nil)
 '(transient-mark-mode 1)
 '(truncate-partial-width-windows nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ediff-current-diff-A ((((class color)) (:background "white" :foreground "blue"))))
 '(ediff-current-diff-B ((((class color)) (:background "white" :foreground "magenta" :weight bold))))
 '(ediff-current-diff-C ((((class color)) (:background "white" :foreground "red" :weight bold))))
 '(ediff-even-diff-A ((((class color)) (:background "yellow" :foreground "white" :weight bold))))
 '(ediff-even-diff-B ((((class color)) (:background "yellow" :foreground "magenta" :weight bold))))
 '(ediff-even-diff-C ((((class color)) (:background "yellow" :foreground "red" :weight bold))))
 '(ediff-fine-diff-B ((((class color)) (:background "magenta" :foreground "white"))))
 '(ediff-fine-diff-C ((((class color)) (:background "red" :foreground "white" :weight bold))))
 '(linum ((t (:inherit (shadow default) :foreground "black")))))

;; Ignore case in tab-completing filenames
(setq read-file-name-completion-ignore-case t)

;; LaTeX stuff
;; (setq compile-command "pdflatex *.tex")
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
  (LaTeX-environment-menu "frame")
  (insert "\\frametitle{}")
  (backward-char 1))

(define-key minibuffer-local-map
  [f3] (lambda () (interactive)
       (insert (buffer-name (current-buffer-not-mini)))))

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
(global-linum-mode t)

(add-hook 'LaTeX-mode-hook
	  (lambda()
	    (LaTeX-add-environments
	     '("block" "title"))))
(add-hook 'LaTeX-mode-hook
	  (lambda()
	    (LaTeX-add-environments
	     '("varblock" "title" "width"))))
(put 'narrow-to-region 'disabled nil)

;; org-mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(require 'remember)
(add-hook 'remember-mode-hook 'org-remember-apply-template)
(define-key global-map [(control meta ?r)] 'remember)

;; Useful functions
(defun current-buffer-not-mini ()
  "Return current-buffer if current buffer is not the *mini-buffer*
  else return buffer before minibuf is activated."
  (if (not (window-minibuffer-p)) (current-buffer)
      (if (eq (get-lru-window) (next-window))
	    (window-buffer (previous-window)) (window-buffer (next-window)))))

;; let me copy and paste to X11 clipboard
(load-file "~/.emacs.d/xclip.el")
(put 'downcase-region 'disabled nil)

;; A keyboard macro to cycle through windows in reverse
;; Bound to C-x p
(fset 'cycle-window-backwards
   "\C-u-\C-xo")
(global-set-key (kbd "C-x p") 'cycle-window-backwards)

;; If two buffers with the same name are open, append enough of
;; the path to make them unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)

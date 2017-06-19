;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package manager stuff
(require 'package)

;; Do some basic hardening of the package system
;; See https://glyph.twistedmatrix.com/2015/11/editor-malware.html

;; Use https for packages
(setq package-archives
      `(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
;; Always check certificates!
(setq tls-checktrust t)
;; Set trust roots
(let ((trustfile
       (replace-regexp-in-string
        "\\\\" "/"
        (replace-regexp-in-string
         "\n" ""
         (shell-command-to-string "python -m certifi")))))
  (setq tls-program
        (list
         (format "gnutls-cli%s --x509cafile %s -p %%p %%h"
                 (if (eq window-system 'w32) ".exe" "") trustfile)))
  (setq gnutls-verify-error t)
  (setq gnutls-trustfiles (list trustfile)))

;; Initialise packages now
(setq package-enable-at-startup nil)
(package-initialize)

;; Make sure we have use-package installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; Ensure all packages are installed
(setq use-package-always-ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Colour schemes
(use-package solarized-theme
  :config
  (load-theme 'solarized-dark t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; Set MajorMode preferences based on filenames
(setq auto-mode-alist
      (append
       '(("\\.cl\\'" . c-mode)
         ("\\emacs\\'" . emacs-lisp-mode)
         ("\\.F90\\'"  . f90-mode)
         ("\\.f03\\'"  . f90-mode)
         ("\\.m\\'"    . matlab-mode)
         ("\\.inp\\'"  . conf-mode))
       auto-mode-alist))

;; Follow symlinks
(setq vc-follow-symlinks t)

;; Some nifty moving between windows
(windmove-default-keybindings)

;; Reload TAGS file automatically
(setq tags-revert-without-query 1)

;; Shortcut for align-regexp
(global-set-key "\M-#" 'align-regexp)

;; Default font
(set-frame-font "Inconsolata LGC-8" nil t)

;; Desktop mode
(desktop-save-mode 1)

;; Dictionary
(setq ispell-dictionary "british")

;; Enable disabled functions
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stuff from easy customisation

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t)
 '(TeX-source-correlate-method (quote synctex))
 '(TeX-source-correlate-mode t)
 '(TeX-source-correlate-start-server t)
 '(TeX-view-program-list (quote (("Okular" "okular --unique %o#src:%n$(pwd)/./%b"))))
 '(TeX-view-program-selection
   (quote
    ((output-pdf "Okular")
     ((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "Evince")
     (output-html "xdg-open"))))
 '(blink-matching-delay 0.3)
 '(c-macro-prompt-flag t)
 '(custom-safe-themes
   (quote
    ("4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" default)))
 '(delete-selection-mode nil)
 '(desktop-path (quote ("~/.emacs.d/" "~" "~/.emacs.d/desktop")))
 '(desktop-registry-registry (quote (("desktop" . "/home/peter/.emacs.d/desktop"))))
 '(doxymacs-doxygen-style "Qt")
 '(ecb-layout-window-sizes
   (quote
    (("left10"
      (ecb-methods-buffer-name 0.22878228782287824 . 0.7391304347826086)
      (ecb-sources-buffer-name 0.11439114391143912 . 0.2463768115942029)
      (ecb-history-buffer-name 0.11439114391143912 . 0.2463768115942029)))))
 '(ecb-options-version "2.40")
 '(ede-project-directories
   (quote
    ("/home/peter/Codes/my-tinyrenderer" "/home/peter/Learning/C/md5_map" "/home/peter/Learning/C/c_vs_haskell")))
 '(f90-auto-keyword-case (quote downcase-word))
 '(font-latex-match-reference-keywords (quote (("Cref" "{") ("cref" "{") ("autoref" "{"))))
 '(global-semantic-decoration-mode t)
 '(global-semantic-highlight-func-mode t)
 '(history-length 100)
 '(inhibit-startup-screen t)
 '(linum-format "%d ")
 '(mark-even-if-inactive t)
 '(org-agenda-files
   (quote
    ("~/Codes/BOUT-dev/todo.org" "~/Codes/NTM9/todo.org" "/home/peter/Documents/orgmode/work.org")))
 '(package-selected-packages
   (quote
    (sphinx-mode smartparens smart-mode-line rtags clang-format use-package helm-projectile solarized-theme color-theme-sanityinc-solarized color-theme-sanityinc-tomorrow modern-cpp-font-lock yaml-mode swiper smartrep realgud rainbow-delimiters pylint projectile paredit pandoc-mode org-pandoc org-bullets org multiple-cursors mkdown matlab-mode material-theme magit-tramp magit-push-remote magit-gitflow magit-find-file list-utils highlight-symbol highlight-parentheses helm-gtags gitconfig gist ghc ggtags flymake-cppcheck flycheck-irony f elpy ein ecb desktop-registry cython-mode cmake-mode auctex adoc-mode ac-clang)))
 '(reb-re-syntax (quote string))
 '(remember-annotation-functions (quote (org-remember-annotation)))
 '(remember-handler-functions (quote (org-remember-handler)))
 '(safe-local-variable-values
   (quote
    ((setq ff-search-directories
           (quote
            ("." "~/Codes/BOUT-dev-clean/include/*" "~/Codes/BOUT-dev-clean/src/*")))
     (ff-search-directories "." "~/Codes/IDAM/latest/source/*")
     (ff-search-directories "." "~/Codes/BOUT-dev/include/*" "~/Codes/BOUT-dev/src/*")
     (ff-search-directories "." "~/Codes/BOUT-dev-clean/include/*" "~/Codes/BOUT-dev-clean/src/*")
     (setq ff-search-directories
           (quote
            ("." "~/Codes/BOUT-dev/include/*" "~/Codes/BOUT-dev/src/*")))
     (tab-width 8)
     (TeX-master . t)
     (TeX-master . "thesis"))))
 '(scroll-bar-mode nil)
 '(semantic-mode nil)
 '(solarized-bold nil)
 '(solarized-distinct-fringe-background t)
 '(transient-mark-mode 1)
 '(truncate-partial-width-windows nil)
 '(which-function-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-latex-sectioning-5-face ((t (:inherit variable-pitch :foreground "#708183" :weight bold))))
 '(font-lock-negation-char-face ((t (:foreground "#dc322f" :weight normal))))
 '(linum ((t (:inherit (shadow default) :foreground "black")))))

;; Ignore case in tab-completing filenames
(setq read-file-name-completion-ignore-case t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
(require 'tex)
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
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-default-bibliography '("/home/peter/Documents/library.bib"))

;; changes \ref to \cref when inserting a reference
(defun reftex-format-cref (label def-fmt style)
  (format "\\cref{%s}" label))

;; previous function no longer works?
(eval-after-load
    "latex"
  '(TeX-add-style-hook
    "cleveref"
    (lambda ()
      (if (boundp 'reftex-ref-style-alist)
      (add-to-list
       'reftex-ref-style-alist
       '("Cleveref" "cleveref"
         (("\\cref" ?c) ("\\Cref" ?C) ("\\cpageref" ?d) ("\\Cpageref" ?D)))))
      (reftex-ref-style-activate "Cleveref")
      (TeX-add-symbols
       '("cref" TeX-arg-ref)
       '("Cref" TeX-arg-ref)
       '("cpageref" TeX-arg-ref)
       '("Cpageref" TeX-arg-ref)))))

(setq reftex-format-ref-function 'reftex-format-cref)
(setq reftex-ref-macro-prompt nil)

;; Beamer
(add-hook 'LaTeX-mode-hook
	  (lambda()
	    (LaTeX-add-environments
	     '("block" "title"))))
(add-hook 'LaTeX-mode-hook
	  (lambda()
	    (LaTeX-add-environments
	     '("varblock" "title" "width"))))

;; Use relative path to find images
(setq LaTeX-includegraphics-read-file 'LaTeX-includegraphics-read-file-relative)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode

(use-package org
  :bind
  (("\C-cl" . org-store-link)
   ("\C-cc" . org-capture)
   ("\C-ca" . org-agenda)
   ("\C-cb" . org-iswitchb))

  :init
  (require 'remember)
  (add-hook 'remember-mode-hook 'org-remember-apply-template)
  (define-key global-map [(control meta ?r)] 'remember)

  (setq org-agenda-files (quote ("/home/peter/Documents/orgmode/work.org"
                                 "/home/peter/Documents/orgmode/mylife.org"))
        org-agenda-ndays 7
        org-agenda-show-all-dates t
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-scheduled-if-done t
        org-agenda-start-on-weekday nil
        org-deadline-warning-days 14
        org-default-notes-file "~/Dropbox/orgmode/notes.org"
        org-fast-tag-selection-single-key 'expert
        org-log-done 'time
        org-remember-store-without-prompt t
        org-remember-templates (quote ((116 "* TODO %?
  %u" "~/Dropbox/orgmode/mylife.org" "Tasks")
                                       (110 "* %u %?" "~/Dropbox/orgmode/notes.org" "Notes")))
        org-reverse-note-order t
        org-startup-folded nil
        org-clock-persist 'history
        )

  :config
  (org-clock-persistence-insinuate))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Useful functions

;; Spell-checking on the fly
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'markdown-mode-hook 'flyspell-mode)

;; linum mode
(use-package linum
  :init
  (global-linum-mode t))

(defun current-buffer-not-mini ()
  "Return current-buffer if current buffer is not the *mini-buffer*
  else return buffer before minibuf is activated."
  (if (not (window-minibuffer-p)) (current-buffer)
      (if (eq (get-lru-window) (next-window))
	    (window-buffer (previous-window)) (window-buffer (next-window)))))



;; A keyboard macro to cycle through windows in reverse
;; Bound to C-x p
(fset 'cycle-window-backwards
   "\C-u-\C-xo")
(global-set-key (kbd "C-x p") 'cycle-window-backwards)

;; If two buffers with the same name are open, append enough of
;; the path to make them unique
(use-package uniquify
  :ensure nil
  :init
  (setq uniquify-buffer-name-style 'reverse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use magit for projects under git

(use-package magit
  :bind
  (("\C-cm" . magit-status))

  :init
  ;; Always split vertically
  (setq split-height-threshold 1600)
  (setq split-width-threshold 160)

  ;; Wrap at 72 columns when writing git log messages in magit
  (defun my-turn-on-auto-fill ()
    (setq fill-column 72)
    (turn-on-auto-fill))

  (add-hook 'magit-log-edit-mode-hook 'my-turn-on-auto-fill))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compile customisation
(use-package cl
  :ensure nil

  :bind
  (("<f5>" . recompile))

  :init
  (setq
   compilation-scroll-output 'first-error      ;; scroll until first error
   compilation-skip-threshold 2                ;; skip warnings
   compilation-read-command nil                ;; don't need enter
   compilation-window-height 12                ;; keep it readable
   compilation-auto-jump-to-first-error nil      ;; jump to first error auto
   compilation-auto-jump-to-next-error nil       ;; jump to next error
   )

  ;; If there were no compilation errors, delete the compilation window
  (setq compilation-exit-message-function
        (lambda (status code msg)
          ;; If M-x compile exists with a 0
          (when (and (eq status 'exit) (zerop code))
            ;; then bury the *compilation* buffer, so that C-x b doesn't go there
            (bury-buffer "*compilation*")
            ;; and return to whatever were looking at before
            (replace-buffer-in-windows "*compilation*"))
          ;; Always return the anticipated result of compilation-exit-message-function
          (cons msg code))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Make minibuffer history behave like bash history
(define-key minibuffer-local-map (kbd "<up>") 'previous-complete-history-element)
(define-key minibuffer-local-map (kbd "<down>") 'next-complete-history-element)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sensible indenting

(setq-default c-default-style "k&r"
              c-basic-offset 2
              indent-tabs-mode nil
              tab-width 4)

(add-hook 'c-mode-hook
          (lambda ()
            (setq comment-start "//" comment-end "")))

(defun cleanup-c-buffer ()
  "Correctly indent, remove tabs and extra whitespace in C source code"
  (interactive)
  (c-indent-region (point-min) (point-max))
  (untabify (point-min) (point-max))
  (whitespace-cleanup-region (point-min) (point-max)))

(use-package ansi-color
  :init
  (defun colorize-compilation-buffer ()
    (toggle-read-only)
    (ansi-color-apply-on-region (point-min) (point-max))
    (toggle-read-only))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
  ;; Colour in shell?
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on))

;; doxymacs
(add-to-list 'load-path "/home/peter/share/emacs/site-lisp/")
(add-hook 'c-mode-common-hook
  (lambda ()
    (require 'doxymacs)
    (doxymacs-mode t)
    (doxymacs-font-lock)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Switching between source and header files

;; Open the other file in a different window
(setq-default ff-always-in-other-window nil)
;; Don't make new files
(setq-default ff-always-try-to-create nil)
;; Ignore #include lines
(setq-default ff-ignore-include t)

(eval-after-load 'cc-mode
  '(progn
     (define-key c-mode-map   "\C-cs" 'ff-find-other-file)
     (define-key c++-mode-map "\C-cs" 'ff-find-other-file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ECB stuff

(require 'semantic/ia)
(require 'semantic/bovine/gcc)
(setq ecb-tip-of-the-day nil)
(setq ecb-layout-name "left10")

(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)

(semantic-mode t)
;; (global-ede-mode nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Highlighting

(use-package highlight-symbol
  :bind
  (("C-x w s" . highlight-symbol)
   ("M-p" .   highlight-symbol-prev)
   ("M-n" .   highlight-symbol-next)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multiple cursors
(use-package multiple-cursors
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
   ("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-c C-<" . mc/mark-all-like-this)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm

(use-package helm
  :diminish helm-mode
  :bind
  (("C-x C-f" . helm-find-files)
   ("M-x" . helm-M-x)
   ("M-y" . helm-show-kill-ring)
   ("C-x b" . helm-mini)
   ("C-c h" . helm-command-prefix))

  :init
  ;; Make helm less ugly
  (setq helm-display-header-line nil)

  ;; Nice window size
  (setq helm-split-window-in-side-p t)
  (setq helm-autoresize-max-height 30)
  (setq helm-autoresize-min-height 30)

  :config
  (require 'helm-config)

  (set-face-attribute 'helm-source-header nil :height 1.0)
  (helm-autoresize-mode 1)

  ;; Keys for helm mode
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
  (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

  (helm-mode 1)

  ;; Don't use helm for settings tags in org-mode
  (add-to-list 'helm-completing-read-handlers-alist '(org-set-tags))
  (add-to-list 'helm-completing-read-handlers-alist '(org-tags-view)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elpy

(use-package elpy
  :config
  (setq elpy-rpc-python-command "python3")
  (setq elpy-test-runner 'elpy-test-pytest-runner)

  (when (require 'flycheck nil t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax highlighting for modern C++ (11+)

(use-package modern-cpp-font-lock
  :init (modern-c++-font-lock-global-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Company mode for code completion

(use-package company
  :config
  (global-company-mode))

(use-package irony
  :config
  (use-package company-irony
    :ensure t
    :config
    (push 'company-irony company-backends)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rtags - C++ aware taggin

(use-package rtags
  :init
  (setq rtags-path "/home/peter/Tools/rtags/install/bin")

  ;; Start rtags automatically for C/C++
  (add-hook 'c-mode-common-hook #'rtags-start-process-unless-running)

  :config
  (rtags-enable-standard-keybindings)

  ;; Get completions working with company mode
  (setq rtags-autostart-diagnostics t)
  (rtags-diagnostics)
  (setq rtags-completions-enabled t)
  (push 'company-rtags company-backends)

  (use-package helm-rtags
    :init
    (setq rtags-use-helm t)

    :config
    (setq rtags-display-result-backend 'helm))

  (use-package flycheck-rtags))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Projectile - project management

(use-package projectile
  :init
  (projectile-mode t)
  (setq projectile-completion-system 'helm)
  (helm-projectile-on))

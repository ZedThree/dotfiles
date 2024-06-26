;; -*- mode: emacs-lisp; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package manager stuff
(require 'package)

;; Do some basic hardening of the package system
;; See https://glyph.twistedmatrix.com/2015/11/editor-malware.html

;; Use https for packages
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
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

;; Ensure all packages are installed
(setq use-package-always-ensure t)

(setq auth-sources '("~/.authinfo.gpg"))

(use-package diminish)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Colour schemes

(use-package solarized-theme
  :config
  (load-theme 'solarized-dark t)
  (setq solarized-bold nil
        solarized-distinct-fringe-background t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generic customisation

;; Custom customisations file
;; Don't want machine-local customisations under version control
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file t)

;; start in savehist mode
(savehist-mode 1)

;; Display line and column numbers on the status line
(setq line-number-mode   t)
(setq column-number-mode t)

;; Turn off menu-bar, tool-bar, scroll-bar
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode nil)

;; Only define region when I tell you to
(transient-mark-mode 1)
;; but do remember the region
(setq mark-even-if-inactive t)

;; Don't overwrite region
(delete-selection-mode nil)

;; Always display in mode line current function point is in (when
;; available)
(which-function-mode t)

;; Respect the value of truncate-lines
(setq truncate-partial-width-windows nil)

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
         ("\\.m\\'"    . matlab-mode)
         ("\\.inp\\'"  . conf-mode)
         ("make.*\\.log" . compilation-mode))
       auto-mode-alist))

;; Follow symlinks
(setq vc-follow-symlinks t)

;; Some nifty moving between windows
(windmove-default-keybindings)

;; Reload TAGS file automatically
(setq tags-revert-without-query 1)

;; Shortcut for align-regexp
(global-set-key "\M-#" 'align-regexp)

;; Default font, if available
(cond
 ((find-font (font-spec :name "Inconsolata LGC"))
  (set-frame-font "Inconsolata LGC-10" nil t)))

;; Sort out emojis
(setq use-default-font-for-symbols nil)
(progn
  ;; set font for emoji (if before emacs 28, should come after setting symbols. emacs 28 now has 'emoji . before, emoji is part of 'symbol)
  (set-fontset-font
   t
   (if (version< emacs-version "28.1")
       '(#x1f300 . #x1fad0)
     'emoji
     )
   (cond
    ((member "Twitter Color Emoji" (font-family-list)) "Twitter Color Emoji")
    ((member "EmojiOne Color" (font-family-list)) "EmojiOne Color")
    ((member "Noto Color Emoji" (font-family-list)) "Noto Color Emoji")
    ((member "Symbola" (font-family-list)) "Symbola"))))
;; Check that emojis look ok: 🙂🙂

;; Desktop mode
(desktop-save-mode 1)
(setq desktop-load-locked-desktop t)
(setq desktop-path '("~/.emacs.d/desktop" "~" "~/.emacs.d/desktop"))

;; Dictionary
(setq ispell-dictionary "british")

;; Enable disabled functions
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

;; Faster highlighting of matching brackets
(setq blink-matching-delay 0.3)

;; Set how many items to remember for commands
(setq history-length 100)

;; No startup screen
(setq inhibit-startup-screen t)

;; Revert PDFs without asking
(setq revert-without-query '(".*\\.pdf"))

;; Confirm close
(setq confirm-kill-emacs 'yes-or-no-p)

;; Don't keep duplicate entries in history
(setq history-delete-duplicates t)

;; Don't make a noise
(setq ring-bell-function 'ignore)

;; Maximise initial frame
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Move which-function earlier in mode-line
(setq-default mode-line-format
    '("%e" mode-line-front-space
      (:propertize
       ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote)
       display (min-width (5.0)))
      mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position
      (vc-mode vc-mode)
      "  " mode-line-misc-info mode-line-modes  mode-line-end-spaces))

;; Don't show abbrev mode in mode-line
(use-package abbrev
  :ensure nil
  :diminish)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fortran

(defun my-chain-flycheck-gfortran ()
  (lsp-deferred)
  (flycheck-add-next-checker 'lsp 'fortran-gfortran))

(use-package f90
  ;; built-in
  :ensure nil
  :mode (("\\.F90\\'"  . f90-mode)
         ("\\.f03\\'"  . f90-mode)
         ("\\.fpp\\'"  . f90-mode)
         ("\\.pf\\'"  . f90-mode)
         )
  :hook
  (f90-mode . lsp)
  (f90-mode . my-chain-flycheck-gfortran)

  :init
  ;; Use lower-case keywords for Fortran
  (setq f90-auto-keyword-case #'downcase-word
        f90-if-indent 2
        f90-do-indent 2
        f90-type-indent 2
        f90-indented-comment-re "!<?"
        f90-beginning-ampersand nil))

(use-package lsp-mode
  :init
  :hook
  (lsp-mode . yas-minor-mode-on)

  :diminish
  lsp-lens-mode
  eldoc-mode

  :bind
  (:map lsp-signature-mode-map
        ("C-c l s n" . lsp-signature-next)
        ("C-c l s p" . lsp-signature-previous)
        )

  :config
  (setq read-process-output-max (* 1024 1024)
        gc-cons-threshold 100000000
        lsp-enable-xref t
        lsp-headerline-breadcrumb-enable nil
        lsp-pylsp-plugins-flake8-enabled nil)

  (define-key lsp-signature-mode-map (kbd "M-n") nil)
  (define-key lsp-signature-mode-map (kbd "M-p") nil)
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol)

  (use-package helm-lsp)
  (use-package dap-mode)
  (use-package yasnippet
    :diminish yas-minor-mode)
  )

;; File regexes to ignore when using grep
(setq grep-find-ignored-files
      '(".#*" "*.hi" "*.o" "*~" "*.bin" "*.lbin" "*.so" "*.a" "*.ln" "*.blg" "*.bbl" "*.elc" "*.lof" "*.glo" "*.idx" "*.lot" "*.fmt" "*.tfm" "*.class" "*.fas" "*.lib" "*.mem" "*.x86f" "*.sparcf" "*.dfsl" "*.pfsl" "*.d64fsl" "*.p64fsl" "*.lx64fsl" "*.lx32fsl" "*.dx64fsl" "*.dx32fsl" "*.fx64fsl" "*.fx32fsl" "*.sx64fsl" "*.sx32fsl" "*.wx64fsl" "*.wx32fsl" "*.fasl" "*.ufsl" "*.fsl" "*.dxl" "*.lo" "*.la" "*.gmo" "*.mo" "*.toc" "*.aux" "*.cp" "*.fn" "*.ky" "*.pg" "*.tp" "*.vr" "*.cps" "*.fns" "*.kys" "*.pgs" "*.tps" "*.vrs" "*.pyc" "*.pyo"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LaTeX stuff

(use-package tex
  :ensure auctex

  :config
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-PDF-mode t
        ;; Synctex integration for playing nice with okular
        TeX-source-correlate-method 'synctex
        TeX-source-correlate-mode t
        TeX-source-correlate-start-server t
        TeX-view-program-selection '((output-pdf "Okular")
                                     ((output-dvi style-pstricks) "dvips and gv")
                                     (output-dvi "xdvi")
                                     (output-pdf "Evince")
                                     (output-html "xdg-open"))
        ;; Use relative path to find images
        LaTeX-includegraphics-read-file 'LaTeX-includegraphics-read-file-relative
        ;; RefTeX stuff
        reftex-plug-into-AUCTeX t
        reftex-default-bibliography '("~/Documents/library.bib")
        font-latex-match-reference-keywords '(("Cref" "{") ("cref" "{") ("autoref" "{")))

  (setq-default TeX-master nil)

  (add-hook 'LaTeX-mode-hook 'turn-on-reftex))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode

(use-package org
  :bind
  (("\C-col" . org-store-link)
   ("\C-coc" . org-capture)
   ("\C-coa" . org-agenda)
   ("\C-cob" . org-iswitchb))

  :init
  (require 'remember)
  (add-hook 'remember-mode-hook 'org-remember-apply-template)
  (define-key global-map [(control meta ?r)] 'remember)

  (setq org-agenda-files '("~/Documents/orgmode/work.org"
                           "~/Documents/orgmode/mylife.org")
        org-agenda-ndays 7
        org-agenda-show-all-dates t
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-scheduled-if-done t
        org-agenda-start-on-weekday nil
        org-blank-before-new-entry nil
        org-deadline-warning-days 14
        org-default-notes-file "~/Dropbox/orgmode/notes.org"
        org-fast-tag-selection-single-key 'expert
        org-log-done 'time
        org-remember-store-without-prompt t
        org-remember-templates '((116 "* TODO %?
  %u" "~/Dropbox/orgmode/mylife.org" "Tasks")
                                 (110 "* %u %?" "~/Dropbox/orgmode/notes.org" "Notes"))
        org-reverse-note-order t
        org-startup-folded nil
        org-clock-persist 'history
        remember-annotation-functions #'org-remember-annotation
        remember-handler-functions #'org-remember-handler
        org-agenda-restore-windows-after-quit t)

  :config
  (org-clock-persistence-insinuate)
  ;; Don't let org-mode clobber windmove bindings
  (define-key org-mode-map (kbd "S-<left>") nil)
  (define-key org-mode-map (kbd "S-<right>") nil)
  (define-key org-mode-map (kbd "S-<up>") nil)
  (define-key org-mode-map (kbd "S-<down>") nil)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Useful functions

;; Spell-checking on the fly
(use-package flyspell
  :defer t
  :diminish
  :init
  (add-hook 'markdown-mode-hook 'flyspell-mode)
  (add-hook 'org-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'rst-mode-hook 'flyspell-mode))

(global-display-line-numbers-mode)

(defun current-buffer-not-mini ()
  "Return current-buffer if current buffer is not the *mini-buffer*
  else return buffer before minibuf is activated."
  (if (not (window-minibuffer-p)) (current-buffer)
      (if (eq (get-lru-window) (next-window))
        (window-buffer (previous-window)) (window-buffer (next-window)))))

(define-key minibuffer-local-map
  [f3] (lambda () (interactive)
       (insert (buffer-name (current-buffer-not-mini)))))

;; A keyboard macro to cycle through windows in reverse
;; Bound to C-x p
(fset 'cycle-window-backwards "\C-u-\C-xo")
(global-set-key (kbd "C-x p") 'cycle-window-backwards)

;; If two buffers with the same name are open, append enough of
;; the path to make them unique
(use-package uniquify
  :ensure nil
  :init
  (setq uniquify-buffer-name-style 'reverse))

(setq split-width-threshold (- (window-width) 10))
(setq split-height-threshold nil)

(defun count-visible-buffers (&optional frame)
  "Count how many buffers are currently being shown. Defaults to selected frame."
  (length (mapcar #'window-buffer (window-list frame))))

(defun do-not-split-more-than-two-windows (window &optional horizontal)
  (if (and horizontal (> (count-visible-buffers) 1))
      nil
    t))

(advice-add 'window-splittable-p :before-while #'do-not-split-more-than-two-windows)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use magit for projects under git

(use-package magit
  :bind
  (("\C-cm" . magit-status))

  :init
  (add-hook 'git-commit-setup-hook #'git-commit-turn-on-auto-fill)
  (add-hook 'git-commit-setup-hook #'git-commit-turn-on-flyspell))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compile customisation

(use-package notifications)

(use-package cl
  :ensure nil

  :bind
  (("<f5>" . recompile))

  :init
  (setq
   compilation-scroll-output 'first-error      ;; scroll until first error
   compilation-skip-threshold 1                ;; skip things less than warnings
   compilation-read-command t                  ;; confirm compile command
   compilation-window-height 12                ;; keep it readable
   compilation-auto-jump-to-first-error nil      ;; jump to first error auto
   compilation-auto-jump-to-next-error nil       ;; jump to next error
   )

  (setq compilation-finish-functions
        (lambda (buf str)
          (progn
            (if (null (string-match ".*exited abnormally.*" str))
                ;;no errors, make the compilation window go away in a few seconds
                (progn
                  (run-at-time "0.4 sec" nil
                               (lambda ()
                                 (select-window (get-buffer-window (get-buffer-create "*compilation*")))
                                 (switch-to-buffer nil)))
                  (message "No Compilation Errors!")
                  (notifications-notify
                   :title "Emacs compilation buffer finished"
                   :body "No compilation errors!")
                  )
              (notifications-notify
               :title "Emacs compilation buffer finished"
               :body "Some errors!")
              )))))

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
            (setq comment-start "//" comment-end ""
                  c-macro-prompt-flag t)))

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
   ("C-c C-<" . mc/mark-all-like-this)
   ("C-S-c 1" . mc/insert-numbers)
   ("C-S-c a" . mc/insert-letters)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm

(use-package helm
  :diminish helm-mode
  :bind
  (("C-x C-f" . helm-find-files)
   ("M-x" . helm-M-x)
   ("M-y" . helm-show-kill-ring)
   ("C-x b" . helm-mini)
   ("C-c h" . helm-command-prefix)
   ("M-$" . helm-flyspell-correct)
   :map helm-map
   ([tab] . helm-execute-persistent-action)
   ;; So tab also works in terminal
   ("C-i" . helm-execute-persistent-action)
   ("C-z" . helm-select-action))

  :config
  ;; Make helm less ugly
  (setq helm-display-header-line nil
        ;; Nice window size
        helm-split-window-in-side-p t
        helm-autoresize-max-height 30
        helm-autoresize-min-height 30
        helm-boring-buffer-regexp-list '("\\` "
                                         "\\`\\*helm"
                                         "\\`\\*Echo Area"
                                         "\\`\\*Minibuf"
                                         "\\`\\*fortls"
                                         "\\`\\*.*lsp"
                                         "\\`\\*Async"
                                         "\\`\\*clangd")
        )

  (use-package helm-flyspell
    :ensure t
    :diminish)

  (use-package helm-xref
    :ensure t)

  (set-face-attribute 'helm-source-header nil :height 1.0)
  (helm-autoresize-mode 1)

  (helm-mode 1)

  ;; Don't use helm for settings tags in org-mode
  (add-to-list 'helm-completing-read-handlers-alist '(org-set-tags))
  (add-to-list 'helm-completing-read-handlers-alist '(org-tags-view))
  (add-to-list 'helm-completing-read-handlers-alist '(xref-find-references)))

(use-package ffap
  :config
  (setq ffap-machine-p-known 'reject))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; python

(use-package python-mode
  :ensure nil
  :hook
  (python-mode . lsp)

  :config
  (setq lsp-pylsp-plugins-flake8-enabled nil
        forward-sexp-function nil
        )
  :custom
  (python-forward-sexp-function nil "Turn off stupid python mode forward-sexp")

  (use-package highlight-indentation
    :diminish)

  (use-package numpydoc
    :ensure t
    :bind (:map python-mode-map
                ("C-c C-n" . numpydoc-generate))
    :config
    (setq numpydoc-insert-return-without-typehint t
          numpydoc-insert-raises-block nil
          numpydoc-insert-examples-block nil
          numpydoc-insertion-style 'yas))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax highlighting for modern C++ (11+)

(use-package modern-cpp-font-lock
  :init (modern-c++-font-lock-global-mode t)
  :diminish modern-c++-font-lock-global-mode
  :diminish modern-c++-font-lock-mode)

(use-package cmake-mode
  :bind
  (:map cmake-mode-map
        ("C-c C-d" . cmake-help)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto-formatting for C++

(use-package clang-format
  :init
  (defun clang-format-defun ()
    (interactive)
    (save-excursion
      (mark-defun)
      (clang-format-region (region-beginning) (region-end))
      (deactivate-mark))))

(defun gvol-dont-indent-outernamespace (langelem)
    "Indent a namespace by 0 if it's the outer namespace and by + otherwise."
    (save-excursion
      ;; The beginning of the current namespace
      (goto-char (cdr langelem))
      (if (alist-get 'innamespace (c-guess-basic-syntax))
          '+
        0)))

(use-package cc-mode
  :bind
  (:map c++-mode-map
        ("C-c c d" . clang-format-defun)
        ("C-c c r" . clang-format-region))

  :hook
  (c-mode . lsp)
  (c++-mode . lsp)

  :config
  (c-set-offset 'innamespace #'gvol-dont-indent-outernamespace)
  (setq lsp-clangd-binary-path "/usr/bin/clangd"
        lsp-clients-clangd-args '("-j" "2"
                                  "--compile-commands-dir=./build"
                                  "--header-insertion-decorators=0")
        )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Company mode for code completion

(use-package company
  :config
  (global-company-mode)

  :diminish company-mode

  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Projectile - project management

(use-package projectile
  :init
  (use-package helm-projectile
    :init
    (setq projectile-completion-system 'helm))

  :bind
  (:map projectile-mode-map
        ("C-c p" . 'projectile-command-map))

  :config
  (projectile-mode t)
  (setq projectile-globally-ignored-file-suffixes '(".o", ".mod" "~"))

  (helm-projectile-on)

  :bind-keymap
  ("C-c p" . projectile-command-map)

  :diminish projectile-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Regex building

(use-package re-builder
  :config
  (setq reb-re-syntax 'string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markdown/pandoc/restructuredtext

(use-package markdown-mode
  :init
  (defun my-markdown-insert-columns (start end)
    "Insert pandoc-style column markers"
    (interactive
     (if (use-region-p)
         (list (region-beginning) (region-end))
       (list (point) (point))))

    (save-excursion
      (goto-char end)
      (insert "\n:::\n::: {.column align=center}\n\n:::\n::::::")
      (goto-char start)
      (insert ":::::: {.columns}\n::: {.column align=center}\n\n")
      ))

  :bind
  (:map markdown-mode-map
        ("M-<right>" . markdown-demote)
        ("M-<left>" . markdown-promote)
        ("C-c C-c /" . my-markdown-insert-columns)
        ;; Don't clobber highlight-symbol bindings
        ("M-p" . nil)
        ("M-n" . nil))
  :hook
  (markdown-mode . auto-fill-mode)

  :diminish auto-fill-function)

(use-package pandoc-mode
  :config
  (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)

  :hook markdown-mode)

(use-package rst
  :hook
  (rst-mode . auto-fill-mode)

  :diminish auto-fill-function)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; YAML

(use-package yaml-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flycheck

(use-package flycheck
  :config
  (setq flycheck-gfortran-args '("-Wall" "-Wextra" "-ffree-form"
                                 "-ffree-line-length-none" "-fno-backslash"
                                 "-std=f2018"))
  :hook (f90-mode . flycheck-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Autorevert

(use-package autorevert
  :diminish auto-revert-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Search cppreference

;; Adapted from https://batsov.com/articles/2011/11/19/why-emacs/
(defun cppreference-query ()
  "Searches cppreference"
  (interactive)
  (browse-url
   (concat
    "https://en.cppreference.com/mwiki/index.php?title=Special:Search&search="
    (if mark-active
        (buffer-substring (region-beginning) (region-end))
      (read-string "cppreference: ")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Makefiles

(use-package makefile
  ;; built-in
  :ensure nil
  :mode (("Makefile\\.*"  . makefile-gmake-mode)
         ("makefile\\.*"  . makefile-gmake-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gdb

;; The global value is 1, but this interferes with gdb's single
;; character shortcuts
(defun my-change-company-prefix-for-gud nil
  (setq-local company-minimum-prefix-length 2))

(use-package gdb
  :ensure nil
  :hook (gdb-mode . my-change-company-prefix-for-gud))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Last thing, start server

(add-hook 'after-init-hook 'server-start t)

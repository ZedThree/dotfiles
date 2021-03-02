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

(use-package diminish)

;; Ensure all packages are installed
(setq use-package-always-ensure t)

(setq auth-sources '("~/.authinfo.gpg" "~/.authinfo" "~/.netrc"))

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

;; Default font, if available
(cond
 ((find-font (font-spec :name "Inconsolata LGC"))
  (set-frame-font "Inconsolata LGC-10" nil t)))

;; Desktop mode
(desktop-save-mode 1)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fortran

(use-package f90
  ;; built-in
  :ensure nil
  :mode (("\\.F90\\'"  . f90-mode)
         ("\\.f03\\'"  . f90-mode)
         ("\\.fpp\\'"  . f90-mode)
         ("\\.pf\\'"  . f90-mode)
         )
  :hook
  (f90-mode . yas-minor-mode)

  :init
  ;; Use lower-case keywords for Fortran
  (setq f90-auto-keyword-case #'downcase-word
        f90-if-indent 2
        f90-do-indent 2
        f90-type-indent 2
        f90-indented-comment-re "!<?"))

(use-package lsp-mode
  :init
  :hook
  (f90-mode . lsp)
  :config
  (use-package lsp-ui
    :ensure f
    :after flycheck))

;; File regexes to ignore when using grep
(setq grep-find-ignored-files
      '(".#*" "*.hi" "*.o" "*~" "*.bin" "*.lbin" "*.so" "*.a" "*.ln" "*.blg" "*.bbl" "*.elc" "*.lof" "*.glo" "*.idx" "*.lot" "*.fmt" "*.tfm" "*.class" "*.fas" "*.lib" "*.mem" "*.x86f" "*.sparcf" "*.dfsl" "*.pfsl" "*.d64fsl" "*.p64fsl" "*.lx64fsl" "*.lx32fsl" "*.dx64fsl" "*.dx32fsl" "*.fx64fsl" "*.fx32fsl" "*.sx64fsl" "*.sx32fsl" "*.wx64fsl" "*.wx32fsl" "*.fasl" "*.ufsl" "*.fsl" "*.dxl" "*.lo" "*.la" "*.gmo" "*.mo" "*.toc" "*.aux" "*.cp" "*.fn" "*.ky" "*.pg" "*.tp" "*.vr" "*.cps" "*.fns" "*.kys" "*.pgs" "*.tps" "*.vrs" "*.pyc" "*.pyo" "*.html"))

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
        remember-handler-functions #'org-remember-handler)

  ;; Don't let org-mode clobber windmove bindings
  (define-key org-mode-map (kbd "S-<left>") nil)
  (define-key org-mode-map (kbd "S-<right>") nil)
  (define-key org-mode-map (kbd "S-<up>") nil)
  (define-key org-mode-map (kbd "S-<down>") nil)

  :config
  (org-clock-persistence-insinuate))

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
  (global-magit-file-mode)

  (add-hook 'git-commit-setup-hook #'git-commit-turn-on-auto-fill)
  (add-hook 'git-commit-setup-hook #'git-commit-turn-on-flyspell))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compile customisation

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
          (if (null (string-match ".*exited abnormally.*" str))
              ;;no errors, make the compilation window go away in a few seconds
              (progn
                (run-at-time "0.4 sec" nil
                             (lambda ()
                               (select-window (get-buffer-window (get-buffer-create "*compilation*")))
                               (switch-to-buffer nil)))
                (message "No Compilation Errors!"))))))

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
   ("C-c h" . helm-command-prefix)
   ("M-$" . helm-flyspell-correct)
   :map helm-map
   ([tab] . helm-execute-persistent-action)
   ;; So tab also works in terminal
   ("C-i" . helm-execute-persistent-action)
   ("C-z" . helm-select-action))

  :config
  (require 'helm-config)
  ;; Make helm less ugly
  (setq helm-display-header-line nil
        ;; Nice window size
        helm-split-window-in-side-p t
        helm-autoresize-max-height 30
        helm-autoresize-min-height 30)

  (use-package helm-flyspell
    :ensure t
    :diminish)

  (set-face-attribute 'helm-source-header nil :height 1.0)
  (helm-autoresize-mode 1)

  (helm-mode 1)

  ;; Don't use helm for settings tags in org-mode
  (add-to-list 'helm-completing-read-handlers-alist '(org-set-tags))
  (add-to-list 'helm-completing-read-handlers-alist '(org-tags-view)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elpy

(use-package elpy
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable)

  :config
  (setq elpy-rpc-python-command "python3")
  (setq elpy-test-runner 'elpy-test-pytest-runner)

  (when (require 'flycheck nil t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax highlighting for modern C++ (11+)

(use-package modern-cpp-font-lock
  :init (modern-c++-font-lock-global-mode t)
  :diminish modern-c++-font-lock-global-mode
  :diminish modern-c++-font-lock-mode)

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

(use-package cc-mode
  :bind
  (:map c++-mode-map
        ("C-c c d" . clang-format-defun)
        ("C-c c r" . clang-format-region)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Company mode for code completion

(use-package company
  :config
  (global-company-mode)

  :diminish company-mode)

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

  ;; Start rtags automatically for C/C++
  (add-hook 'c-mode-common-hook #'rtags-start-process-unless-running)

  :config
  (rtags-enable-standard-keybindings)

  ;; Get completions working with company mode
  (setq rtags-autostart-diagnostics t)
  (rtags-diagnostics)
  (setq rtags-completions-enabled t)

  (use-package company-rtags
    :init
    (push 'company-rtags company-backends))

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
;; Markdown/pandoc

(use-package markdown-mode
  :bind
  (:map markdown-mode-map
        ("M-<right>" . markdown-demote)
        ("M-<left>" . markdown-promote))
  :hook
  (markdown-mode . auto-fill-mode)

  :diminish auto-fill-function)

(use-package pandoc-mode
  :config
  (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)

  :hook markdown-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; YAML

(use-package yaml-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flycheck

(use-package flycheck
  :config
  (setq flycheck-python-flake8-executable "python3"
        flycheck-python-pylint-executable "python3"
        flycheck-gfortran-args '("-Wall" "-Wextra" "-ffree-form"
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
;; Last thing, start server

(add-hook 'after-init-hook 'server-start t)

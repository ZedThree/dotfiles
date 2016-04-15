;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package manager stuff
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; (add-to-list 'package-archives-enable-alist
;;              '("melpa" "magit" "git-commit-mode" "git-rebase-mode"))
(setq package-enable-at-startup nil)
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Colour schemes
(require 'color-theme)
(require 'color-theme-solarized)
(load-theme 'solarized t)

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
;; (add-hook 'before-save-hook 'delete-trailing-whitespace)

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

;; Add doxygen comments to variable declarations in F90 mode
(defun doxygen-comment-dwim (arg)
  "Uses doxygen comment format for variable declarations. See comment-dwim for more information."
  (interactive "*P")
  (if (save-excursion (end-of-line)
		      (search-backward "::" (line-beginning-position) t)) ; Only apply doxygen comments to variable declarations
      (let ((comment-start "!< "))	 ; Change the comment style to doxygen format
	(comment-dwim arg))		 ; Comment/uncomment the line
    (comment-dwim arg)))

(add-hook 'f90-mode-hook
	  '(lambda ()
	     (define-key f90-mode-map [remap comment-dwim] 'doxygen-comment-dwim)))

;; Follow symlinks
(setq vc-follow-symlinks t)

;; matlab-mode stuff
(defun my-matlab-mode-hook ()  (setq fill-column 90))

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
 '(c-macro-prompt-flag t)
 '(custom-safe-themes
   (quote
    ("8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" default)))
 '(delete-selection-mode nil)
 '(desktop-path (quote ("~/.emacs.d/" "~" "~/.emacs.d/desktop")))
 '(desktop-registry-registry (quote (("desktop" . "/home/peter/.emacs.d/desktop"))))
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
 '(reb-re-syntax (quote string))
 '(remember-annotation-functions (quote (org-remember-annotation)))
 '(remember-handler-functions (quote (org-remember-handler)))
 '(safe-local-variable-values
   (quote
    ((ff-search-directories "." "~/Codes/IDAM/latest/source/*")
     (ff-search-directories "." "~/Codes/BOUT-dev/include/*" "~/Codes/BOUT-dev/src/*")
     (setq ff-search-directories
           (quote
            ("." "~/Codes/BOUT-dev/include/*" "~/Codes/BOUT-dev/src/*")))
     (tab-width 8)
     (TeX-master . t)
     (TeX-master . "thesis"))))
 '(scroll-bar-mode nil)
 '(transient-mark-mode 1)
 '(truncate-partial-width-windows nil)
 '(which-function-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-latex-sectioning-5-face ((t (:inherit variable-pitch :foreground "#708183" :weight bold))))
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
;; (add-hook 'reftex-load-hook 'imenu-add-menubar-index)
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

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
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

(org-clock-persistence-insinuate)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Useful functions

;; Spell-checking on the fly
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'markdown-mode-hook 'flyspell-mode)

;; linum mode
(require 'linum)
(global-linum-mode t)

;; Let me use narrow-to-region
(put 'narrow-to-region 'disabled nil)

(defun current-buffer-not-mini ()
  "Return current-buffer if current buffer is not the *mini-buffer*
  else return buffer before minibuf is activated."
  (if (not (window-minibuffer-p)) (current-buffer)
      (if (eq (get-lru-window) (next-window))
	    (window-buffer (previous-window)) (window-buffer (next-window)))))

;; let me copy and paste to X11 clipboard
;; (load-file "~/.emacs.d/xclip.el")
;; (setq x-select-enable-clipboard t)

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

;; Use magit for projects under git
(require 'magit)
(global-set-key "\C-cm" 'magit-status)
(setq magit-last-seen-setup-instructions "1.4.0")

;; Always split vertically
(setq split-height-threshold 1600)
(setq split-width-threshold 160)

;; Magit wants to delete its window - don't let it
(defun magit-quit-window (&optional kill-buffer)
  "Bury the buffer and delete its window. With a prefix argument, kill the
buffer instead."
  (interactive "P")
  (bury-buffer kill-buffer))

;; Wrap at 72 columns when writing git log messages in magit
(defun my-turn-on-auto-fill ()
  (setq fill-column 72)
  (turn-on-auto-fill))

(add-hook 'magit-log-edit-mode-hook 'my-turn-on-auto-fill)

(put 'set-goal-column 'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compile customisation
(require 'cl)

(setq
 compilation-scroll-output 'first-error      ;; scroll until first error
 compilation-skip-threshold 2                ;; skip warnings
 compilation-read-command nil                ;; don't need enter
 compilation-window-height 12                ;; keep it readable
 compilation-auto-jump-to-first-error nil      ;; jump to first error auto
 compilation-auto-jump-to-next-error nil       ;; jump to next error
)

;; (defun* get-closest-pathname (&optional (file "Makefile"))
;;   "Determine the pathname of the first instance of FILE starting from the current directory towards root.
;; This may not do the correct thing in presence of links. If it does not find FILE, then it shall return the name
;; of FILE in the current directory, suitable for creation"
;;   (let ((root (expand-file-name "/"))) ; the win32 builds should translate this correctly
;;     (expand-file-name file
;; 		      (loop
;; 			for d = default-directory then (expand-file-name ".." d)
;; 			if (file-exists-p (expand-file-name file d))
;; 			return d
;; 			if (equal d root)
;; 			return nil))))

;; (require 'compile)
;; (add-hook 'f90-mode-hook
;; 	  (lambda ()
;; 	    (set (make-local-variable 'compile-command)
;; 		 (let ((file (file-name-nondirectory buffer-file-name))
;; 		       (mkfile (get-closest-pathname)))
;; 		   (progn (format "cd %s; make -j4 -k -f %s"
;; 				(file-name-directory mkfile) mkfile))))))

(global-set-key (kbd "<f5>") 'recompile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Some nifty moving between windows
(windmove-default-keybindings)

;; Reload TAGS file automatically
(setq tags-revert-without-query 1)

;; Shortcut for align-regexp
(global-set-key "\M-#" 'align-regexp)

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
        (cons msg code)))

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

;; Default font
(set-frame-font "Inconsolata LGC-8" nil t)

;; Colour in shell?
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; doxymacs
(add-to-list 'load-path "/home/peter/share/emacs/site-lisp/")
(add-hook 'c-mode-common-hook
  (lambda ()
    (require 'doxymacs)
    (doxymacs-mode t)
    (doxymacs-font-lock)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tramp stuff

(require 'tramp)
(add-to-list 'tramp-remote-path "/hwdisks/data/modules/pkg/git/1.8.4.1/bin/git")
(add-to-list 'tramp-remote-path 'tramp-default-remote-path)
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

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
(global-ede-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Highlighting

(define-key global-map "\C-xws" 'highlight-symbol)
(define-key global-map "\M-p"   'highlight-symbol-prev)
(define-key global-map "\M-n"   'highlight-symbol-next)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Desktop mode

(desktop-save-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dictionary

(setq ispell-dictionary "british")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multiple cursors
(require 'multiple-cursors)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm

(require 'helm)
(require 'helm-config)

;; Make helm less ugly
(setq helm-display-header-line nil)
(set-face-attribute 'helm-source-header nil :height 1.0)

;; Nice window size
(setq helm-split-window-in-side-p t)
(helm-autoresize-mode 1)
(setq helm-autoresize-max-height 30)
(setq helm-autoresize-min-height 30)

;; Keys for helm mode
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

;; Global keys for helm functions
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-c h") 'helm-command-prefix)

;; Don't use helm for settings tags in org-mode
(eval-after-load 'helm-mode
  (progn
    '(add-to-list 'helm-completing-read-handlers-alist '(org-set-tags))
    '(add-to-list 'helm-completing-read-handlers-alist '(org-tags-view))))

;; (add-to-list 'helm-completing-read-handlers-alist '(org-tags-view))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elpy

(require 'elpy)
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

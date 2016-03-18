;;;;;; .emacs --- emacs dot file.
;;; Commentary:
;; http://www.lunaryorn.com/2015/01/06/my-emacs-configuration-with-use-package.html
;; https://github.com/lunaryorn/.emacs.d/blob/master/init.el
;; To unset a key binding: M-x global-unset-key
;;
;; To remove all hooks from a mode during testing, evaulate:
;; (setq <mode-hook> nil)
;; as per: https://www.gnu.org/software/emacs/manual/html_node/emacs/Hooks.html

;;; Code:
;; Please don't load outdated byte code
(setq load-prefer-newer t)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)



(setq debug-on-error t)
;; *****************************************************
;; Elisp file paths
;; *****************************************************
;; http://www.emacswiki.org/emacs/InstallingPackages
;; http://xahlee.org/emacs/emacs_installing_packages.html
(add-to-list 'load-path "~/.emacs.d/elpa/")
;; http://stackoverflow.com/questions/221365/emacs-lisp-how-to-add-a-folder-and-all-its-first-level-sub-folders-to-the-load
(let* ((my-emacsd-dir "~/.emacs.d/elpa/")
       (default-directory my-emacsd-dir)
       (orig-load-path load-path))
  (setq load-path (cons my-emacsd-dir nil))
  (normal-top-level-add-subdirs-to-load-path)
  (nconc load-path orig-load-path))


;; *****************************************************
;; *****************************************************
;; Various Tweaks (one-liners and minor package config).
;; *****************************************************
;; *****************************************************
(put 'downcase-region 'disabled nil)  ; allow downcase-region without the disabled feature warning.
(put 'upcase-region 'disabled nil)  ; allow upcase-region without the disabled feature warning.
'(flycheck-error-list-column-number ((t (:inherit font-lock-constant-face :background "blue"))))
'(flycheck-warning ((t (:background "color-17" :underline (:color "DarkOrange" :style wave)))))
'(calendar-week-start-day 1)
(setq compilation-scroll-output 'first-error)

;; *****************************************************
;; *****************************************************
;; Text mode functions
;; *****************************************************
;; *****************************************************
;; Highlight specific words.
;; ========================
;; http://www.emacswiki.org/emacs/EmacsNiftyTricks
;; http://emacs-fu.blogspot.com/2008/12/highlighting-todo-fixme-and-friends.html
(defun my_highlighted_words ()
  "Highlight specific words in the buffer."
 (interactive)
  (font-lock-add-keywords nil
   '(("\\<\\(Note\\|NOTE\\|FIXME\\|Todo\\|TODO\\|BUG\\|Bug\\):" 1 '(:foreground "red" :weight bold) t))))


(load-theme 'wombat t)
;; ========================
;; Highlight tabs in green.
;; ========================
;; http://emacswiki.org/emacs/ShowWhiteSpace#toc9
(defface extra-whitespace-face '((t (:background "pale green")))
  "Used for tabs and such.")
(defvar my-extra-keywords '(("\t" . 'extra-whitespace-face)))


;; ========================
;; Temp directories.
;; ========================
(setq temporary-file-directory "/tmp/")  ;; This lets me say where my temp dir is.
;; move backups & autosaves to system temp directory (http://www.emacswiki.org/emacs/BackupDirectory).
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(menu-bar-mode -1)  ;; Disable Menu Bar
(fset 'yes-or-no-p 'y-or-n-p)  ;; yes/no -> y/n

;; ========================
;; Higlight Line
;; ========================
(global-hl-line-mode 1)
;; http://stackoverflow.com/questions/9990370/how-to-disable-hl-line-feature-in-specified-mode
(add-hook 'ansi-term-hook (lambda () (global-hl-line-mode 0)))

;; ========================
;; Fill Column (used to reflow text automatically & highlight margins)
;; ========================
;; different 79 char ruler, that's solid down.
;; http://www.emacswiki.org/FillColumnIndicator
;; Also changed the column fill to be a double pipe. See unicode table.
(use-package fill-column-indicator
  :ensure t
  :config
  (progn
    (setq-default fci-rule-column 79)
    (setq fci-rule-character ?\u2016)
    ;; automatically wrap to 79 characters.
    (setq-default fill-column 79)
    (setq-default git-commit-fill-column 79))
)

;; ========================
;; White Space Mode
;; ========================
;; http://ergoemacs.org/emacs/whitespace-mode.html
;; make whitespace-mode use just basic coloring
;(setq whitespace-style (quote (spaces tabs newline space-mark tab-mark newline-mark)))
(setq whitespace-style (quote (trailing face)))
(setq whitespace-display-mappings
'(
  (space-mark 32 [183] [46]) ; normal space, ·
  (newline-mark 10 [182 10]) ; newlne, ¶
  (tab-mark 9 [9655 9] [92 9]) ; tab, ▷
))

;; ========================
;; spell checking (aspell)
;; ========================
;; ispell is the built in spell checker, but aspell is better (multiple dictionaries)
; http://www.emacswiki.org/emacs/InteractiveSpell#toc6
; brew install aspell --with-lang-es --with-lang-uk --with-lang-en
(setq ispell-program-name "aspell")
(setq ispell-list-command "list")
; #TODO: ispell integration to auto-complete




;; *****************************************************
;; *****************************************************
;; My custom functions
;; *****************************************************
;; *****************************************************
(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (file-exists-p (buffer-file-name)) (not (buffer-modified-p)))
        (revert-buffer t t t) )))
  (message "Refreshed open files.") )


(defun go-to-column (column)
  "GoTo column. Was getting annoyed seeing errors that point to a column number
   so grabbed this code:
   - http://emacsredux.com/blog/2013/07/09/go-to-column/"
  (interactive "nColumn: ")
  (move-to-column column t))
(global-set-key (kbd "M-g M-c") 'go-to-column)

;; ========================
;; Find Non ASCII
;; ========================
;; Here’s a simple defun to show non-ascii characters of current buffer in an Occur buffer
;; http://www.emacswiki.org/emacs/FindingNonAsciiCharacters
(defun occur-non-ascii ()
  "Find any non-ascii characters in the current buffer."
  (interactive)
  (occur "[^[:ascii:]]"))

;; ========================
;; Comment/uncomment code easily
;; ========================
; https://stackoverflow.com/questions/9688748/emacs-comment-uncomment-current-line
(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))
(global-set-key (kbd "C-c '") 'comment-or-uncomment-region-or-line)



;; *****************************************************
;; *****************************************************
;; File/Buffer Management.
;; *****************************************************
;; *****************************************************
(setq large-file-warning-threshold (* 40 1024 1024))  ;; large files shouting from 40MB's

;; ========================
;; Ido (easily find files & open buffers with fuzzy matching)
;; ========================
;; Ido mode with fuzzy matching
(use-package ido
  :ensure t
  :bind ("C-x C-b" . ibuffer)
  :init
  (progn
    (ido-mode t)
    (setq ido-enable-flex-matching t) ;; enable fuzzy matching
  )
)

;; ========================
;; smex - Smex is IDO, but for M-x
;; ========================
;; http://writequit.org/org/settings.html
(use-package smex
  :ensure t
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)))

;; ========================
;; Desktop - Auto save tabs.
;; ========================
(use-package desktop                    ; Save buffers, windows and frames
  :init (desktop-save-mode)
  :config
  (progn
    ;; Don't autosave desktops, it's too expensive.  Desktops aren't
    ;; that precious, and Emacs will save the desktop on exit anyway.
    (setq
     desktop-auto-save-timeout nil
     desktop-path '("~/")
     desktop-dirname "~/")
    (dolist (mode '(magit-mode git-commit-mode))
      (add-to-list 'desktop-modes-not-to-save mode))))

;; ========================
;; mode-line (the gutter bar) (smart mode line wraps up a lot of nice tweaks in one package)
;; ========================
;; https://github.com/Bruce-Connor/smart-mode-line
(use-package smart-mode-line
  :ensure t
  :init
  (progn
    (setq sml/no-confirm-load-theme t)
    (setq sml/theme 'dark)
    (sml/setup)
    (setq sml/name-width `55)
    (setq sml/mode-width `full)
    (column-number-mode 1)
  )
)



;; ========================
;; ansi-term (terminal emulator. Prefer it to multi-term/eshell/shell)
;; ========================
;; From: https://github.com/jwalgran/dotfiles/blob/master/.emacs.d/config.el
(use-package term
  :demand t
  :bind (("<f2>" . visit-ansi-term)
         ;; Note: gdb keybinding is: C-x C-a C-l, which I did have my rename term windows as.
         ;; ("C-x C-a" . open-term)
         )
  :init
  (progn
    ;; https://github.com/ahinz/emacs-config/blob/7e025076097f045aea2a0aedd0523ee996753346/.emacs.d/ah-modes.el#L268
    (defun open-named-term (new-buffer-name cmd &rest switches)
      (setq term-ansi-buffer-name (generate-new-buffer-name new-buffer-name))
      (setq term-ansi-buffer-name (apply 'make-term term-ansi-buffer-name cmd nil switches))
      (set-buffer term-ansi-buffer-name)
      (term-mode)
      (term-char-mode)
      (term-set-escape-char ?\C-x)
      (switch-to-buffer term-ansi-buffer-name))

    ;; https://github.com/ahinz/emacs-config/blob/7e025076097f045aea2a0aedd0523ee996753346/.emacs.d/ah-modes.el#L268
    (defun open-term (name)
      (interactive "sName: ")
      (open-named-term name "/bin/bash"))

    (defun visit-ansi-term ()
      "If the current buffer is:
         1) a running ansi-term named *ansi-term*, rename it.
         2) a stopped ansi-term, kill it and create a new one.
         3) a non ansi-term, go to an already running ansi-term
            or start a new one while killing a defunt one"
      (interactive)
      (let ((is-term (string= "term-mode" major-mode))
            (is-running (term-check-proc (buffer-name)))
            (term-cmd "/bin/bash")
            (anon-term (get-buffer "*ansi-term*")))
        (if is-term
            (if is-running
                (if (string= "*ansi-term*" (buffer-name))
                    (call-interactively 'rename-buffer)
                  (if anon-term
                      (switch-to-buffer "*ansi-term*")
                    (ansi-term term-cmd)))
              (kill-buffer (buffer-name))
              (ansi-term term-cmd))
          (if anon-term
              (if (term-check-proc "*ansi-term*")
                  (switch-to-buffer "*ansi-term*")
                (kill-buffer "*ansi-term*")
                (ansi-term term-cmd))
            (ansi-term term-cmd)))))

    ;; Make ansi-term buffers close when you kill the shell process
    (defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
      (if (memq (process-status proc) '(signal exit))
          (let ((buffer (process-buffer proc)))
            ad-do-it
            (kill-buffer buffer))
        ad-do-it))
    (ad-activate 'term-sentinel))
  )



;; *****************************************************
;; *****************************************************
;; MODE DEFAULTS
;; *****************************************************
;; *****************************************************
;; ========================
;; TEXT MODE DEFAULTS
;; ========================
(defun my-text-mode-config ()
  "All of my 'text-mode' config in one place."
  (interactive)
  (whitespace-mode)  ;; highlights whitespace.
  (my_highlighted_words)  ;; highlights specific words in red & bold.
  (lambda () (font-lock-add-keywords nil my-extra-keywords))  ;; highlight tabs
  (fci-mode)  ;; adds fill column indicator.
  (auto-fill-mode)  ;; wraps at auto fill column.
  (flyspell-mode)
  (linum-mode)  ; Line Numbers Mode
  (my_highlighted_words)  ;; highlight specific words
  )

(add-hook 'text-mode-hook 'my-text-mode-config)  ;; singular text-mode-hook

;; ========================
;; PROGRAMMING DEFAULTS
;; ========================
(defun my-programming-defaults-config ()
  "All of my programming defaults  in one place."
  (interactive)
  (whitespace-mode)  ;; highlights whitespace.
  (my_highlighted_words)  ;; highlights specific words in red & bold.
  (lambda () (font-lock-add-keywords nil my-extra-keywords))  ;; highlight tabs
  (fci-mode)  ;; adds fill column indicator.
  ;; (auto-fill-mode nil)  ;; disables auto fill at column.
  (flyspell-mode)
  (flyspell-prog-mode)  ;; spell check comments/strings
  (setq indent-tabs-mode nil)  ;; spaces instead of tabs
  (setq tab-width 4)  ;; 4 spaces per tab key press.
  (which-function-mode)  ;; Display current function in mode line. (http://emacsredux.com/blog/2014/04/05/which-function-mode/)
  (linum-mode)  ; Line Numbers Mode
  (my_highlighted_words)  ;; highlight specific words
  )
(add-hook 'sh-mode-hook 'my-programming-defaults-config)

;; ========================
;; *SCRATCH* BUFFER DEFAULTS
;; ========================
(defun my-scratch-mode-config ()
  "Disabling config for *scratch* buffer."
  (interactive)
  (fci-mode -1)
  )
(add-hook 'lisp-interaction-mode-hook 'my-scratch-mode-config)
(add-hook 'my-scratch-mode-config-hook (lambda() (linum-mode 0)))

;; *****************************************************
;; *****************************************************
;; General programming packages
;; *****************************************************
;; *****************************************************
;; ========================
;; Setup Flycheck (Code checking on the fly (replaces flymake)
;; ========================
(use-package flycheck                   ; On-the-fly syntax checking
  :ensure t
  :bind (("C-c e" . list-flycheck-errors)
         ("C-c T f" . flycheck-mode)
         ("C-c j" . flycheck-next-error)
        )
  :init (global-flycheck-mode)
  :config
  (progn
    (setq
     flycheck-completion-system 'ido
     flycheck-highlighting-mode 'lines
     flycheck-display-errors-delay 0.0
     flycheck-flake8-maximum-complexity 10
     flycheck-highlighting-mode (quote lines)
     ;; Set the standard library to libc++ so that C++11 headers will work
     flycheck-clang-standard-library "libc++"
     )
    (set-face-attribute 'flycheck-error nil :background "color-52")  ; dark red
    (set-face-attribute 'flycheck-warning nil :background "color-17")  ; dark blue
    (set-face-attribute 'flycheck-info nil :background "color-22")  ; dark green
    ;; Use italic face for checker name
    (set-face-attribute 'flycheck-error-list-checker-name nil :inherit 'italic)
  )
  :diminish flycheck-mode)
(flycheck-mode flycheck-mode-line) ; Flycheck status

;; ========================
;; VC config (VC is built in version control package. Magit is an enhanced git VC package)
;; ========================
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/General-VC-Options.html
(setq vc-follow-symlinks t)
;; magit - a pretty good git package with more features than the built in emacs "vc" package.
;; http://magit.github.com/magit/
; (add-to-list 'load-path "~/elisp/magit/")
(use-package magit
  :ensure t
  :bind (
	 ("<f3>" . magit-status)
	 ("\C-cg" . vc-git-grep)
	 ("\C-cb" . magit-blame-mode))
  :config
  (progn
    (setq magit-auto-revert-mode t)
    (setq magit-last-seen-setup-instructions "1.4.0")
    (use-package magit-svn
      :ensure t
      )
    )
  )
'(magit-item-highlight ((t nil)) t)


;; *****************************************************
;; *****************************************************
;; Lisp programming
;; *****************************************************
;; *****************************************************
(add-hook 'emacs-lisp-mode-hook 'my-programming-defaults-config)
;; code from: http://www.emacswiki.org/emacs/EmacsLispMode
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            ;; Pretty-print eval'd expressions.
            (define-key emacs-lisp-mode-map
                        "\C-x\C-e" 'pp-eval-last-sexp)
            ;; ;; Recompile if .elc exists. ;; recompiles everything on every save -cas
            ;; (add-hook (make-local-variable 'after-save-hook)
            ;;           (lambda ()
            ;;             (byte-force-recompile default-directory)))
            (define-key emacs-lisp-mode-map
                        "\r" 'reindent-then-newline-and-indent)))
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)


;; *****************************************************
;; *****************************************************
;; Erlang programming
;; *****************************************************
;; *****************************************************
(use-package erlang
  ;; (require 'erlang-start)
  :ensure t
  )



;; *****************************************************
;; *****************************************************
;; XML programming
;; *****************************************************
;; *****************************************************
(use-package nxml-mode
  :mode ("web.config$" . xml-mode)
  :init
  (progn
    (add-hook 'nxml-mode-hook 'my-programming-defaults-config)
    (add-hook 'nxml-mode-hook (lambda () (auto-fill-mode -1)))  ;; disables auto fill at column.
    ;; http://www.nuxeo.com/blog/nxml-mode-tabs/
    (add-hook 'nxml-mode-hook (lambda () (setq indent-tabs-mode nil)))
    (setq
     nxml-child-indent 4
     )
    )
  )

;; *****************************************************
;; *****************************************************
;; Php programming
;; *****************************************************
;; *****************************************************
(use-package php-mode
  :ensure t
  :config
  (progn
    (add-hook 'php-mode-hook 'my-programming-defaults-config)
    )
  )


;; *****************************************************
;; *****************************************************
;; Groovy programming
;; *****************************************************
;; *****************************************************
(use-package groovy-mode
  ; https://github.com/Groovy-Emacs-Modes/groovy-emacs-modes
  :ensure t
  :config
  (progn
    (add-hook 'groovy-mode-hook 'my-programming-defaults-config)
    )
  )


;; *****************************************************
;; *****************************************************
;; Markdown
;; *****************************************************
;; *****************************************************
(use-package markdown-mode
  ; NOTE: 'M-x markdown-preview', requires: 'markdown', to be installed with
  ; system package manager.
  :ensure t
  :init
  (progn
    (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
    (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
    (add-hook 'markdown-mode-hook 'my-text-mode-config)
    )
  )

;; *****************************************************
;; *****************************************************
;; Dockerfile
;; *****************************************************
;; *****************************************************
(use-package dockerfile-mode
  :ensure t
  :init
  (progn
    (add-hook 'dockerfile-mode-hook 'my-programming-defaults-config)
    )
  )
;; *****************************************************
;; *****************************************************
;; Json programming
;; *****************************************************
;; *****************************************************
(use-package json-mode
  :ensure t
  :config
  (progn
    (add-hook 'json-mode-hook 'my-programming-defaults-config)
    (add-hook 'json-mode-hook (lambda () (auto-fill-mode -1)))  ;; disables auto fill at column.
    )
  )

;; *****************************************************
;; *****************************************************
;; Python IDE stuff
;; *****************************************************
;; *****************************************************
;; ========================
;; Python Virtual Env
;; ========================
;; https://github.com/jorgenschaefer/pyvenv
;; for tracking and auto virt_env loading, I need to add the following to a
;; .dir-locals.el file at the root of a project:
;; ((python-mode . ((pyvenv-workon . "<virt_env>))))
(use-package pyvenv
  :ensure t
  ;; FIXME: Need to do a buffer reload after pyvenv-workon. Add a hook.
  )

;; ========================
;; python.el
;; ========================
;; http://www.emacswiki.org/emacs/ProgrammingWithPythonDotEl
;; https://github.com/fgallina/python.el
;; http://www.saltycrane.com/blog/2010/05/my-emacs-python-environment/
(use-package python
  :defer t
  :bind ("\C-m" . newline-and-indent)
  :config
  (progn
    (add-hook 'python-mode-hook 'my-programming-defaults-config)
    (add-hook 'python-mode-hook (lambda () (pyvenv-mode t)))
    (add-hook 'python-mode-hook (lambda () (pyvenv-tracking-mode t)))
    (add-hook 'python-mode-hook (lambda () (jedi-mode t)))
    ;; (setq
    ;;  python-indent-offset 4  ;; FIXME: With this all indenting is broken. Without a package moans that it is missing.
    ;;  )
    ; http://www.emacswiki.org/emacs/ProgrammingWithPythonDotEl#toc1
    ;; (add-hook 'python-mode-hook '(lambda () (define-key python-mode-map "\C-m" 'newline-and-indent)))  ; maintain indentation on newline
    (use-package auto-complete
      :ensure t
      :config
      (progn
        (setq
         ac-candidate-limit 20
         auto-complete-mode t
         ;; TODO: I think latest auto-complete no longer needs the ac-flyspell-workaround.
         ;; Need to find out if this is stated in the auto-complete changelogs. Was getting
         ;; an incorrect number of arguments 'setq 5'.
         ;; Or it could possibly be a use-package change and it just needs to be changed to
         ;; 'ac-flyspell-workaround t'.
         ;; Not in python code atm, so on the backburner for now.
         ;; ac-flyspell-workaround
         ; https://stackoverflow.com/questions/11484225/fix-an-auto-complete-mode-and-linum-mode-annoyance
         ;; ac-linum-workaround   ; fix linum jumping characters around.
         )
        (add-to-list 'ac-sources 'ac-source-jedi-direct)
        (add-hook 'python-mode-hook 'jedi:setup)
        )
      )
    (use-package jedi
      :ensure t
      :init
      (progn
        (jedi:install-server)
        )
      :config
      (progn
        (jedi:setup)
        (jedi:ac-setup)
        (setq
         jedi:tooltip-method nil
         jedi:get-in-function-call-delay 100
         jedi:complete-on-dot t)
        (set-face-attribute 'jedi:highlight-function-argument nil
                            :foreground "green")
        (define-key python-mode-map (kbd "C-c C-d") 'jedi:show-doc)
        (define-key python-mode-map (kbd "C-c C-l") 'jedi:get-in-function-call)
        (define-key python-mode-map (kbd "C-c .") 'jedi:key-goto-definition)
        )
      )
    (use-package sphinx-doc
      ; https://github.com/naiquevin/sphinx-doc.el
      ; C-c M-d, to auto generate sphinx docs for current function.
      :ensure t
      :config
      (progn
        (add-hook 'python-mode-hook (lambda ()
                                      (require 'sphinx-doc)
                                      (sphinx-doc-mode t)))
        )
      )
    )
  )
;; ========================
;; nosetests
;; ========================
(use-package nose
  :ensure t
  :config
  (progn
    (define-key nose-mode-map "\C-cn" 'nosetests-all)
    ;; (define-key python-mode-map "\C-cn" 'nosetests-all)
    ;; (define-key python-mode-map "\C-cm" 'nosetests-one)
    (setq nose-use-verbose nil) ; default is t  ; nil = dots as output.
  )
)



;; *****************************************************
;; *****************************************************
;; C++ IDE stuff
;; *****************************************************
;; *****************************************************
(defun read-lines (filePath)
  "Return a list of lines of a file at FILEPATH."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))


(use-package company
  ;; Completion
  :ensure t
  :config
  (progn
    (add-hook 'c-mode-hook 'company-mode)
    (add-hook 'c++-mode-hook 'company-mode)
    (setq company-tooltip-limit 20) ; bigger popup window
    (setq company-idle-delay .3)    ; decrease delay before autocompletion popup shows

    (bind-keys :map company-active-map
               ("C-n" . company-select-next)
               ([(tab)] . company-complete)
               )
    ;; (setq company-backends (delete 'company-semantic company-backends))

    (use-package company-c-headers
      ;; Complete c-headers
      :ensure t
      :config
      (progn
        ;; (add-to-list 'company-c-headers-path-system "/usr/include/c++/4.8.2/")
        ;; (add-to-list 'company-c-headers-path-system "/usr/include/c++/4.8/")
        (add-to-list 'company-backends 'company-c-headers)
      )
    (use-package company-statistics
      ;; Rate completions by use.
      :ensure t
      :config
      (add-hook 'after-init-hook 'company-statistics-mode)
      )
    )
  )
)

(use-package cc-mode
  ;; gdb on mac:
  ;; brew tap homebrew/dupes && brew install gdb
  ;; Note: gdb keybinding is: C-x C-a C-l, which I did have my rename term windows as.
  :ensure t
  ;; :bind (
  ;;        ("<f9>" . compile)
  ;;        ("C-c C-c" . compile)
  ;;        ;; ("M-." . semantic-ia-fast-jump)
  ;;        )
  :config
  (progn
    (use-package smart-compile
      :ensure t)
    (use-package xcscope
      ;; Use cscope files within emacs, to jump around C/C++ code.
      ;; https://github.com/dkogan/xcscope.el
      :ensure t
      :config
      (progn
        ;; Setup auto-magically hooks into c/c++ modes.
        (cscope-setup)
        )
      (define-key c++-mode-map [remap c-set-style] 'cscope-find-this-symbol)  ;; C-c .
      ;; Note etags search defaults to: M-.
      )
    ;; cc-mode general settings.

    ;; g++-4.9 -g3 -Wall -std=c++11 -stdlib=libc++ -lc++ *.cpp
    ;; clang++ -g3 -Wall -std=c++11 -stdlib=libc++ -lc++ *.cpp
    (add-to-list 'smart-compile-alist '("\\.[Cc]+[Pp]*\\'" . "clang++ -g3 -Wall -std=c++11 -stdlib=libc++ -lc++ -o %n.out *.cpp"))
    (add-hook 'c++-mode-hook 'my-programming-defaults-config)
    (setq-default c-basic-offset 4)  ;; http://emacswiki.org/emacs/IndentingC
    (setq c-default-style "linux")  ;; http://cc-mode.sourceforge.net/html-manual/Built_002din-Styles.html#Built_002din-Styles
    )
  (define-key c++-mode-map [remap comment-region] 'compile)  ;; C-c C-c
  )


;; *****************************************************
;; *****************************************************
;; Org Mode
;; *****************************************************
;; *****************************************************
;; ========================
;; Initialisation
;; ========================
;; http://orgmode.org/worg/code/elisp/dto-org-gtd.el
;; http://www.gnu.org/software/emacs/manual/html_node/org/Remember-templates.html
(use-package org
  :ensure t
  :bind (
	 ("C-c l" . org-store-link)
	 ("C-c a" . org-agenda))
  :init
  (progn
    (setq
     org-directory "~/org/"
     org-default-notes-file "~/org/notes.org"
    ;; agenda items. (http://orgmode.org/worg/org-tutorials/orgtutorial_dto.html).
    org-agenda-files (list "~/org/projects.org"
                           "~/org/todo.org"
                           "~/org/notes.org")
    ;; refile level.
    ;; http://www.millingtons.eclipse.co.uk/glyn/dotemacs.html
    org-refile-targets (quote
			((org-agenda-files :maxlevel . 5)
			 ("~/org/projects.org" :maxlevel . 2)))
    org-log-done t
    )
    (global-set-key "\C-cr" (lambda () (interactive) (org-capture nil "t")))
    (global-set-key "\C-cn" (lambda () (interactive) (org-capture nil "n")))
    (add-hook 'org-mode-hook (lambda() (linum-mode 0)))
  )
)

(use-package remember
  :ensure t
  :init
  (progn
    ;; remember templates.
    (setq org-capture-templates
	  '(("t" "Todo" entry (file+headline "~/org/todo.org" "INBOX")
	     "* TODO %?\n %U - %i\n  %a")
	    ("p" "Project" entry (file+headline "~/org/projects.org" "INBOX")
	     "* TODO %?\n %U - %i\n  %a")
	    ("n" "Notes" entry (file+headline "~/org/notes.org" "Tasks")
	     "* TODO %?\n %U - %i\n  %a")
	    )
     )
  )
)


;; ========================
;; Github blog
;; ========================
(defun org-custom-link-img-follow (path)
  "PATH to find custom linked images."
  (org-open-file-with-emacs
   (format "~/org/github_blog/images/%s" path)))

(defun org-custom-link-img-export (path desc format)
  "Rewrite custom linked images for export."
  (cond
   ((eq format 'html)
    (format "<img src=\"http://jackson15j.github.io/%s\" alt=\"%s\"/>" path desc))))

(require 'org)
(org-add-link-type "img" 'org-custom-link-img-follow 'org-custom-link-img-export)


(use-package org-jekyll
  :ensure org-jekyll)

(setq org-publish-project-alist
      `(
        ("jackson15j-org"
         :base-directory "~/org/github_blog/"
         :base-extension "org"
         :publishing-directory "~/github_blog/"
         :recursive t
         :site-root "http://jackson15j.github.io/"
         :publishing-function org-html-publish-to-html ;org-publish-org-to-html
         :headline-levels 6
         :html-extension "html"
         :body-only t
         :section-numbers nil
         :table-of-contents nil
         :author "Craig Astill")
        ("jackson15j-static"
         :base-directory "~/org/github_blog/"
         :base-extension "css\\|js\\|png\\|jpg\\|jpeg\\|ico\\|gif\\|pdf\\|mp3\\|flac\\|ogg\\|swf\\|php\\|markdown\\|md\\|html\\|htm\\|sh\\|xml\\|gz\\|bz2\\|vcf\\|zip\\|txt\\|tex\\|otf\\|ttf\\|eot\\|rb\\|yml\\|htaccess\\|gitignore"
         :publishing-directory "~/github_blog/"
         :recursive t
         :publishing-function org-publish-attachment)
        ("jackson15j-blog" :components ("jackson15j-org" "jackson15j-static"))
        )
      )

; TODO: fix up, or move to beamer??
;; ========================
;; org-present
;; ========================
;; Open an org-mode file with each slide under a top-level heading.
;; Start org-present with org-present-mode, left and right keys will move forward
;; and backward through slides. C-c C-q will quit org-present.
;; This works well with hide-mode-line (http://webonastick.com/emacs-lisp/hide-mode-line.el),
;; which hides the mode-line when only one frame and buffer are open.
;; If you're on a Mac you might also want to look at the fullscreen patch here:
;; http://cloud.github.com/downloads/typester/emacs/feature-fullscreen.patch
;  (add-to-list 'load-path "~/path/to/org-present")
(autoload 'org-present "org-present" nil t)
(add-hook 'org-present-mode-hook
          (lambda ()
            (org-present-big)
            (org-display-inline-images)))
(add-hook 'org-present-mode-quit-hook
          (lambda ()
            (org-present-small)
            (org-remove-inline-images)))



;; *****************************************************
;; *****************************************************
;; Log file highlighting
;; *****************************************************
;; *****************************************************
(use-package log4j-mode
  :ensure t
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.log\\'" . log4j-mode))
    )
  )
'(log4j-font-lock-fatal-face ((t (:foreground "darkred" :weight bold))))
'(log4j-font-lock-info-face ((t (:foreground "ForestGreen"))))
'(log4j-font-lock-warn-face ((t (:foreground "orange"))))




;; *****************************************************
;; *****************************************************
;; email support (Mutt - external cli Message User Agent (MUA) for emails)
;; *****************************************************
;; *****************************************************
(setq auto-mode-alist (append '((".*mutt.*" . mail-mode)) auto-mode-alist))  ; use mail major mode.


;; *****************************************************
;; *****************************************************
;; Web support (eww - web browser)
;; *****************************************************
;; *****************************************************
(use-package eww
  :bind (("<f4>" . eww))
  ;; FIXME: eww is okay, but for things like google account redirects, we need
  ;; a real browser.
  ;; :config
  ;; (progn
  ;;   (setq
  ;;    browse-url-browser-function (quote eww-browse-url)
  ;;    )
  ;;   )
  )

;; bound this to Darwin only.
(cond
 ((string-equal system-type "darwin")
  (setq browse-url-browser-function  'browse-url-generic
        browse-url-generic-program "/Applications/Opera.app/Contents/MacOS/Opera")))
;; ========================
;; Stackoverflow search (SOS)
;; ========================
(use-package sos
  :ensure t
  :bind (("<f5>" . sos))
  )


;; ========================
;; Load extra dot files (if they exist)
;; ========================
(let () (dolist (dot_emacs '("~/configs/emacs/private_dot_emacs.el"
                             "~/configs/emacs/unstable_config_dot_emacs.el"
                             "~/configs/emacs/work_specific_dot_emacs.el"))
          "Loading my extra emacs dot files if they exist."
          (when (file-exists-p dot_emacs)
            (message (concat "Loading external dot file: " dot_emacs))
            (load-file dot_emacs))))



;; *****************************************************
;; *****************************************************
;; emacs auto configured items from: M-x customize (DON'T TOUCH!!)
;; *****************************************************
;; *****************************************************
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(browse-url-browser-function (quote eww-browse-url))
 '(calendar-week-start-day 1)
 '(markdown-preview-port 8080)
 '(package-selected-packages
   (quote
    (sos log4j-mode org-jekyll nose magit-svn magit xcscope smart-compile company-c-headers company-statistics company sphinx-doc jedi auto-complete php-mode erlang flycheck smart-mode-line smex fill-column-indicator use-package))))
;;; .emacs ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

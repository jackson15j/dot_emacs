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

;; *****************************************************
;; *****************************************************
;; use-package, melpa and debugging bootstrapping.
;; *****************************************************
;; *****************************************************

;; Elisp file paths
;; http://www.emacswiki.org/emacs/InstallingPackages
;; http://xahlee.org/emacs/emacs_installing_packages.html
; (add-to-list 'load-path "~/.emacs.d/elpa/")
;; http://stackoverflow.com/questions/221365/emacs-lisp-how-to-add-a-folder-and-all-its-first-level-sub-folders-to-the-load
(ignore-errors
 (make-directory "~/.emacs.d/elpa"))

(let* ((my-emacsd-dir "~/.emacs.d/elpa/")
       (default-directory my-emacsd-dir)
       (orig-load-path load-path))
  (setq load-path (cons my-emacsd-dir nil))
  (normal-top-level-add-subdirs-to-load-path)
  (nconc load-path orig-load-path))

;; Please don't load outdated byte code
(setq load-prefer-newer t)
; https://stackoverflow.com/questions/1217180/how-do-i-byte-compile-everything-in-my-emacs-d-directory
; (byte-recompile-directory (expand-file-name "~/.emacs.d") 0)

(require 'package)
(setq package-eanable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(setq debug-on-error t)
;; *****************************************************
;; Bug-Hunter: Debug lisp files
;; *****************************************************
;; https://github.com/Malabarba/elisp-bug-hunter
(use-package bug-hunter
  :ensure t)

(use-package server
  :ensure t
  :config
  (server-start))



;; *****************************************************
;; *****************************************************
;; Various Tweaks (one-liners and minor package config).
;; *****************************************************
;; *****************************************************
(setq global-auto-revert-mode 1)
(put 'downcase-region 'disabled nil)  ; allow downcase-region without the disabled feature warning.
(put 'upcase-region 'disabled nil)  ; allow upcase-region without the disabled feature warning.
'(flycheck-error-list-column-number ((t (:inherit font-lock-constant-face :background "blue"))))
'(flycheck-warning ((t (:background "DarkBlue" :underline (:color "DarkOrange" :style wave)))))
(setq calendar-week-start-day 1)
(setq compilation-scroll-output 't)
(menu-bar-mode -1)  ;; Disable Menu Bar
(tool-bar-mode -1)  ;; Disable tool Bar
(fset 'yes-or-no-p 'y-or-n-p)  ;; yes/no -> y/n
(load-theme 'wombat t)
; (display-time)  ;; Time in modeline. Un-comment to enable.
(display-battery-mode t)  ;; Battery in modeline. Un-comment to enable.
(setq large-file-warning-threshold (* 40 1024 1024))  ;; large files shouting from 40MB's

;; Temp directories
(setq temporary-file-directory "/tmp/")  ;; This lets me say where my temp dir is.
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))  ; http://www.emacswiki.org/emacs/BackupDirectory
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Highlights
(global-hl-line-mode 1)  ;; horizontal highlighted line on cursor.
;; http://www.emacswiki.org/emacs/EmacsNiftyTricks
;; http://emacs-fu.blogspot.com/2008/12/highlighting-todo-fixme-and-friends.html
(defun my_highlighted_words ()
  "Highlight specific words in the buffer."
 (interactive)
  (font-lock-add-keywords nil
   '(("\\<\\(Note\\|NOTE\\|FIXME\\|Todo\\|TODO\\|BUG\\|Bug\\):" 1 '(:foreground "red" :weight bold) t))))


; Use the C-based line numbers instead of the slower lisp (`linum`).
;; https://www.emacswiki.org/emacs/LineNumbers#h5o-1
(use-package display-line-numbers
  :config (global-display-line-numbers-mode)
  :custom-face
   (line-number ((t (:inherit (shadow default) :background "grey10"))))
  )
(defcustom display-line-numbers-exempt-modes '(vterm-mode eshell-mode shell-mode term-mode ansi-term-mode lisp-interaction-mode, org, compilation-mode)
  "Major modes on which to disable the linum mode, exempts them from global requirement."
  :group 'display-line-numbers
  :type 'list
  :version "green")
(defun display-line-numbers--turn-on ()
  "Turn on line numbers but exempting certain major modes defined in `display-line-numbers-exempt-modes'."
  (if (and
       (not (member major-mode display-line-numbers-exempt-modes))
       (not (minibufferp)))
      (display-line-numbers-mode)))


(use-package whitespace
  ;; White Space Mode
  ;; http://ergoemacs.org/emacs/whitespace-mode.html
  :ensure t
  :config
  (global-whitespace-mode)
  ;; make whitespace-mode use just basic coloring
  (setq whitespace-style (quote (face trailing tabs)))
  )


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
;; spell checking (aspell)
;; ========================
;; ispell is the built in spell checker, but aspell is better (multiple dictionaries)
; http://www.emacswiki.org/emacs/InteractiveSpell#toc6
; brew install aspell --with-lang-es --with-lang-uk --with-lang-en
(setq ispell-program-name "aspell")
(setq ispell-list-command "list")

;; ========================
;; Lookup dictionary definitions.
;; ========================
;; `M-x dictionary-search` look up word definition.
(use-package dictionary
  :ensure t)



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
  "GoTo column.
Was getting annoyed seeing errors that point to a COLUMN number;
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
  :ensure t
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
  (setq
   sml/no-confirm-load-theme t
   sml/theme 'dark
   sml/mode-width `full
   )
  (sml/setup)
  (column-number-mode t)
  )



(use-package vterm
  :if (not (eq system-type 'windows-nt))  ;; FIXME: compiling on Windows.
  :ensure t
  :init (setq vterm-always-compile-module t)
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

  (add-hook 'ansi-term-hook (lambda () (global-hl-line-mode 0)))  ; http://stackoverflow.com/questions/9990370/how-to-disable-hl-line-feature-in-specified-mode
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
  (fci-mode)  ;; adds fill column indicator.
  (auto-fill-mode)  ;; wraps at auto fill column.
  (flyspell-mode)
  (my_highlighted_words)  ;; highlight specific words
  (setq indent-tabs-mode nil)  ;; spaces instead of tabs
  )

(add-hook 'text-mode-hook 'my-text-mode-config)  ;; singular text-mode-hook
(add-hook 'conf-mode-hook 'my-text-mode-config)  ;; *.conf

;; ========================
;; PROGRAMMING DEFAULTS
;; ========================
(defun my-programming-defaults-config ()
  "All of my programming defaults  in one place."
  (interactive)
  (whitespace-mode)  ;; highlights whitespace.
  (my_highlighted_words)  ;; highlights specific words in red & bold.
  (fci-mode)  ;; adds fill column indicator.
  ;; (auto-fill-mode nil)  ;; disables auto fill at column.
  (flyspell-mode)
  (flyspell-prog-mode)  ;; spell check comments/strings
  (setq indent-tabs-mode nil)  ;; spaces instead of tabs
  (setq tab-width 4)  ;; 4 spaces per tab key press.
  (which-function-mode)  ;; Display current function in mode line. (http://emacsredux.com/blog/2014/04/05/which-function-mode/)
  (my_highlighted_words)  ;; highlight specific words
  (show-paren-mode 1)  ;; highlight matching brackets
  (setq tags-revert-without-query t)
  )
(add-hook 'sh-mode-hook 'my-programming-defaults-config)
;; Don't line-wrap in html files.
;; https://stackoverflow.com/questions/9294437/emacs-disable-wordwrapping-in-html-mode
(add-hook 'html-mode-hook (lambda () (auto-fill-mode -1)))

;; ========================
;; *SCRATCH* BUFFER DEFAULTS
;; ========================
(defun my-scratch-mode-config ()
  "Disabling config for *scratch* buffer."
  (interactive)
  (fci-mode -1)
  (auto-fill-mode -1)
  )
(add-hook 'lisp-interaction-mode-hook 'my-scratch-mode-config)

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
  :hook (
         (python-mode . (lambda () (setq flycheck-checker 'python-flake8)))
         )
  :init (global-flycheck-mode)
  :config
  (progn
    (setq
     flycheck-completion-system 'ido
     flycheck-highlighting-mode 'lines
     flycheck-display-errors-delay 0.0
     flycheck-flake8-maximum-complexity 10
     flycheck-flake8rc "setup.cfg"
     flycheck-highlighting-mode (quote lines)
     ;; Set the standard library to libc++ so that C++11 headers will work
     flycheck-clang-standard-library "libc++"
     )
    (set-face-attribute 'flycheck-error nil :background "DarkRed")  ; dark red
    (set-face-attribute 'flycheck-warning nil :background "DarkBlue")  ; dark blue
    (set-face-attribute 'flycheck-info nil :background "DarkGreen")  ; dark green
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
(use-package magit
  ;; magit - a pretty good git package with more features than the built in
  ;; emacs "vc" package.
  ;; http://magit.github.com/magit/
  :ensure t
  :bind (
     ("<f3>" . magit-status)
     ("\C-c\C-s" . magit-status)
     ("\C-cg" . vc-git-grep)
     ("\C-cb" . magit-blame))
  :config
  (setq magit-auto-revert-mode t)
  ;; `M-x magit-describe-section-briefly`, then check the square brackets in:
  ;; `<magit-section ... [<section_name> status] ...>`.
  (setq
   magit-section-initial-visibility-alist
   '(
     (stashes . hide)
     (unpulled . show)
     (unpushed . show)
     (pullreqs . show)
     ))

  (use-package magit-popup
    ;; https://github.com/magit/magit/issues/3749
    ;; `magit` moved to using `transient` but some packages (`magithub` -
    ;; https://github.com/vermiculus/magithub/issues/402) haven't updated,
    ;; hence explicit definition of `magit-popup`
    :ensure t
    )

  (use-package magit-svn
    :ensure t
    )

  (use-package forge
    ;; https://github.com/magit/forge
    ;; Replacement for `magithub` (https://github.com/vermiculus/magithub),
    ;; which works with Gitlab/Github.
    ;; See old commits for my old `magithub` config.
    :if (not (eq system-type 'windows-nt))  ;; FIXME: Needs `cc` compiler defined.
    :ensure t
    :config
    (add-to-list 'forge-alist '("git-scm.clinithink.com:2009" "git-scm.clinithink.com/api/v4" "git-scm.clinithink.com" forge-gitlab-repository))
    )

  )






;; *****************************************************
;; *****************************************************
;; Code Completion.
;; *****************************************************
;; *****************************************************
(use-package company
  ;; Completion
  :ensure t
  :config
  (progn
    ;; Enable company mode in every programming mode.
    (add-hook 'prog-mode-hook 'global-company-mode)
    (setq company-tooltip-limit 20) ; bigger popup window
    (setq company-idle-delay .3)    ; decrease delay before autocompletion popup shows

    (bind-keys :map company-active-map
               ("C-n" . company-select-next)
               ([(tab)] . company-complete)
               )
    ;; (setq company-backends (delete 'company-semantic company-backends))

    (use-package company-statistics
      ;; Rate completions by use.
      :ensure t
      :config
      (add-hook 'after-init-hook 'company-statistics-mode)
      )
    )
  )


;; *****************************************************
;; *****************************************************
;; Handle Projects.
;; *****************************************************
;; *****************************************************
; Been getting more annoyed at not using daemon mode on my main box and
; connecting with emacsclients. Due to work, I use quite a few git-worktree's
; of the same repo. The problem would be accidentally cross editing files
; across the different worktree's (Hence not using daemon mode, and instead
; just running up multiple `emacs --debug-init` sessions for each worktree.
;
; Let's have a go at banishing this behaviour:
;
; * Projectile: Allows for project focus (git repo), whilst also doing fuzzy
;   file searching across the entire project (Nice!)
; * Perspective: Allows for workspaces that when switched to, return the
;   buffers to their original state. Also focuses down the `ido` buffer to the
;   open buffers in that workspace (Nice!)
; * persp-projectile: Combines Projectile and Perspective so that switching
;   projects gives you the Perspective buffer change behaviour (Much nicer than
;   Projectile's insistence that you want to always open a new file but also
;   keep old buffers hanging around).
;
; NOTE: Projectile state is not saved in `desktop-save`.
; NOTE: Perspective mode with IDO only show's files in project, so have to use
; ibuffer to get full list.
;
; https://github.com/bbatsov/projectile
; https://github.com/nex3/perspective-el
; https://github.com/bbatsov/persp-projectile
(use-package projectile
  :ensure t
  :bind ("C-c p" . 'projectile-command-map)
  :init
  (progn
    (projectile-mode)
    (recentf-mode)  ; enables projectile-recentf mode for recent files.
    ; https://github.com/bbatsov/projectile/issues/1183
    ; Projectile now scrapes all files to discover project type for modeline.
    ; This is calculated on every cursor movement, so lags emacs like crazy.
    ; Below is the workaround to disable this until it is fixed.
    (setq projectile-mode-line
         '(:eval (format " Projectile[%s]"
                        (projectile-project-name))))
    )
  )


;; (use-package pipenv
;;   ;; https://github.com/pwalsh/pipenv.el
;;   ;; The replacement to `virtualenv`.
;;   ;; Do `C-cC-pa` or `M-x pipenv-activate` to start a projects pipenv.
;;   :ensure t
;;   :hook (python-mode . pipenv-mode)
;;   :init
;;   (setq
;;    pipenv-projectile-after-switch-function
;;    #'pipenv-projectile-after-switch-default))


;; which-key integration, to show keyboard shortcuts.
(use-package which-key
  :ensure t
  :config
  (which-key-mode))




;; *****************************************************
;; *****************************************************
;; Lsp Client/Server for programming
;; *****************************************************
;; *****************************************************
;; https://github.com/emacs-lsp/lsp-mode
;; https://emacs-lsp.github.io/lsp-mode

;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
(setq lsp-keymap-prefix "s-l")

;; optionally if you want to use debugger
;; (use-package dap-mode
;;   :if (not (eq system-type 'windows-nt))  ;; FIXME: (void-function dap-ui-mode)
;;   :ensure t
;;   :config  ; FIXME: breaks after upgrading to latest.
;;   (dap-ui-mode 1)
;;   (dap-tooltip-mode 1)
;;   (dap-ui-controls-mode 1)
;;   )
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

(use-package lsp-mode
  :ensure t
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)

         ;; Python workflow:
         ;; * `pipenv install --dev python-language-server[all]`.
         ;; * Start pipenv: `C-cC-pa`.
         ;; * Start lsp: `M-x lsp`.
         (rust-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-mode . my-programming-defaults-config)
         )
  :commands lsp
  :config
  (setq
   lsp-file-watch-threshold 20000
        )
  )

;; optionally
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)




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
  :mode ("\\.php\\'" . php-mode)
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
  :mode (
         ("\\.groovy\\'" . groovy-mode)
         ("\\Jenkinsfile*\\'" . groovy-mode)
         )
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
;; NOTE: `flymd` looks to be broken and unmaintained. Use `impatient-mode` for
;; live previews.
(fset 'convert-markdown-ref-to-list
   "\C-[xreplace-regexp\C-m\\[\\(.*\\)\\].*\C-m* [\\1].\C-m")
(fset 'convert-markdown-github-url-to-ref
   "\C-[xreplace-regexp\C-m.*github.com/\\(.*\\)/\\(.*\\)\C-m[Github: \\1/\\2]: https://github.com/\\1/\\2\C-m")

(use-package markdown-mode
  ; NOTE: 'M-x markdown-preview', requires: 'markdown', to be installed with
  ; system package manager.
  :ensure t
  :mode ("\\.md\\'" . markdown-mode)
  :bind (
         ("C-c C-a b" . convert-markdown-ref-to-list)
         ("C-c C-a g" . convert-markdown-github-url-to-ref)
         )
  :init
  (progn
    (add-hook 'markdown-mode-hook 'my-text-mode-config)
    )

  (use-package html-to-markdown
    ;; Convert html code to markdown.
    :ensure t)

  (use-package markdown-toc
    ;; https://github.com/ardumont/markdown-toc
    ;; Used to generate a table of contents in a markdown file.
    :ensure t)
  )

;; *****************************************************
;; *****************************************************
;; Dockerfile
;; *****************************************************
;; *****************************************************
(use-package dockerfile-mode
  :ensure t
  :init
  (add-hook 'dockerfile-mode-hook 'my-programming-defaults-config)
  (add-hook 'dockerfile-mode-hook (lambda () (set (make-local-variable 'compile-command) "docker build .")))
  )

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

;; *****************************************************
;; *****************************************************
;; YAML
;; *****************************************************
;; *****************************************************
(use-package yaml-mode
  ; https://emacs-lsp.github.io/lsp-mode/page/lsp-yaml/
  :ensure t
  :mode (
         ("\\.yml\\'" . yaml-mode)
         ("\\.yaml\\'" . yaml-mode)
         )
  :hook (yaml-mode . lsp)
  )

(use-package ansible
  ; https://github.com/k1LoW/emacs-ansible
  :ensure t
  :config
  (progn
    (add-hook 'yaml-mode-hook '(lambda () (ansible 1)))
    )

  (use-package ansible-doc
    ; https://github.com/lunaryorn/ansible-doc.el
    :ensure t
    :hook (yaml-mode . ansible-doc-mode)
    )

  (use-package company-ansible
    ; https://github.com/krzysztof-magosa/company-ansible
    :ensure t
    :after (company)
    :config
    (add-to-list 'company-backends 'company-ansible)
    )

  )


;; *****************************************************
;; *****************************************************
;; Json programming
;; *****************************************************
;; *****************************************************
(use-package json-mode
  :ensure t
  :mode ("\\.json\\'" . json-mode)
  :hook (
         (json-mode . my-programming-defaults-config)
         (json-mode . (lambda () (auto-fill-mode -1)))  ;; disables auto fill at column.
         (json-mode . (lambda () (setq js-indent-level 2)))
         )
  )

;; *****************************************************
;; *****************************************************
;; REST Client
;; *****************************************************
;; *****************************************************
;; Run up a rest client in emacs to quickly test APIs
;; * http://emacsrocks.com/e15.html
;; * https://github.com/pashky/restclient.el
;;
;; Eg. Run (`C-cC-c`) the following in an empty buffer with restclient-mode on:
;; GET https://api.github.com
;; User-Agent: Emacs Restclient
(use-package restclient
  :ensure t
  :mode ("\\.restclient\\'" . restclient-mode)
  )


;; (use-package realgud
;;   FIXME: Breaks after upgrading.
;;   :ensure t)
;; *****************************************************
;; *****************************************************
;; Python IDE stuff
;; *****************************************************
;; *****************************************************
;; ========================
;; python.el
;; ========================
;; http://www.emacswiki.org/emacs/ProgrammingWithPythonDotEl
;; https://github.com/fgallina/python.el
;; http://www.saltycrane.com/blog/2010/05/my-emacs-python-environment/
(use-package pyvenv
  :ensure t
  :functions pyvenv-autoload
  :config
  (defun pyvenv-autoload ()
    (interactive)
    "auto activate venv directory if exists. See: https://github.com/jorgenschaefer/pyvenv/issues/51"
    (f-traverse-upwards (lambda (path)
                          (let ((venv-path (f-expand ".venv" path)))
                            (when (f-exists? venv-path)
                              (pyvenv-activate venv-path)
                              )))))
  :hook (
         (python-mode . pyvenv-autoload)
         ;; Modified from: https://github.com/jorgenschaefer/pyvenv/issues/95
         ;; FIXME: correct this so it runs LSP after above call, so I don't
         ;; need to do: C-xC-v.
         ;; (pyvenv-post-activate-hooks . lsp)
         )
  )

(use-package lsp-jedi
  ;; https://github.com/fredcamps/lsp-jedi
  ;; https://github.com/pappasam/jedi-language-server
  :ensure t
 )

(use-package python
  :ensure t
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  ;; Python workflow:
  ;; * `pipenv install --dev python-language-server[all]`.
  ;; * Start pipenv: `C-cC-pa`.
  ;; * Start lsp: `M-x lsp`.
  :hook (
         (python-mode . lsp)
         )
  )

(use-package blacken
  ; https://github.com/pythonic-emacs/blacken
  :ensure t
  :hook (python-mode . blacken-mode)
  :init
  (setq blacken-only-if-project-is-blackened t)
  )

;; (use-package python
;;   :defer t
;;   :bind ("\C-m" . newline-and-indent)
;;   :config
;;   (progn
;;     (add-hook 'python-mode-hook 'my-programming-defaults-config)
;;     ;; (setq
;;     ;;  python-indent-offset 4  ;; FIXME: With this all indenting is broken. Without a package moans that it is missing.
;;     ;;  )
;;     ; http://www.emacswiki.org/emacs/ProgrammingWithPythonDotEl#toc1
;;     ;; (add-hook 'python-mode-hook '(lambda () (define-key python-mode-map "\C-m" 'newline-and-indent)))  ; maintain indentation on newline

;;     (use-package sphinx-doc
;;       ; https://github.com/naiquevin/sphinx-doc.el
;;       ; C-c M-d, to auto generate sphinx docs for current function.
;;       :ensure t
;;       :config
;;       (progn
;;         (add-hook 'python-mode-hook (lambda ()
;;                                       (require 'sphinx-doc)
;;                                       (sphinx-doc-mode t)))
;;         )
;;       )

;;     (use-package anaconda-mode
;;       ;; https://github.com/proofit404/anaconda-mode/
;;       ;; Even though Anaconda is based off Jedi, it seems to _"just work"_ for
;;       ;; me on my current latest python3, compared to `jedi` + `company-jedi.`
;;       :ensure t
;;       )

;;     (use-package company-anaconda
;;       ;; https://github.com/proofit404/company-anaconda
;;       ;; `M-?` for docs.
;;       :ensure t
;;       :after company
;;       :config
;;       (add-hook 'python-mode-hook 'anaconda-mode)
;;       (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
;;       (add-to-list 'company-backends 'company-anaconda)
;;       )

;;     (use-package realgud-ipdb
;;       :ensure t
;;       ;:after realgud
;;       )

;;     ;; (use-package cov
;;     ;;   :ensure t
;;     ;;   )
;;     ;; (setq cov-coverage-file-paths '("." "~/work/exchange/" "~/work/exchange/build/test-reports" cov--locate-coveralls cov--locate-clover))
;;     ;; (setq cov-coverage-file "coverage.xml")

;;     ;; (use-package coverage
;;     ;;   :ensure t
;;     ;;   )
;;     ;; (setq coverage-dir "~/work/exchange/")
;;     ;; (setq coverage-dir "~/work/exchange/build/test-reports/")

;;     ;; (use-package undercover
;;     ;;   :ensure t
;;     ;;   )

;;     ;; (use-package pycoverage
;;     ;;   :ensure t
;;     ;;   )

;;     )
;;   )
;; ========================
;; nosetests
;; ========================
;; (use-package nose
;;   :ensure t
;;   :config
;;   (progn
;;     (define-key nose-mode-map "\C-cn" 'nosetests-all)
;;     ;; (define-key python-mode-map "\C-cn" 'nosetests-all)
;;     ;; (define-key python-mode-map "\C-cm" 'nosetests-one)
;;     (setq nose-use-verbose nil) ; default is t  ; nil = dots as output.
;;   )
;; )



;; *****************************************************
;; *****************************************************
;; C++ IDE stuff
;; *****************************************************
;; *****************************************************
(use-package cc-mode
  ;; https://emacs-lsp.github.io/lsp-mode/page/lsp-clangd/
  :ensure t
  :hook (
         (c-mode . lsp)
         (cc-mode . lsp)
         (c++-mode . lsp)
         )
  )

;; (use-package cc-mode
;;   ;; gdb on mac:
;;   ;; brew tap homebrew/dupes && brew install gdb
;;   ;; Note: gdb keybinding is: C-x C-a C-l, which I did have my rename term windows as.
;;   :ensure t
;;   :bind (
;;          ;; ("<f9>" . compile)
;;          ([remap comment-region] . 'recompile)  ; "C-c C-c"
;;          ("M-." . 'xref-find-definitions)  ; https://www.emacswiki.org/emacs/EmacsTags
;;          )
;;   :config
;;   (progn

;;     (use-package smart-compile
;;       :ensure t)

;;     (use-package xcscope
;;       ;; Use cscope files within emacs, to jump around C/C++ code.
;;       ;; https://github.com/dkogan/xcscope.el
;;       :ensure t
;;       :config
;;       (progn
;;         ;; Setup auto-magically hooks into c/c++ modes.
;;         (cscope-setup)
;;         )
;;       (define-key c++-mode-map [remap c-set-style] 'cscope-find-this-symbol)  ;; C-c .
;;       ;; Note etags search defaults to: M-.
;;       )

;;     (use-package company-c-headers
;;       ;; Complete c-headers
;;       :ensure t
;;       :config
;;       (push 'company-c-headers company-backends)
;;       )

;;     ;; cc-mode general settings.

;;     ;; g++-4.9 -g3 -Wall -std=c++11 -stdlib=libc++ -lc++ *.cpp
;;     ;; clang++ -g3 -Wall -std=c++11 -stdlib=libc++ -lc++ *.cpp
;;     (add-to-list 'smart-compile-alist '("\\.[Cc]+[Pp]*\\'" . "clang++ -g3 -Wall -std=c++11 -stdlib=libc++ -lc++ -o %n.out *.cpp"))
;;     (add-hook 'c-mode-common-hook 'my-programming-defaults-config)
;;     (setq c-basic-offset 4)  ;; http://emacswiki.org/emacs/IndentingC
;;     (setq c-default-style "linux")  ;; http://cc-mode.sourceforge.net/html-manual/Built_002din-Styles.html#Built_002din-Styles
;;     ;; FIXME: Either bound this to `*compilation*` window only, so it stops
;;     ;; jumping when I grep, or find the old stop-on-first-error behaviour I
;;     ;; used to use.
;;     (setq compilation-auto-jump-to-first-error nil)
;;     )
;;   (define-key c++-mode-map [remap comment-region] 'compile)  ;; C-c C-c
;;   )

(use-package clang-format
  ;; Applies clang-format to C++ files based on a .clang-format file in the
  ;; project.
  ;; requires `clang-format` to be installed from system package manger.
  :ensure t
  :after cc-mode
  :config
  (progn
    (define-key c++-mode-map (kbd "C-c #") 'clang-format-region)
    )
  )

(defun create-tags (dir-name)
  "Create tags file in directory: DIR-NAME."
  (interactive "Directory: ")
  (eshell-command
   ; (format "find %s -type f -name \"*.[ch]\" | etags -" dir-name))) ;; `.c`/`.h` in a non-git repo.
   (format "cd $(git rev-parse --show-toplevel) && git ls-files | etags -" dir-name)))  ;; tag all files.

(use-package cmake-mode
  ;; https://emacs-lsp.github.io/lsp-mode/page/lsp-cmake/
  ;; pipenv install --dev cmake-language-server
  ;; Bit weird, but need to activate pipenv on a python file in the repo, then
  ;; reload the CMakeList.txt`.
  :ensure t
  :hook (cmake-mode . lsp)
  )

;; *****************************************************
;; *****************************************************
;; C# IDE stuff
;; *****************************************************
;; *****************************************************
; FIXME: keep getting: `Unable to activate package ‘csharp-mode’.` messages,
; so disabling until I have time to re-implement with `lsp-mode`.

;; (defun my-csharp-mode-syntax ()
;;   "Hook for my tweaks to 'csharp-mode'."
;;   (interactive)
;;   ;; https://www.gnu.org/software/emacs/manual/html_node/efaq/Indenting-switch-statements.html
;;   ;; https://stackoverflow.com/questions/3954607/c-sharp-emacs-mode-questions-indentation-and-build#3956173
;;   ;; http://kirste.userpage.fu-berlin.de/chemnet/use/info/cc-mode/cc-mode_6.html
;;   ;; `C-cC-s` to see indent at point.
;;   (c-set-offset `inline-open 0)  ; Stop brackets being indented further on a method.
;;   )

;; (use-package csharp-mode
;;   ;; https://jamiecollinson.com/blog/my-emacs-config/#c-1
;;   :ensure t
;;   :init
;;   (add-hook 'csharp-mode-hook 'my-programming-defaults-config)
;;   (add-hook 'csharp-mode-hook 'my-csharp-mode-syntax)
;;   ;; https://stackoverflow.com/questions/4608679/can-i-change-emacs-default-compile-command
;;   (add-hook 'csharp-mode-hook (lambda () (set (make-local-variable 'compile-command) "cd $(git rev-parse --show-toplevel) && dotnet run")))

;;   (use-package omnisharp
;;     ;; https://github.com/OmniSharp/omnisharp-emacs
;;     ;; https://jamiecollinson.com/blog/my-emacs-config/#c-1
;;     ;; https://www.tuicool.com/articles/22a2Ejb
;;     ;; NOTE: Needs a project with a `.csproj` file to do completions. Done with:
;;     ;; `dotnet new <project_type>`
;;     ;; FIXME: Deferring since I don't have omnisharp installed. Currently not
;;     ;; doing csharp. Should do a check of packages installed.
;;     :defer t
;;     :after company
;;     :bind (:map omnisharp-command-map
;;            ;; FIXME: Make these not global to C++ !!
;;            ("C-c f" . 'omnisharp-run-code-action-refactoring)  ; Refactor/missing_imports/etc...
;;            ("M-." . 'omnisharp-go-to-definition)
;;            )
;;     :config
;;     (add-hook 'csharp-mode-hook 'omnisharp-mode)
;;     (add-to-list 'company-backends 'company-omnisharp))

;;   (use-package coverlay
;;     ;; https://github.com/twada/coverlay.el
;;     ;; Coverage from an LCOV file.
;;     ;; Watch a file via: `M-x coverlay-watch-file /path/to/lcov-file`. or:
;;     ;; `C-c C-l w`.
;;     :ensure t
;;     :init
;;     (setq coverlay:mark-tested-lines nil)
;;     )
;;   )


;; *****************************************************
;; *****************************************************
;; Java IDE stuff
;; *****************************************************
;; *****************************************************
(defun my-java-mode-syntax ()
  "Hook for my tweaks to 'java-mode'."
  (interactive)
  ;; https://www.gnu.org/software/emacs/manual/html_node/efaq/Indenting-switch-statements.html
  (c-set-offset 'case-label '+)  ; A "case" or "default" label.
  (c-set-offset 'brace-list-entry '++)  ; Subsequent lines in an enum or static array list.
  (c-set-offset `arglist-intro `+)  ; function fields on a new line.
  )

;; https://writequit.org/eos/eos-java.html
;; https://github.com/dakrone/emacs-java-imports
;; https://github.com/mopemope/meghanada-emacs
(use-package meghanada
  :ensure t
  :init
  (add-hook 'java-mode-hook #'meghanada-mode)
  (add-hook 'java-mode-hook 'flycheck-mode)
  (add-hook 'java-mode-hook 'my-programming-defaults-config)
  ;; Java warnings stop compilation scrolling, so let's always scroll.
  (add-hook 'java-mode-hook (lambda() compilation-scroll-output t))
  (add-hook 'java-mode-hook (lambda () compile-command "cd $(git rev-parse --show-toplevel) && mvn clean verify"))
  (add-hook 'java-mode-hook 'my-java-mode-syntax)
  )

(use-package mvn
  :ensure t
  :init
  ;; Correctly colourise the compilation buffer for maven calls.
  ;; https://github.com/apg/mvn-el
  (ignore-errors
    (require 'ansi-color)
    (defun colorize-compilation-buffer ()
      (when (eq major-mode 'compilation-mode)
        (let ((inhibit-read-only t))
          (if (boundp 'compilation-filter-start)
              (ansi-color-apply-on-region compilation-filter-start (point))))))
    (add-hook 'compilation-filter-hook 'colorize-compilation-buffer))
  )


;; *****************************************************
;; *****************************************************
;; Javascript IDE stuff
;; *****************************************************
;; *****************************************************
; https://emacs.cafe/emacs/javascript/setup/2017/05/09/emacs-setup-javascript-2.html
; https://emacs-lsp.github.io/lsp-mode/tutorials/reactjs-tutorial/ = ts-ls.
; https://emacs-lsp.github.io/lsp-mode/page/lsp-eslint/
; https://classic.yarnpkg.com/en/docs/cli/global
; - move yarn global install path to home dir and then install eslint globally.
; `yarn config set prefix ~/.yarn`
; `npx -p node@14 yarn global add eslint`
; `M-x lsp-install-server <ret> eslint <ret>`
(use-package js2-mode
  :ensure t
  :mode ("\\.js\\'" "\\.mjs\\'")
  :hook (js2-mode . lsp)
  )
;; FIXME: need to update the path to my local node-modules for my project that
;; is in a sub-directory of the repo.
;; (use-package eslint-fix
;;   ; https://github.com/codesuki/eslint-fix
;;   :ensure t
;;   :hook (
;;          (js-mode . selint-fix)
;;          (js2-mode . selint-fix)
;;          )
;;   :init
;;   (setq eslint-fix-executable "npx eslint")
;; )


;; *****************************************************
;; *****************************************************
;; HTML IDE stuff
;; *****************************************************
;; *****************************************************
; https://emacs-lsp.github.io/lsp-mode/page/lsp-html/
(add-hook 'html-mode-hook 'lsp)
(use-package css-mode
  ; https://emacs-lsp.github.io/lsp-mode/page/lsp-css/
  :ensure t
  :hook (css-mode . lsp)
  )

;https://github.com/skeeto/impatient-mode
(use-package impatient-mode
  ; start webserver with: `M-x httpd-start`.
  ; Then set the mode on the buffer: `M-x impatient-mode`.
  :ensure t
  )

; https://stackoverflow.com/questions/36183071/how-can-i-preview-markdown-in-emacs-in-real-time
(defun markdown-html (buffer)
  "Function to allow `impatient-mode` to preview markdown.  Usage:

* `M-x httpd-start`
* Go to required BUFFER.
* `M-x impatient-mode`
* `M-x imp-set-user-filter RET markdown-html RET`"
  (princ (with-current-buffer buffer
           (format "<!DOCTYPE html><html><title>Impatient Markdown</title><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://strapdownjs.com/v/0.2/strapdown.js\"></script></html>" (buffer-substring-no-properties (point-min) (point-max))))
         (current-buffer)))

;; *****************************************************
;; *****************************************************
;; Rust Mode
;; *****************************************************
;; *****************************************************
; https://github.com/rust-lang/rust-mode
(use-package rust-mode
  :ensure t
  ; https://emacs-lsp.github.io/lsp-mode/page/lsp-rust/
  :hook (rust-mode . lsp)
  )


;; *****************************************************
;; *****************************************************
;; Go Mode
;; *****************************************************
;; *****************************************************
; https://github.com/dominikh/go-mode.el
(use-package go-mode
  :ensure t
  ; Requires `go` & `gopls` to be installed. See:
  ; * https://github.com/golang/tools/tree/master/gopls
  ; * https://emacs-lsp.github.io/lsp-mode/page/lsp-gopls/
  :hook (go-mode . lsp)
  )


;; *****************************************************
;; *****************************************************
;; PowerShell Mode
;; *****************************************************
;; *****************************************************
; https://github.com/jschaf/powershell.el
(use-package powershell
  :ensure t
  :hook (
         (powershell-mode . my-programming-defaults-config)
         ; (powershell-mode . lsp) ;; No `Expand-Archive` on Arch pwsh, so cannot install `pwsh-ls` automatically.
         )
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
	 ("C-c a" . org-agenda)
     ("C-c c" . org-capture))
  :init
  (progn
    (setq
     org-directory "~/org/"
     org-agenda-files (list "~/org/")
     org-default-notes-file "~/org/notes.org"
     ;; refile level.
     ;; http://www.millingtons.eclipse.co.uk/glyn/dotemacs.html
     org-refile-targets (quote
                         ((org-agenda-files :maxlevel . 5)
                          ("~/org/projects.org" :maxlevel . 2)))
     org-log-done t
     org-capture-templates '(
                             ("t" "Todo" entry (file+headline "~/org/todo.org" "UNSORTED")
                              "* TODO %?\n %U - %i\n  %a")
                             ("p" "Project" entry (file+headline "~/org/projects.org" "UNSORTED")
                              "* TODO %?\n %U - %i\n  %a")
                             ("b" "Buy" entry (file+headline "~/org/buy.org" "UNSORTED")
                              "* TODO %?\n %U - %i\n  %a")
                             ("n" "Notes" entry (file+headline "~/org/notes.org" "UNSORTED")
                              "* TODO %?\n %U - %i\n  %a")
                             )
     )

    (global-set-key "\C-cr" (lambda () (interactive) (org-capture nil "t")))
    (global-set-key "\C-cn" (lambda () (interactive) (org-capture nil "n")))
    )
  :config
  (progn
    ;; This is an Emacs package that creates graphviz directed graphs from
    ;; the headings of an org file
    ;; https://github.com/theodorewiles/org-mind-map
    (use-package org-mind-map
      :init
      (require 'ox-org)
      :ensure t
      ;; Uncomment the below if 'ensure-system-packages` is installed
      ;;:ensure-system-package (gvgen . graphviz)
      :config
      (setq org-mind-map-default-graph-attribs
            '(("autosize" . "false")
              ("size" . "9,12")
              ("resolution" . "200")
              ("nodesep" . "0.75")
              ("overlap" . "false")
              ("spline" . "true")
              ("rankdir" . "LR")))
      ;; (setq org-mind-map-engine "dot")       ; Default. Directed Graph
      ;; (setq org-mind-map-engine "neato")  ; Undirected Spring Graph
      (setq org-mind-map-engine "twopi")  ; Radial Layout
      ;; (setq org-mind-map-engine "fdp")    ; Undirected Spring Force-Directed
      ;; (setq org-mind-map-engine "sfdp")   ; Multiscale version of fdp for the layout of large graphs
      ;; (setq org-mind-map-engine "twopi")  ; Radial layouts
      ;; (setq org-mind-map-engine "circo")  ; Circular Layout
      )
    )
)

(use-package plantuml-mode
  ; https://plantuml.com/emacs
  :ensure t
  :after (org org-src)
  :init
  ;; Enable plantuml-mode for PlantUML files
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  :config
  (setq
   org-plantuml-jar-path "~/org/plantuml.jar"
   plantuml-jar-path "~/org/plantuml.jar"
   plantuml-default-exec-mode 'jar
   )
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((plantuml . t)))
  )

;; http://cestlaz.github.io/posts/using-emacs-26-gcal/
(use-package calfw
  :ensure t
  :bind
  (
   ("<f8>" . cfw:open-org-calendar)
   )
  :config
  (progn
    (use-package calfw-gcal
      ;; FIXME: 10year old package with deprecated `cl` requirement.
      ;; TODO: replace for: https://github.com/myuhe/org-gcal.el.
      :ensure t)

    (use-package calfw-ical
      :ensure t)

    (use-package calfw-org
      :ensure t)
    )
  ;; FIXME: what does this do??
  (setq cfw:org-overwrite-default-keybinding t)
  )


;; ========================
;; Github blog
;; ========================
(defun org-custom-link-img-follow (path)
  "PATH to find custom linked images."
  (org-open-file-with-emacs
   (format "~/org/github_blog/images/%s" path)))

(defun org-custom-link-img-export (path desc format)
  "Rewrite custom linked images for export.
PATH - path to images.
DESC - Description to add as alt text..
FORMAT - .format to use."
  (cond
   ((eq format 'html)
    (format "<img src=\"http://jackson15j.github.io/%s\" alt=\"%s\"/>" path desc))))

(require 'org)
;; FIXME: `org-add-link-type` is deprecated. Replace with:
;; `org-link-set-parameters`.
(org-add-link-type "img" 'org-custom-link-img-follow 'org-custom-link-img-export)



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
  :mode "\\.log\\'"
  )
'(log4j-font-lock-fatal-face ((t (:foreground "darkred" :weight bold))))
'(log4j-font-lock-info-face ((t (:foreground "ForestGreen"))))
'(log4j-font-lock-warn-face ((t (:foreground "orange"))))


;; *****************************************************
;; *****************************************************
;; pcap file support
;; *****************************************************
;; *****************************************************
;; https://github.com/orgcandman/pcap-mode
(use-package pcap-mode
  :ensure t)

;; *****************************************************
;; *****************************************************
;; email support (Mutt - external cli Message User Agent (MUA) for emails)
;; *****************************************************
;; *****************************************************
(setq auto-mode-alist (append '((".*mutt.*" . mail-mode)) auto-mode-alist))  ; use mail major mode.


;; *****************************************************
;; *****************************************************
;; email support (mu4e, with mbsync for maildir syncing.
;; *****************************************************
;; *****************************************************
;; NOTE: Pushed out account contexts to personal file.
;; Incomplete list of sources:
;;
;; * http://www.djcbsoftware.nl/code/mu/mu4e.html
;; * http://www.djcbsoftware.nl/code/mu/mu4e/ - manual
;; * http://pragmaticemacs.com/emacs/migrating-from-offlineimap-to-mbsync-for-mu4e/ - old style mbsync config.
;; * http://www.macs.hw.ac.uk/~rs46/posts/2014-01-13-mu4e-email-client.html

;; First Steps:
;; * Download mail via mbsync.
;; * run: mu index -m ~/mail/  ; need to do root folder for mu/mu4e to separate out the accounts!!
;; * Now you can use mu4e.
;; TODO: find a package manager with mu/mu4e in it (if there is a non-system level way).
;; Ubuntu requires apt-get maildir-utils mu4e.
(use-package mu4e
  :if (not (eq system-type 'windows-nt))  ;; FIXME: not installed mu4e on windows yet.
  :load-path "/usr/share/emacs/site-lisp/mu4e"  ;; arch.
  ;; FIXME: Cannot set keyboard variable like this, whilst my mu4e-contexts are
  ;; in a private file. Dies on startup with (void-function make-mu4e-context).
  ; :bind (("<f7>" . mu4e))
  :config
  (progn
    (setq
     mu4e-root-maildir "~/mail"  ;; location of my maildir.
     ;; mu4e-maildir (expand-file-name "~/mail")
     ;;rename files when moving
     ;;NEEDED FOR MBSYNC
     mu4e-change-filenames-when-moving t
     ;; General config.
     mu4e-update-interval 120
     mu4e-headers-auto-update t
     mu4e-view-show-images t
     ;; show full addresses in view message (instead of just names)
     ;; toggle per name with M-RET
     mu4e-view-show-addresses t
     ;; http://www.djcbsoftware.nl/code/mu/mu4e/Displaying-rich_002dtext-messages.html
     ;; If you’re using a dark theme, and the messages are hard to read, it can
     ;; help to change the luminosity, e.g.:
     ;; shr-color-visible-luminance-min 80
     ;;
     ;; Occasionally got emails that were completely white, so had to tend to
     ;; 0 to make them legibly, however this made other html emails white. 20
     ;; seems a good compromise.
     shr-color-visible-luminance-min 20
     )
    )
  (add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t)
  ;; FIXME: html2text is garbage for bitbucket emails. Latest version is native eww.
  ;; uncomment one of these on old mu/emacs versions (0.9.9.6, 24.x).
  ;; (setq mu4e-html2text-command "html2text -utf8 -width 120")  ;; requires apt-get html2text
  ;; (setq mu4e-html2text-command "w3m -T text/html")

  (use-package mu4e-maildirs-extension
    ;; Show mu4e maildirs summary in mu4e-main-view
    ;; https://github.com/agpchil/mu4e-maildirs-extension
    :ensure t
    :init (mu4e-maildirs-extension))

  (use-package mu4e-alert
    ;; Does desktop/modeline notifications.
    ;; https://github.com/iqbalansari/mu4e-alert
    :ensure t
    :config
    (mu4e-alert-set-default-style 'libnotify)
    (add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
    (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)
    )
  )


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

'(browse-url-browser-function (quote browse-url-default-browser))  ; Use system default browser instead of eww.
;; bound this to Darwin only.
(cond
 ((string-equal system-type "darwin")
  (setq browse-url-browser-function  'browse-url-generic
        browse-url-generic-program "/Applications/Opera.app/Contents/MacOS/Opera")))

(use-package language-detection
  ; https://github.com/andreasjansson/language-detection.el
  :ensure t
  )

(require 'cl-lib)

(defun eww-tag-pre (dom)
  "See: https://github.com/andreasjansson/language-detection.el.
DOM - web dom."
  (let ((shr-folding-mode 'none)
        (shr-current-font 'default))
    (shr-ensure-newline)
    (insert (eww-fontify-pre dom))
    (shr-ensure-newline)))

(defun eww-fontify-pre (dom)
  "See: https://github.com/andreasjansson/language-detection.el.
DOM - web dom."
  (with-temp-buffer
    (shr-generic dom)
    (let ((mode (eww-buffer-auto-detect-mode)))
      (when mode
        (eww-fontify-buffer mode)))
    (buffer-string)))

(defun eww-fontify-buffer (mode)
  "See: https://github.com/andreasjansson/language-detection.el.
MODE - ??"
  (delay-mode-hooks (funcall mode))
  (font-lock-default-function mode)
  (font-lock-default-fontify-region (point-min)
                                    (point-max)
                                    nil))

(defun eww-buffer-auto-detect-mode ()
  "See: https://github.com/andreasjansson/language-detection.el."
  (let* ((map '((ada ada-mode)
                (awk awk-mode)
                (c c-mode)
                (cpp c++-mode)
                (clojure clojure-mode lisp-mode)
                ; (csharp csharp-mode java-mode)
                (css css-mode)
                (dart dart-mode)
                (delphi delphi-mode)
                (emacslisp emacs-lisp-mode)
                (erlang erlang-mode)
                (fortran fortran-mode)
                (fsharp fsharp-mode)
                (go go-mode)
                (groovy groovy-mode)
                (haskell haskell-mode)
                (html html-mode)
                (java java-mode)
                (javascript javascript-mode)
                (json json-mode javascript-mode)
                (latex latex-mode)
                (lisp lisp-mode)
                (lua lua-mode)
                (matlab matlab-mode octave-mode)
                (objc objc-mode c-mode)
                (perl perl-mode)
                (php php-mode)
                (prolog prolog-mode)
                (python python-mode)
                (r r-mode)
                (ruby ruby-mode)
                (rust rust-mode)
                (scala scala-mode)
                (shell shell-script-mode)
                (smalltalk smalltalk-mode)
                (sql sql-mode)
                (swift swift-mode)
                (visualbasic visual-basic-mode)
                (xml sgml-mode)))
         (language (language-detection-string
                    (buffer-substring-no-properties (point-min) (point-max))))
         (modes (cdr (assoc language map)))
         (mode (cl-loop for mode in modes
                        when (fboundp mode)
                        return mode)))
    (message (format "%s" language))
    (when (fboundp mode)
      mode)))

(setq shr-external-rendering-functions
      '((pre . eww-tag-pre)))

;; ========================
;; Stackoverflow search (SOS)
;; ========================
;; FIXME: package doesn't exist any more?
;; (use-package sos
;;   :ensure t
;;   :bind (("<f5>" . sos))
;;   )


;; FIXME: appears to not exist anymore.
;; ;; ========================
;; ;; iRFC (Download & View RFC's)
;; ;; ========================
;; (use-package irfc
;;   :ensure t
;;   :config
;;   (progn
;;     (setq
;;      irfc-directory "~/Downloads/rfcs/"
;;      irfc-assoc-mode t)
;;     )
;;   )



;; Showing pop-ups
;; http://emacs-fu.blogspot.com/2009/11/showing-pop-ups.html
;;
;; TODO: figure out why the built in `notifications` package doesn't play
;; sounds:
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Desktop-Notifications.html,
;; since this would be nicer to move to a standardised package.
(defun djcb-popup (title msg &optional timeout icon sound)
  "Show a popup if we're on X, or echo it otherwise;
TITLE is the title of the message, MSG is the context.
Optionally, you can provide a TIMEOUT (milliseconds, default=5000) an ICON and
a SOUND to be played (default=/../alert.wav)"
  (interactive)
  (shell-command
   (concat "mplayer -really-quiet "
           (if sound sound "/usr/share/sounds/purple/alert.wav")
           " 2> /dev/null"))
  ;; Removed `(if (eq window-system 'x)` check since it wasn't doing the
  ;; notify-send on my terminal emacs session nested in tmux in a terminal
  ;; under cinnamon.
  (shell-command (concat "notify-send"
                         (if icon (concat " -i " icon) "")
                         (if timeout (concat " -t " timeout) " -t 5000")
                         " '" title "' '" msg "'"))
  ;; text only version
  (message (concat title ": " msg)))


;; Run example:
;; (djcb-popup "Warning" "The end is near"
;;             nil
;;             "/usr/share/icons/gnome/128x128/apps/libreoffice-base.png"
;;             "/usr/share/sounds/purple/alert.wav")



;; ========================
;; Mingus (MPD client)
;; ========================
;; MPD references:
;; * https://github.com/dakrone/eos/blob/master/eos-music.org
;; * https://wiki.archlinux.org/index.php/Music_Player_Daemon
;; * https://wiki.archlinux.org/index.php/Ncmpcpp
(use-package mingus
  :ensure t
  :bind
  (
   ("C-c m" . mingus)
   ("<f9>" . mingus-toggle)
   ("C-<f12>" . mingus-prev)
   ("<f12>" . mingus-next)
   ; "C-<f2>
   ("M-[ 1 ; 5 q" . mingus-vol-down)
   ; "C-<f3>
   ("M-[ 1 ; 5 r" . mingus-vol-up)
   )
  )

;; ========================
;; md4rd (Reddit client)
;; ========================
;; https://github.com/ahungry/md4rd
;; FIXME: uncomment once `Debugger entered--Lisp error: (void-variable hierarchy--make)` is fixed.
;; (use-package md4rd
;;   :ensure t
;;   )


;; FIXME: setup the exwm windows manager.
;; (use-package xelb
;;   :ensure t
;;   :config
;;   (use-package exwm
;;     :ensure t
;;     :config
;;     ('exwm-config)
;;     ('exwm-config-default)
;;     )
;;   )


;; ;; wiki.archlinux.org/index.php/EXWM
;; ;; FIXME: convert to `use-package` format.
;; (require 'exwm)
;; (require 'exwm-config)
;; (exwm-config-default)

;; ;; (require 'exwm-systemtray)
;; ;; (exwm-systemtray-enable)
;; (require 'exwm-randr)
;; (setq exwm-randr-workspace-output-plist '(1 "eDP1" 2 "HDMI2"))
;; (add-hook 'exwm-randr-screen-change-hook
;;           (lambda ()
;;             (start-process-shell-command
;;              "xrandr" nil "xrandr --output eDP1 --below HDMI2 --auto")))
;; (exwm-randr-enable)

;; (exwm-enable)


;; ========================
;; el-get (External Package manager)
;; ========================
;; https://www.emacswiki.org/emacs/el-get
(use-package el-get
  :ensure t)


(use-package alert
  :commands (alert)
  :init
  (setq alert-default-style 'notifier))



;; ========================
;; speed-type
;; ========================
;; https://github.com/parkouss/speed-type/ (was: https://github.com/hagleitn/speed-type).
;; Touch typing practice.
;; Call: `M-x speed-type-text`.
(use-package speed-type
  :ensure t)

;; ========================
;; spray (spritz clone - speed reading)
;; ========================
;; https://github.com/ian-kelling/spray
(use-package spray
  :ensure )

;; ========================
;; MscGen mode (Custom)
;; ========================
;; http://www.mcternan.me.uk/mscgen/
;; https://emacs-fu.blogspot.com/2010/04/creating-custom-modes-easy-way-with.html
(define-generic-mode
    'mscgen-mode                        ;; name of the mode to create
  '("#")                           ;; comments start with '#'
  '("label" "note" "width"
    "textcolour" "linecolour" "textbgcolour")                     ;; some keywords
  '(("=" . 'font-lock-operator)     ;; '=' is an operator
    ("=>" . 'font-lock-operator)
    ("->" . 'font-lock-operator)
    (";" . 'font-lock-builtin)     ;; ';' is a a built-in
    ("[" . 'font-lock-builtin)
    ("]" . 'font-lock-builtin)
    ("|" . 'font-lock-builtin)
    )
  '("\\.msc$")                      ;; files for which to activate this mode
  nil                         ;; other functions to call
  "A mode for mscgen files"            ;; doc string for this mode
  )
(defun mscgen-compile-buffer-hook()
  "Compile command to generate a PNG from the current mscgen buffer.

See: https://stackoverflow.com/questions/6138029/how-to-add-a-hook-to-only-run-in-a-particular-mode
for the use of the hook."
  (compile (concat "mscgen -T png " buffer-file-name " && mscgen -T svg " buffer-file-name))
  (message (concat "Generated PNG/SVG for: " buffer-file-name))
  )
(add-hook 'mscgen-mode-hook 'my-programming-defaults-config)
(add-hook 'mscgen-mode-hook
          (lambda ()
            (add-hook 'after-save-hook 'mscgen-compile-buffer-hook nil 'make-it-local)))



(use-package csv-mode
  :ensure t)

(use-package i3wm-config-mode
  :ensure t)


(use-package iss-mode
  ; https://github.com/rasmus-toftdahl-olesen/iss-mode
  ; InnoSetup mode: https://jrsoftware.org/isinfo.php
  :mode "\\.iss\\'"
  :ensure t
  :init
  (setq iss-compiler-path "C:/Program Files (x86)/Inno Setup 6")
  )


(use-package devdocs
  ; https://github.com/astoff/devdocs.el
  :ensure t
  :hook (
         (c-mode . (lambda () (setq-local devdocs-current-docs '("c"))))
         (c++-mode . (lambda () (setq-local devdocs-current-docs '("cpp" "cmake~3.20"))))
         (python-mode . (lambda () (setq-local devdocs-current-docs '("python~3.9" "django~3.2" "django_rest_framework"))))
         )
  )




;; ========================
;; Finally; Load extra dot files (if they exist)
;; ========================
(use-package cus-edit
  :custom (custom-file "~/configs/emacs/custom_set_variables.el" "Moved custom-set-variables to it's own file")
  )

(let () (dolist (dot_emacs '("~/configs/emacs/custom_set_variables.el"
                             "~/configs/emacs/private_dot_emacs.el"
                             "~/configs/emacs/unstable_config_dot_emacs.el"
                             "~/configs/emacs/work_specific_dot_emacs.el"))
          "Loading my extra emacs dot files if they exist."
          (when (file-exists-p dot_emacs)
            (message (concat "Loading external dot file: " dot_emacs))
            (load-file dot_emacs))))


;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
(provide '.emacs)
;;; .emacs ends here

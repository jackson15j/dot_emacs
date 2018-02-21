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


(use-package server
  :config
  (server-start))

;; *****************************************************
;; Bug-Hunter: Debug lisp files
;; *****************************************************
;; https://github.com/Malabarba/elisp-bug-hunter
(use-package bug-hunter
  :ensure t)

;; *****************************************************
;; *****************************************************
;; Various Tweaks (one-liners and minor package config).
;; *****************************************************
;; *****************************************************
(put 'downcase-region 'disabled nil)  ; allow downcase-region without the disabled feature warning.
(put 'upcase-region 'disabled nil)  ; allow upcase-region without the disabled feature warning.
'(flycheck-error-list-column-number ((t (:inherit font-lock-constant-face :background "blue"))))
'(flycheck-warning ((t (:background "color-17" :underline (:color "DarkOrange" :style wave)))))
(setq calendar-week-start-day 1)
(setq compilation-scroll-output 't)
'(browse-url-browser-function (quote browse-url-default-browser))  ; Use system default browser instead of eww.

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
  (setq
   sml/no-confirm-load-theme t
   sml/theme 'dark
   sml/mode-width `full
   )
  (sml/setup)
  (column-number-mode t)
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
  (show-paren-mode 1)  ;; highlight matching brackets
  )
(add-hook 'sh-mode-hook 'my-programming-defaults-config)

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

    (use-package magithub
      ;; https://github.com/vermiculus/magithub
      ;; Uses ~/.authinfo.gpg for token.
      :after magit
      :ensure t
      :config
      (magithub-feature-autoinject t)
      (add-hook 'magithub-issue-post-mode-hook 'flyspell-mode)
      )

    )
  )
'(magit-item-highlight ((t nil)) t)



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
    (setq company-auto-complete nil) ; don't auto complete words when I'm hitting space to move on.

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
    (add-to-list 'auto-mode-alist '("jenkinsfile\\'" . groovy-mode))
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

  (use-package flymd
    ; https://github.com/mola-T/flymd/blob/master/browser.md. Live preview
    ; markdown changes in the browser. Much better than `markdown-preview`.
    :ensure t
    :init
    (defun my-flymd-browser-function (url)
      "flymd is broken on Chrome based browsers so going with
Option 1: use Firefox
instead. https://github.com/mola-T/flymd/blob/master/browser.md"
      (let ((browse-url-browser-function 'browse-url-firefox))
        (browse-url url)))
    :config
    (setq
     flymd-browser-open-function 'my-flymd-browser-function
     flymd-close-buffer-delete-temp-files t
     flymd-output-directory "/tmp/"
     )
    ;; Removed hook, since I don't want every markdown file to open in the
    ;; browser.
    ;; (add-hook 'markdown-mode-hook 'flymd-flyit)
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
  (progn
    (add-hook 'dockerfile-mode-hook 'my-programming-defaults-config)
    )
  )

;; *****************************************************
;; *****************************************************
;; YAML
;; *****************************************************
;; *****************************************************
(use-package yaml-mode
  :ensure t
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
    )
  :init
  (progn
    (add-hook 'yaml-mode-hook 'my-programming-defaults-config)
    (add-hook 'yaml-mode-hook
              '(lambda ()
                 (define-key yaml-mode-map "\C-m" 'newline-and-indent)))
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
    :mode ("\\.restclient\\'" . restclient-mode))

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
;; (use-package pyvenv
;;   :ensure t
;;   ;; FIXME: Need to do a buffer reload after pyvenv-workon. Add a hook.
;;   )

;; ;; http://cestlaz.github.io/posts/using-emacs-27-shell/#.WL2fFV7iphH
;; (use-package virtualenvwrapper
;;   :ensure t
;;   :config
;;   (progn
;;     (setq
;;      venv-location "~/virt_envs/"
;;      ; Show virtualenv in eshell.
;;      eshell-prompt-function 'my-eshell-prompt
;;      )
;;     )
;;   (venv-initialize-interactive-shells)
;;   (venv-initialize-eshell)
;;   )

;; (defun pwd-replace-home (pwd)
;;   "Replace home in PWD with tilde (~) character."
;;   (interactive)
;;   (let* ((home (expand-file-name (getenv "HOME")))
;;          (home-len (length home)))
;;     (if (and
;;          (>= (length pwd) home-len)
;;          (equal home (substring pwd 0 home-len)))
;;         (concat "~" (substring pwd home-len))
;;       pwd)))

;; (defun my-eshell-prompt ()
;;   "Change prompt to from default of: `~/path/ $ `, to also have virtualenv."
;;   (concat
;;    (pwd-replace-home(eshell/pwd))
;;    (if venv-current-name (concat " [" venv-current-name "]"))
;;    " $ "
;;    )
;;   )

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

    (use-package company-jedi
      ;; Complete python via jedi-core and company.
      ;; Since we're using this package we don't need `jedi`, which is based
      ;; on `auto-complete`. Now we're fully `company-mode`.
      :ensure t
      :init
      (add-hook 'python-mode-hook (lambda() (add-to-list 'company-backends 'company-jedi)))
      :config
      (progn
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

    (use-package company-c-headers
      ;; Complete c-headers
      :ensure t
      :config
      (push 'company-c-headers company-backends)
      )

    ;; cc-mode general settings.

    ;; g++-4.9 -g3 -Wall -std=c++11 -stdlib=libc++ -lc++ *.cpp
    ;; clang++ -g3 -Wall -std=c++11 -stdlib=libc++ -lc++ *.cpp
    (add-to-list 'smart-compile-alist '("\\.[Cc]+[Pp]*\\'" . "clang++ -g3 -Wall -std=c++11 -stdlib=libc++ -lc++ -o %n.out *.cpp"))
    (add-hook 'c++-mode-hook 'my-programming-defaults-config)
    (setq c-basic-offset 4)  ;; http://emacswiki.org/emacs/IndentingC
    (setq c-default-style "linux")  ;; http://cc-mode.sourceforge.net/html-manual/Built_002din-Styles.html#Built_002din-Styles
    )
  (define-key c++-mode-map [remap comment-region] 'compile)  ;; C-c C-c
  )

(use-package clang-format
  ;; Applies clang-format to C++ files based on a .clang-format file in the
  ;; project.
  ;; requires `clang-format` to be installed from system package manger.
  :ensure t
  :config
  (progn
    (define-key c++-mode-map (kbd "C-c #") 'clang-format-region)
    )
  )


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
     org-default-notes-file "~/org/notes.org"
     ;; refile level.
     ;; http://www.millingtons.eclipse.co.uk/glyn/dotemacs.html
     org-refile-targets (quote
                         ((org-agenda-files :maxlevel . 5)
                          ("~/org/projects.org" :maxlevel . 2)))
     org-log-done t
     )
    ;; agenda items. (http://orgmode.org/worg/org-tutorials/orgtutorial_dto.html).
    (dolist (my-agendas '("~/org/projects.org"
                          "~/org/todo.org"
                          "~/org/notes.org"))
      (loop for agenda in my-agendas do
            (if (file-exists-p agenda)
                (add-to-list 'org-agenda-files agenda)
              (message "Could not find: %s" agenda))))

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
  :load-path "/usr/share/emacs/site-lisp/mu4e"  ;; arch.
  :config
  (progn
    (setq
     mu4e-maildir "~/mail"  ;; location of my maildir.
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

  ;; bookmarks
  (if (fboundp 'make-mu4e-bookmark)
      ;; if `mu/mu4e` is modern enough that it has the `make-mu4e-bookmark`
      ;; variable, then add my custom bookmark.
      ;; http://ergoemacs.org/emacs/elisp_check_defined.html.
      (add-to-list 'mu4e-bookmarks
                   (make-mu4e-bookmark
                    :name  "Big messages"
                    :query "size:5M..500M"
                    :key ?b)))

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
;; iRFC (Download & View RFC's)
;; ========================
(use-package irfc
  :ensure t
  :config
  (progn
    (setq
     irfc-directory "~/Downloads/rfcs/"
     irfc-assoc-mode t)
    )
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



;; Showing pop-ups
;; http://emacs-fu.blogspot.com/2009/11/showing-pop-ups.html
;;
;; TODO: figure out why the built in `notifications` package doesn't play
;; sounds:
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Desktop-Notifications.html,
;; since this would be nicer to move to a standardised package.
(defun djcb-popup (title msg &optional timeout icon sound)
  "Show a popup if we're on X, or echo it otherwise; TITLE is the
title of the message, MSG is the context. Optionally, you can
provide a timeout (milliseconds, default=5000) an ICON and a
sound to be played (default=/../alert.wav)"
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


;; wiki.archlinux.org/index.php/EXWM
;; FIXME: convert to `use-package` format.
(require 'exwm)
(require 'exwm-config)
(exwm-config-default)

(require 'exwm-randr)
(setq exwm-randr-workspace-output-plist '(1 "eDP1" 2 "HDMI2"))
(add-hook 'exwm-randr-screen-change-hook
          (lambda ()
            (start-process-shell-command
             "xrandr" nil "xrandr --output eDP1 --below HDMI2 --auto")))
(exwm-randr-enable)

(exwm-enable)

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

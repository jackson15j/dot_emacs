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
(make-directory "~/.emacs.d/elpa" t)

(let* ((my-emacsd-dir "~/.emacs.d/elpa/")
       (default-directory my-emacsd-dir)
       (orig-load-path load-path))
  (setq load-path (cons my-emacsd-dir nil))
  (normal-top-level-add-subdirs-to-load-path)
  (nconc load-path orig-load-path))

(require 'package)
(setq package-eanable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("elpa" .  "https://elpa.gnu.org/packages/" ) t)
(package-initialize)
;; Follow symlinks. NOTE: `~/config.org` is a symlink!!
(setq vc-follow-symlinks t)

(use-package system-packages
;; `use-package` is a built-in in emacs29, but need to install
;; `system-packages` for `use-package-ensure-system-package` built-in to work.
  :ensure t
)
;; Run config from an org file.
;; https://himmallright.gitlab.io/post/org-babel-setup/
(org-babel-load-file "~/config.org")

(use-package posframe
  ;; Seeing LSP and other packages blowing up on this missing requirement.
  :ensure t
  :defer t
  )

(use-package treemacs
  ;; Seeing LSP and other packages blowing up on this missing requirement.
  :ensure t
  :defer t
  )


(use-package windmove
  ;; Builtin method of moving between windows with (default) `Shift+<arrow>`.
  ;; https://www.emacswiki.org/emacs/WindMove
  ;; https://pragmaticemacs.wordpress.com/2016/12/26/whizz-between-windows-with-windmove/
  :ensure t
  :config
  (windmove-default-keybindings)
  )



;; *****************************************************
;; *****************************************************
;; Various Tweaks (one-liners and minor package config).
;; *****************************************************
;; *****************************************************
(global-auto-revert-mode)
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

;; backup files - https://www.emacswiki.org/emacs/BackupDirectory
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.emacs.d/.backups/"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups
(make-directory "~/.emacs.d/.backups/" t)
;; https://emacs.stackexchange.com/questions/17210/how-to-place-all-auto-save-files-in-a-directory
(setq auto-save-file-name-transforms
      `(
        ("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'" "/tmp/\\2" t)
        (".*" "~/.emacs.d/.auto-saves/" t)
        )
      )
(make-directory "~/.emacs.d/.auto-saves/" t)
;; Don't save lock files by files - https://www.emacswiki.org/emacs/LockFiles.
(setq create-lockfiles nil)

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
  "Major modes on which to disable the linum mode.
Exempts them from global requirement."
  :group 'display-line-numbers
  :type 'list
  :version "green")
(defun display-line-numbers--turn-on ()
  "Turn on line numbers but exempting certain major modes.
Defined in: `display-line-numbers-exempt-modes'."
  (if (and
       (not (member major-mode display-line-numbers-exempt-modes))
       (not (minibufferp)))
      (display-line-numbers-mode)))



;; http://pragmaticemacs.com/emacs/dired-human-readable-sizes-and-sort-by-size/
(setq dired-listing-switches "-alh")


;; Allow `Alt+3` on a Mac to be `#`:
;; https://stackoverflow.com/questions/1704119/carbon-emacs-re-enable-hash-key
;; https://stackoverflow.com/questions/3977069/emacs-question-hash-key
(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))
(define-key isearch-mode-map (kbd "M-3") '(lambda () (interactive) (isearch-process-search-char ?\#)))


;; ========================
;; Fill Column (used to reflow text automatically & highlight margins)
;; ========================
;; different 79 char ruler, that's solid down.
;; http://www.emacswiki.org/FillColumnIndicator
;; Also changed the column fill to be a double pipe. See unicode table.
(use-package fill-column-indicator
  :ensure t
  :defer t
  :config
  (progn
    (setq-default fci-rule-column 79)
    (setq fci-rule-character ?\u2016)
    ;; automatically wrap to 79 characters.
    (setq-default fill-column 79)
    (setq-default git-commit-fill-column 79))
)


;; ========================
;; Lookup dictionary definitions.
;; ========================
;; `M-x dictionary-search` look up word definition.
(use-package dictionary
  :ensure t
  :defer t)



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
  "Un/Comments the region or the current line if there's no active region."
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
  "All of my `text-mode` config in one place."
  (interactive)
  (whitespace-mode)  ;; highlights whitespace.
  (my_highlighted_words)  ;; highlights specific words in red & bold.
  (fci-mode)  ;; adds fill column indicator.
  (auto-fill-mode)  ;; wraps at auto fill column.
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
  (setq indent-tabs-mode nil)  ;; spaces instead of tabs
  (setq tab-width 4)  ;; 4 spaces per tab key press.

  ;; TODO: raise a bug on which-function-mode breaking in python when opening a
  ;; triple double-qoute (`"""`) docstring in a function and then emacs
  ;; freezes. Replicated on work files with: `emacs -q`, but failed to
  ;; replicate so far on a quickly mocked up file in /tmp/.
  ;;
  ;; (which-function-mode)  ;; Display current function in mode line. (http://emacsredux.com/blog/2014/04/05/which-function-mode/)
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
  :defer t
  :bind (:map flycheck-mode-map
         ("C-c e" . list-flycheck-errors)
         ("C-c T f" . flycheck-mode)
         ("C-c j" . flycheck-next-error)
        )
  ;; :init (global-flycheck-mode)
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

;; ;; Chain modes after `lsp`.
;; ;; https://rat.dev/flycheck/flycheck/issues/1762
;; (defvar-local my/flycheck-local-cache nil)
;; (defun my/flycheck-checker-get (fn checker property)
;;   (or (alist-get property (alist-get checker my/flycheck-local-cache))
;;       (funcall fn checker property)))
;; (advice-add 'flycheck-checker-get :around 'my/flycheck-checker-get)
;; (add-hook 'lsp-managed-mode-hook
;;           (lambda ()
;;             (when (derived-mode-p 'python-mode)
;;               (setq my/flycheck-local-cache '((lsp . ((next-checkers . (python-pylint)))))))))


;; *****************************************************
;; *****************************************************
;; Lsp Client/Server for programming
;; *****************************************************
;; *****************************************************
;; https://github.com/emacs-lsp/lsp-mode
;; https://emacs-lsp.github.io/lsp-mode

;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
(setq lsp-keymap-prefix "s-l")


(use-package lsp-mode
  :ensure t
  :defer t
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
   gc-cons-threshold 100000000
   read-process-output-max (* 1024 1024 4)  ;; 4MB
        )
  )

;; optionally
(use-package lsp-ui
  ;; https://github.com/emacs-lsp/lsp-ui
  :ensure t
  :defer t
  :commands (
             lsp-ui-mode
             lsp-ui-peek-mode
             lsp-ui-sideline-mode
             )
  :bind (:map lsp-command-map
         ([remap xref-find-definitions] . #'lsp-ui-peek-find-definitions)  ;; M-.
         ([remap xref-find-references] . #'lsp-ui-peek-find-references)  ;; M-?
         )
  :config
  (setq
   lsp-ui-doc-show-with-cursor t
   )
  )

(use-package lsp-treemacs
  :after (lsp))


;; optionally if you want to use debugger
(use-package dap-mode
  :if (not (eq system-type 'windows-nt))  ;; FIXME: (void-function dap-ui-mode)
  :ensure t
  :defer t
  :bind (
         ([f6] . dap-hydra)
         ([f7] . 'dap-ui-repl)
         )
  :commands
  (
   dap-mode
   dap-ui-mode
   dap-tooltip-mode
   dap-ui-controls-mode
   )
  :config
  (setq
  ;; NOTE: For Python. install: `debugpy` python package in the project!
   dap-python-debugger 'debugpy   ;; The default: `ptvsd` is deprecated!
   dap-ui-variable-length 1000  ;; https://github.com/emacs-lsp/dap-mode/issues/416 - don't truncate `locals` variables.
   dap-internal-terminal 'dap-internal-terminal-shell  ;; Forgotten how to scroll `vterm` so using shell.
   )
  ;; https://www.reddit.com/r/emacs/comments/tckmb2/dapmode_breakpoints_not_showing_when_in_terminal/
  (add-hook 'dap-stopped-hook
            (lambda (arg) (call-interactively #'dap-hydra)))
  )
;; (use-package dap-LANGUAGE) to load the dap adapter for your language
(with-eval-after-load 'dap-faces
  (unless (display-graphic-p)
    (set-face-background 'dap-ui-marker-face "color-166") ; An orange background for the line to execute
    (set-face-attribute 'dap-ui-marker-face nil :inherit nil) ; Do not inherit other styles
    (set-face-background 'dap-ui-pending-breakpoint-face "blue") ; Blue background for breakpoints line
    (set-face-attribute 'dap-ui-verified-breakpoint-face nil :inherit 'dap-ui-pending-breakpoint-face)
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
;; FIXME: erlang mode is throwing errors on start in latest emacs28!!
;; (use-package erlang
;;   ;; (require 'erlang-start)
;;   :ensure t
;;  :defer t
;;   )



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
  :defer t
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
  :defer t
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
;; Dockerfile
;; *****************************************************
;; *****************************************************
(use-package dockerfile-mode
  :ensure t
  :defer t
  :after (flycheck lsp-mode)
  :hook
  (
   (dockerfile-mode . lsp)
   (dockerfile-mode . (lambda () (set (make-local-variable 'compile-command) "docker build .")))
   ;; (dockerfile-mode . (lambda () (lsp-deferred) (flycheck-add-next-checker 'lsp 'dockerfile-hadolint)))
   )
  )

(use-package docker
  :ensure t
  :defer t
  :bind ("C-c d" . docker)
  :config
  ;; https://github.com/Silex/docker.el/issues/188
  ;; Don't use vterm everywhere.
  (setq
   docker-run-async-with-buffer-function 'docker-run-async-with-buffer-shell
   docker-container-columns '(
                              (:name "Names" :width 30 :template "{{ json .Names }}" :sort nil :format nil)
                              (:name "Status" :width 30 :template "{{ json .Status }}" :sort nil :format nil)
                              (:name "Image" :width 40 :template "{{ json .Image }}" :sort nil :format nil)
                              (:name "Id" :width 12 :template "{{ json .ID }}" :sort nil :format nil)
                              (:name "Ports" :width 20 :template "{{ json .Ports }}" :sort nil :format nil)
                              (:name "Command" :width 23 :template "{{ json .Command }}" :sort nil :format nil)
                              (:name "Created" :width 23 :template "{{ json .CreatedAt }}" :sort nil :format (lambda (x) (format-time-string "%F %T" (date-to-time x))))
                              )
   )
  )


;; (use-package kubernetes
;;   ;; https://kubernetes-el.github.io/kubernetes-el/
;;   :ensure t
;;   :defer t
;;   :commands (kubernetes-overview)
;;   :init
;;   ;; https://github.com/kubernetes-el/kubernetes-el/issues/265
;;   ;; Work around: cyclic dependency.
;;   ;; `Debugger entered--Lisp error: (invalid-function kubernetes-utils--save-window-state)`
;;   (defmacro kubernetes-utils--save-window-state (&rest body)
;;     `(let ((pos (point)) (col (current-column)) (window-start-line (window-start)) (inhibit-redisplay t))
;;        (save-excursion ,@body)
;;        (goto-char pos)
;;        (move-to-column col)
;;        (set-window-start (selected-window) window-start-line)))
;; )


;; *****************************************************
;; *****************************************************
;; Shell script (bash)(built-in)(Major mode)
;; *****************************************************
;; *****************************************************
(use-package sh-script
  :ensure t
  :defer t
  :hook (
         (shell-script-mode . lsp)
         (sh-mode . lsp)
         )
  )


(use-package ansible
  ; https://github.com/k1LoW/emacs-ansible
  :ensure t
  :defer t
  :config
  (progn
    (add-hook 'yaml-mode-hook '(lambda () (ansible 1)))
    )

  (use-package ansible-doc
    ; https://github.com/lunaryorn/ansible-doc.el
    :ensure t
  :defer t
    :hook (yaml-mode . ansible-doc-mode)
    )

  (use-package company-ansible
    ; https://github.com/krzysztof-magosa/company-ansible
    :ensure t
  :defer t
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
; TODO: Figure out which package is requiring `json-reformat` ??
;; Debugger entered--Lisp error: (file-missing "Cannot open load file" "No such file or directory" "json-reformat")
;;   require(json-reformat)
;;   byte-code("\300\301!\210\300\302!\210\300\303!\210\300\304!\210\305\306\307\310\311\301%\207" [require js rx json-snatcher json-reformat custom-declare-group json-mode nil "Major mode for editing JSON files." :group] 6)
;;   json-mode()
;;   set-auto-mode-0(json-mode nil)
;;   set-auto-mode--apply-alist((("\\.iss\\'" . iss-mode) ("\\.msc$" . mscgen-mode) ("\\.rcp\\'" . emacs-lisp-mode) (".*mutt.*" . mail-mode) ("\\.plantuml\\'" . plantuml-mode) ("\\.odc\\'" . archive-mode) ("\\.odf\\'" . archive-mode) ("\\.odi\\'" . archive-mode) ("\\.otp\\'" . archive-mode) ("\\.odp\\'" . archive-mode) ("\\.otg\\'" . archive-mode) ("\\.odg\\'" . archive-mode) ("\\.ots\\'" . archive-mode) ("\\.ods\\'" . archive-mode) ("\\.odm\\'" . archive-mode) ("\\.ott\\'" . archive-mode) ("\\.odt\\'" . archive-mode) ("\\.mjs\\'" . js2-mode) ("\\.jsx\\'" . js2-mode) ("\\.js\\'" . js2-mode) ("\\.py\\'" . python-mode) ("\\.restclient\\'" . restclient-mode) ("\\.json\\'" . json-mode) ("\\.yaml\\'" . yaml-mode) ("\\.yml\\'" . yaml-mode) ("\\.\\(e?ya?\\|ra\\)ml\\'" . yaml-mode) ("\\.md\\'" . markdown-mode) ("\\Jenkinsfile*\\'" . groovy-mode) ("\\.groovy\\'" . groovy-mode) ("\\.php\\'" . php-mode) ("\\.\\(?:php[s345]?\\|phtml\\)\\'" . php-mode-maybe) ("\\.\\(?:php\\.inc\\|stub\\)\\'" . php-mode) ("/\\.php_cs\\(?:\\.dist\\)?\\'" . php-mode) ("web.config$" . xml-mode) ("\\.cmake\\'" . cmake-mode) ("CMakeLists\\.txt\\'" . cmake-mode) ("\\.tsv\\'" . tsv-mode) ("\\.[Cc][Ss][Vv]\\'" . csv-mode) ("\\.dockerfile\\'" . dockerfile-mode) ("/Dockerfile\\(?:\\.[^/\\]*\\)?\\'" . dockerfile-mode) ("\\.hrl\\'" . erlang-mode) ("\\.erl\\'" . erlang-mode) ("/ebin/.+\\.app" . erlang-mode) ("\\.yrl" . erlang-mode) ("\\.xrl$" . erlang-mode) ("\\.hrl$" . erlang-mode) ("\\.escript" . erlang-mode) ("\\.app\\.src$" . erlang-mode) ("\\.erl$" . erlang-mode) ("go\\.mod\\'" . go-dot-mod-mode) ...) nil nil)
;;   set-auto-mode()
;;   normal-mode(t)
;;   after-find-file(nil nil)
;;   find-file-noselect-1(#<buffer package.json> "~/work/Apollo/Unlock/unlock_webui/package.json" :nowarn nil "~/work/Apollo/Unlock/unlock_webui/package.json" (27399170 66307))
;;   find-file-noselect("/home/craig/work/Apollo/Unlock/unlock_webui/packag..." :nowarn)
;;   desktop-restore-file-buffer("/home/craig/work/Apollo/Unlock/unlock_webui/packag..." "package.json" nil)
;;   desktop-create-buffer(208 "/home/craig/work/Apollo/Unlock/unlock_webui/packag..." "package.json" fundamental-mode (override-global-mode global-whitespace-mode company-mode projectile-mode which-key-mode dap-mode global-auto-revert-mode) 2720 (nil nil) nil nil ((buffer-display-time 24951 42966 348475 416000) (buffer-file-coding-system . utf-8-unix)) ((mark-ring nil)))
;;   eval-buffer(#<buffer  *load*> nil "/home/craig/.emacs.desktop" nil t)  ; Reading at buffer position 15343
;;   load-with-code-conversion("/home/craig/.emacs.desktop" "/home/craig/.emacs.desktop" t t)
;;   load("/home/craig/.emacs.desktop" t t t)
;;   desktop-read()
;;   #f(compiled-function () #<bytecode -0x19a5bc467a428ba3>)()
;;   run-hooks(after-init-hook delayed-warnings-hook)
;;   command-line()
;;   normal-top-level()

(use-package json-reformat
  :ensure t
  :defer t)
(use-package json-mode
  :ensure t
  :defer t
  :mode ("\\.json\\'" . json-mode)
  :hook (
         (json-mode . my-programming-defaults-config)
         (json-mode . (lambda () (auto-fill-mode -1)))  ;; disables auto fill at column.
         (json-mode . (lambda () (setq js-indent-level 2)))
         )
  )


(use-package realgud
  :ensure t
  :defer t)



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
;;   :defer t
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
;;     :after (company)
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
;;     :defer t
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
  "Hook for my tweaks to `java-mode`."
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
  :defer t
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
  :defer t
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
;; https://github.com/codesuki/add-node-modules-path
;; (use-package add-node-modules-path
;;   :ensure t
;;   :defer t
;;   )
;; https://github.com/jscheid/prettier.el
;; (use-package prettier
;;   :ensure t
;;   :defer t
;;   )
; https://github.com/prettier/prettier-emacs
; Requires global prettier install: `npm install -g prettier`.
;; (use-package prettier-js
;;   :ensure t
;;   :defer t
;;   )
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
  :defer t
  :mode ("\\.js\\'" "\\.jsx\\'" "\\.mjs\\'")
  :hook (
         (js2-mode . lsp)
         ;; (js2-mode . add-node-modules-path)
         ;; (js2-mode . prettier-js-mode)  ; runs prettier on save.
         ;; (js2-mode . prettier-mode)  ; runs prettier on save.
         ; (js2-mode . lsp-treemacs-error-list-mode)
         )
  :config
  (require 'dap-chrome)
  (dap-chrome-setup)
  (require 'dap-node)
  (dap-node-setup)
  (require 'dap-firefox)
  (dap-node-setup)
  (setq
   lsp-eslint-auto-fix-on-save t
   )
  )

;; FIXME: need to update the path to my local node-modules for my project that
;; is in a sub-directory of the repo.
;; (use-package eslint-fix
;;   ; https://github.com/codesuki/eslint-fix
;;   :ensure t
;;   :defer t
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
  :defer t
  :hook (css-mode . lsp)
  )



;; *****************************************************
;; *****************************************************
;; Rust Mode
;; *****************************************************
;; *****************************************************
; https://github.com/rust-lang/rust-mode
(use-package rust-mode
  :ensure t
  :defer t
  ; https://emacs-lsp.github.io/lsp-mode/page/lsp-rust/
  :hook (rust-mode . lsp)
  )


;; *****************************************************
;; *****************************************************
;; PowerShell Mode
;; *****************************************************
;; *****************************************************
; https://github.com/jschaf/powershell.el
(use-package powershell
  :ensure t
  :defer t
  :hook (
         (powershell-mode . my-programming-defaults-config)
         ; (powershell-mode . lsp) ;; No `Expand-Archive` on Arch pwsh, so cannot install `pwsh-ls` automatically.
         )
  )



;; http://cestlaz.github.io/posts/using-emacs-26-gcal/
(use-package calfw
  :ensure t
  :defer t
  :bind
  (
   ("<f8>" . cfw:open-org-calendar)
   )
  :config
  (progn
    (use-package calfw-gcal
      ;; FIXME: 10year old package with deprecated `cl` requirement.
      ;; TODO: replace for: https://github.com/myuhe/org-gcal.el.
      :ensure t
  :defer t)

    (use-package calfw-ical
      :ensure t
  :defer t)

    (use-package calfw-org
      :ensure t
  :defer t)
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




;; *****************************************************
;; *****************************************************
;; Log file highlighting
;; *****************************************************
;; *****************************************************
(use-package log4j-mode
  :ensure t
  :defer t
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
  :ensure t
  :defer t)

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
  :ensure-system-package mu
  :defer t
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
    :defer t
    :init (mu4e-maildirs-extension))

  (use-package mu4e-alert
    ;; Does desktop/modeline notifications.
    ;; https://github.com/iqbalansari/mu4e-alert
    :ensure t
    :defer t
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
;; (cond
;;  ((string-equal system-type "darwin")
;;   (setq browse-url-browser-function  (quote browse-url-default-browser))));;'browse-url-generic
;;         browse-url-generic-program "/Applications/Opera.app/Contents/MacOS/Opera")))

(use-package language-detection
  ; https://github.com/andreasjansson/language-detection.el
  :ensure t
  :defer t
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
;;   :defer t
;;   :bind (("<f5>" . sos))
;;   )


;; FIXME: appears to not exist anymore.
;; ;; ========================
;; ;; iRFC (Download & View RFC's)
;; ;; ========================
;; (use-package irfc
;;   :ensure t
;;   :defer t
;;   :config
;;   (progn
;;     (setq
;;      irfc-directory "~/Downloads/rfcs/"
;;      irfc-assoc-mode t)
;;     )
;;   )






;; ========================
;; Mingus (MPD client)
;; ========================
;; MPD references:
;; * https://github.com/dakrone/eos/blob/master/eos-music.org
;; * https://wiki.archlinux.org/index.php/Music_Player_Daemon
;; * https://wiki.archlinux.org/index.php/Ncmpcpp
(use-package mingus
  :ensure t
  :defer t
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
;;   :defer t
;;   )




;; ========================
;; speed-type
;; ========================
;; https://github.com/parkouss/speed-type/ (was: https://github.com/hagleitn/speed-type).
;; Touch typing practice.
;; Call: `M-x speed-type-text`.
(use-package speed-type
  :ensure t
  :defer t)

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
  :ensure t
  :defer t)

(use-package i3wm-config-mode
  :ensure t
  :defer t)


(use-package iss-mode
  ; https://github.com/rasmus-toftdahl-olesen/iss-mode
  ; InnoSetup mode: https://jrsoftware.org/isinfo.php
  :mode "\\.iss\\'"
  :ensure t
  :defer t
  :init
  (setq iss-compiler-path "C:/Program Files (x86)/Inno Setup 6")
  )


(use-package devdocs
  ; https://github.com/astoff/devdocs.el
  :ensure t
  :defer t
  :hook (
         (c-mode . (lambda () (setq-local devdocs-current-docs '("c"))))
         (c++-mode . (lambda () (setq-local devdocs-current-docs '("cpp" "cmake~3.20"))))
         (python-mode . (lambda () (setq-local devdocs-current-docs '("python~3.9" "django~3.2" "django_rest_framework"))))
         )
  )


;; (make-directory "~/org/jira/" t)
;; (use-package org-jira
;;   :ensure t
;;   :defer t
;;   :config
;;   (setq
;;    jiralib-url "https://eigentech.atlassian.net/"
;;    org-jira-working-dir "~/org/jira/"
;;    )
;;   )


;; ========================
;; Finally; Load extra dot files (if they exist)
;; ========================
(make-directory "~/configs" t)
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

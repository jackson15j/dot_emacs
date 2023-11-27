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





;; *****************************************************
;; *****************************************************
;; Various Tweaks (one-liners and minor package config).
;; *****************************************************
;; *****************************************************
(global-auto-revert-mode t)
(put 'downcase-region 'disabled nil)  ; allow downcase-region without the disabled feature warning.
(put 'upcase-region 'disabled nil)  ; allow upcase-region without the disabled feature warning.
'(flycheck-error-list-column-number ((t (:inherit font-lock-constant-face :background "blue"))))
'(flycheck-warning ((t (:background "DarkBlue" :underline (:color "DarkOrange" :style wave)))))
(setq calendar-week-start-day 1)
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



;; http://pragmaticemacs.com/emacs/dired-human-readable-sizes-and-sort-by-size/
(setq dired-listing-switches "-alh")


;; Allow `Alt+3` on a Mac to be `#`:
;; https://stackoverflow.com/questions/1704119/carbon-emacs-re-enable-hash-key
;; https://stackoverflow.com/questions/3977069/emacs-question-hash-key
(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))
(define-key isearch-mode-map (kbd "M-3") '(lambda () (interactive) (isearch-process-search-char ?\#)))


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
(add-hook 'prog-mode-hook 'my-programming-defaults-config)
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
;; md4rd (Reddit client)
;; ========================
;; https://github.com/ahungry/md4rd
;; FIXME: uncomment once `Debugger entered--Lisp error: (void-variable hierarchy--make)` is fixed.
;; (use-package md4rd
;;   :ensure t
;;   :defer t
;;   )


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

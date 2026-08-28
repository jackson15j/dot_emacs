;;;;;; .emacs --- emacs dot file.  -*- lexical-binding: t; -*-
;;; Commentary:
;; http://www.lunaryorn.com/2015/01/06/my-emacs-configuration-with-use-package.html
;; https://github.com/lunaryorn/.emacs.d/blob/master/init.el

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

;; https://sachachua.com/blog/2026/04/org-mode-tangle-emacs-config-snippets-to-different-files-and-add-boilerplate/
;; NOTE: had to manually delete the tangled `*.el` when there was no changes to regenerate the file.
;;;###autoload
(defun my/sacha-org-babel-post-tangle-insert-lexical-binding ()
  "Insert an Elisp file header into the file just tangled into this buffer."
  (interactive)
  (when-let* ((file (buffer-file-name))
              ((equal (file-name-extension file) "el")))
    (goto-char (point-min))
    (unless (looking-at-p ";;;")
      (insert (format ";;; %s.el --- Tangled from Org -*- lexical-binding: t -*-\n\n"
                      (file-name-base file)))
      (save-buffer))))

(use-package org
  :ensure t
  :hook (org-babel-post-tangle . my/sacha-org-babel-post-tangle-insert-lexical-binding)
  )


;; Run config from an org file.
;; https://himmallright.gitlab.io/post/org-babel-setup/
(set-default-toplevel-value 'lexical-binding t) ; Emacs 31
(org-babel-load-file "~/config.org")

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
(provide '.emacs)
;;; .emacs ends here

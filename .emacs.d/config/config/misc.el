;;;; MISC EMACS CONFIG ;;;;

;; Disable menu bar
(menu-bar-mode -1)

;; Disable backups (files with ~ at the end)
(setq make-backup-files nil)

;; 2-space tabs
(setq-default
 tab-width 2
 standard-indent 2
 indent-tabs-mode nil)
(setq c-basic-indent 2)

(global-set-key (kbd "C-x f") 'magit-find-file)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-c r") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c C-d") 'replace-regexp)
(global-set-key (kbd "C-c C-g") 'rgrep)

;; Delete trailing whitespace when you save a file
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Remote access should go via ssh
(setq tramp-default-method "ssh")

;; Org-mode indents with headings
(add-hook
 'org-mode-hook
 '(lambda ()
    (org-indent-mode t)))

;; Don't put customizations into our beautiful .emacs
(let ((custom-path "~/.emacs-custom.el"))
  (if (not (file-exists-p custom-path))
      (with-temp-buffer (write-file custom-path)))
  (setq custom-file custom-path)
  (load custom-file))

;; Go back to where you were if you close a file
(require 'saveplace)
(setq-default save-place t)

;; Ensure we have ANSI colors in emacs
(require 'ansi-color)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
(add-hook 'eshell-preoutput-filter-functions 'ansi-color-filter-apply)

;; Show column numbers in tool bar
(column-number-mode t)

;; Highlight lines over 80 characters
(require 'whitespace)
(setq whitespace-line-column 80)
(setq whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)

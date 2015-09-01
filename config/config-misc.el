;; Disable menu bar
(menu-bar-mode -1)

;; Move backups to somewhere more convenient
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; Accept "y" at prompts instead of "yes"
(fset 'yes-or-no-p 'y-or-n-p)

;; Always highlight parentheses
(show-paren-mode t)

;; Don't need to display anything on startup
(setq inhibit-startup-message t)

;; 2-space tabs
(setq-default
 tab-width 2
 standard-indent 2
 indent-tabs-mode nil)
(setq c-basic-indent 2)

;; Replacement for other-window (C-x o) that splits the window if it doesn't exist yet
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))

(global-set-key (kbd "C-j") 'other-window-or-split)
(global-set-key (kbd "C-x o") 'other-window-or-split)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c g") 'rgrep)
(global-set-key (kbd "s-<left>") 'windmove-left)
(global-set-key (kbd "s-<right>") 'windmove-right)
(global-set-key (kbd "s-<up>") 'windmove-up)
(global-set-key (kbd "s-<down>") 'windmove-down)

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

;; Highlight lines over 100 characters
(require 'whitespace)
(setq whitespace-line-column 100)
(setq whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)

(add-hook 'css-mode-hook '(lambda() (setq css-indent-offset 2)))

;; Disable scrollbars if they exists
(if (boundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

;; Always fullscreen when we open emacs
(set-frame-parameter nil 'fullscreen 'fullboth)

;; Disable blinking cursor
(blink-cursor-mode 0)

;; Disable audible alarm
(setq ring-bell-function #'ignore)

(provide 'config-misc)

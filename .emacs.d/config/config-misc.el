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

;; Make copy-pasting work correctly on OS X (don't add extra tabs)
(electric-indent-mode 0)

;; Don't prompt about abbrevs on install
(setq save-abbrevs nil)

;; 2-space tabs
(setq-default
 tab-width 2
 standard-indent 2
 indent-tabs-mode nil)
(setq c-basic-indent 2)

;; Replacement for other-window (C-x o) that splits the window if it doesn't exist yet
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p) (split-window-horizontally))
  (other-window 1))

(defun switch-to-eshell ()
  (interactive)
  (let ((name (eshell-curr-name)))
    (unless (get-buffer name)
      (make-shell name))
    (switch-to-buffer name)))

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))

;; Useful keybindings
(global-set-key (kbd "C-x o") 'other-window-or-split)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c g") 'rgrep)
(global-set-key (kbd "C-x m") 'switch-to-eshell)
(global-set-key (kbd "M-DEL") 'backward-delete-word)

;; Delete trailing whitespace when you save a file
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Remote access should go via ssh
(setq tramp-default-method "ssh")
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

;; Org-mode indents with headings
(add-hook 'org-mode-hook 'org-indent-mode)

;; Show line/column numbers in tool bar
(line-number-mode t)
(column-number-mode t)

;; Only on graphical systems
(when (window-system)
  ;; Confirm before exiting
  (setq confirm-kill-emacs 'yes-or-no-p))

;; Disable blinking cursor
(blink-cursor-mode 0)

;; Disable audible alarm
(setq ring-bell-function 'ignore)

;; When typing after selecting a region, delete that region
(delete-selection-mode t)

;; Make some symbols prettier, e.g. "fun" becomes a lambda character
(when (boundp 'global-prettify-symbols-mode)
  (global-prettify-symbols-mode t))

;; OS X specific configuration
(when (eq system-type 'darwin)
  (global-set-key [wheel-up] 'previous-line)
  (global-set-key [wheel-down] 'next-line)
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'super)

  (global-set-key (kbd "s-<left>") 'windmove-left)
  (global-set-key (kbd "s-<right>") 'windmove-right)
  (global-set-key (kbd "s-<up>") 'windmove-up)
  (global-set-key (kbd "s-<down>") 'windmove-down)
  (global-set-key (kbd "s-x") 'kill-region)
  (global-set-key (kbd "s-c") 'kill-ring-save)
  (global-set-key (kbd "s-v") 'yank)
  (global-set-key (kbd "s-s") 'save-buffer)
  (global-set-key (kbd "s-a") 'mark-whole-buffer)
  (global-set-key (kbd "s-z") 'undo)
  (global-set-key (kbd "s-q") 'save-buffers-kill-terminal)
  (global-set-key (kbd "s-r") 'revert-buffer)
  (global-set-key (kbd "s-{") 'persp-prev)
  (global-set-key (kbd "s-}") 'persp-next)
  (global-set-key (kbd "s-t") 'persp-switch)
  (global-set-key (kbd "s-w") 'persp-kill))

;; If you want to highlight the line containing your cursor
(add-hook 'prog-mode-hook 'hl-line-mode)
(setq hl-line-sticky-flag nil)

(provide 'config-misc)

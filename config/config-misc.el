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
  (when (one-window-p) (split-window-horizontally))
  (other-window 1))

(defun switch-to-eshell ()
  (interactive)
  (switch-to-buffer (eshell-curr-name)))

;; Useful keybindings
(global-set-key (kbd "C-x o") 'other-window-or-split)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c g") 'rgrep)
(global-set-key (kbd "C-x m") 'switch-to-eshell)

;; Delete trailing whitespace when you save a file
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Remote access should go via ssh
(setq tramp-default-method "ssh")
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

;; Org-mode indents with headings
(add-hook 'org-mode-hook '(lambda () (org-indent-mode t)))

;; Show line/column numbers in tool bar
(line-number-mode t)
(column-number-mode t)

;; Only on graphical systems
(when (window-system)
  ;; Confirm before exiting
  (setq confirm-kill-emacs 'yes-or-no-p)

  ;; Show line numbers
  (add-hook 'prog-mode-hook '(lambda () (linum-mode t))))

;; Disable a bunch of bars
(when (functionp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (functionp 'set-scroll-bar-mode)
  (set-scroll-bar-mode 'nil))
(when (functionp 'mouse-wheel-mode)
  (mouse-wheel-mode -1))
(when (functionp 'tooltip-mode)
  (tooltip-mode -1))
(when (functionp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (functionp 'blink-cursor-mode)
  (blink-cursor-mode -1))

;; Always fullscreen when we open emacs
(set-frame-parameter nil 'fullscreen 'fullboth)

;; Disable blinking cursor
(blink-cursor-mode 0)

;; Disable audible alarm
(setq ring-bell-function 'ignore)

;; When typing after selecting a region, delete that region
(delete-selection-mode t)

;; Make some symbols prettier, e.g. "fun" becomes a lambda character
(when (boundp 'global-prettify-symbols-mode)
  (global-prettify-symbols-mode +1))

;; OS X specific configuration
(when (eq system-type 'darwin)
  (global-set-key [wheel-up] 'previous-line)
  (global-set-key [wheel-down] 'next-line)
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'super)

  ;; Use Command-<arrow key> to navigate windows
  (global-set-key (kbd "s-<left>") 'windmove-left)
  (global-set-key (kbd "s-<right>") 'windmove-right)
  (global-set-key (kbd "s-<up>") 'windmove-up)
  (global-set-key (kbd "s-<down>") 'windmove-down)

  ;; Use Command-[c|v|x] as normal cut/paste
  (global-set-key (kbd "s-x") 'kill-region)
  (global-set-key (kbd "s-c") 'kill-ring-save)
  (global-set-key (kbd "s-v") 'yank))

(provide 'config-misc)

;;; -*- lexical-binding: t -*-

;; Don't put customizations into our beautiful .emacs
(let ((custom-path "~/.emacs-custom.el"))
  (if (not (file-exists-p custom-path))
      (with-temp-buffer (write-file custom-path)))
  (setq custom-file custom-path)
  (load custom-file))

(add-to-list 'default-frame-alist '(undecorated-round . t))

;; Accept "y" at prompts instead of "yes"
(fset 'yes-or-no-p 'y-or-n-p)

;; Always highlight parentheses
(show-paren-mode t)

;; Don't need to display anything on startup
(setq inhibit-startup-message t)

;; Make copy-pasting work correctly on OS X (don't add extra tabs)
(electric-indent-mode 0)

;; Replacement for other-window (C-x o) that splits the window if it doesn't exist yet
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p) (split-window-horizontally))
  (other-window 1))

;; -or-split creates a new window if none exists
(global-set-key (kbd "C-x o") 'other-window-or-split)

;; by default, prompts to ask which buffer to kill, which is annoying
(global-set-key (kbd "C-x k") 'kill-current-buffer)

(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c g") 'rgrep)

(global-set-key (kbd "s-<left>") 'windmove-left)
(global-set-key (kbd "s-<right>") 'windmove-right)
(global-set-key (kbd "s-<up>") 'windmove-up)
(global-set-key (kbd "s-<down>") 'windmove-down)
(global-set-key (kbd "s-{") 'tab-line-switch-to-prev-tab)
(global-set-key (kbd "s-}") 'tab-line-switch-to-next-tab)
(global-set-key (kbd "C-<tab>") 'tab-bar-switch-to-next-tab)
(global-set-key (kbd "C-S-<tab>") 'tab-bar-switch-to-prev-tab)

(defun switch-to-eshell ()
  (interactive)
  (let ((name (eshell-curr-name)))
    (unless (get-buffer name)
      (make-shell name))
    (switch-to-buffer name)))
(global-set-key (kbd "C-x m") 'switch-to-eshell)

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))
(global-set-key (kbd "M-DEL") 'backward-delete-word)

;; Disable blinking cursor
(blink-cursor-mode 0)

;; Disable audible alarm
(setq ring-bell-function 'ignore)

;; Delete trailing whitespace when you save a file
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Make some symbols prettier, e.g. "fun" becomes a lambda character
(when (boundp 'global-prettify-symbols-mode)
  (global-prettify-symbols-mode t))

;; When typing after selecting a region, delete that region
(delete-selection-mode t)

;; If you want to highlight the line containing your cursor
(add-hook 'prog-mode-hook 'hl-line-mode)
(setq hl-line-sticky-flag nil)

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

;; Use "windowed" fullscreen instead of normal fullscreen
(setq ns-use-native-fullscreen nil)
(toggle-frame-maximized)

;; Confirm before exiting
(setq confirm-kill-emacs 'yes-or-no-p)

;; Make fullscreen by default
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Don't show word wrap indicators
(setf (alist-get 'continuation fringe-indicator-alist) '(nil nil))

;; Allow mouse wheel to scroll buffers
(mouse-wheel-mode 1)
(pixel-scroll-precision-mode 1)

(defvar base-font-size 130 "Base font size.")
(defvar big-font-size 250 "Base font size.")

(set-face-attribute 'default nil :height base-font-size)

(defun toggle-font-size ()
  (interactive)
  (set-face-attribute 'default (selected-frame)
		      :height
		      (if (eq (face-attribute 'default :height) base-font-size)
			  big-font-size
			base-font-size)))


(provide 'config-prelude)

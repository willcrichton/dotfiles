;; PRE-INIT

;; Don't put customizations into our beautiful .emacs
(let ((custom-path "~/.emacs-custom.el"))
  (if (not (file-exists-p custom-path))
      (with-temp-buffer (write-file custom-path)))
  (setq custom-file custom-path)
  (load custom-file))

;; M-x make-shell for multiple shells in one emacs instance
(defun make-shell (name)
  "Create a shell buffer named NAME."
  (interactive "sName: ")
  (setq name (concat "$" name))
  (eshell)
  (rename-buffer name))

;; Ignore dumb AD warnings
(setq ad-redefinition-action 'accept)

;; Set up package management, adding various sources to search
(package-initialize)
(add-to-list
 'package-archives
 '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list
 'package-archives
 '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list
 'package-archives
 '("org" . "http://orgmode.org/elpa/"))
(add-to-list
 'package-archives
 '("melpa" . "http://melpa.org/packages/") t)

;; Get the "use-package" package for simple package configuration
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-verbose t)
(setq use-package-always-ensure t)
(require 'use-package)
(setq load-prefer-newer t)

;; Company-mode for autocompletion
(use-package company
  :diminish company-mode
  :demand t
  :config
  (add-hook 'after-init-hook 'global-company-mode))

;; Helm for file navigation
(use-package helm
  :demand t
  :diminish helm-mode
  :bind (("C-c h" . helm-command-prefix)
	 ("M-x" . helm-M-x)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("C-h SPC" . helm-all-mark-rings)
         ("M-y" . helm-show-kill-ring))
  :config
  (progn
    (require 'helm-eshell)
    (require 'helm-files)
    (require 'helm-grep)
    (use-package helm-ag)
    (use-package helm-ls-git)
    (bind-key "<tab>" 'helm-execute-persistent-action helm-map)
    (bind-key "C-i" 'helm-execute-persistent-action helm-map)
    (bind-key "C-z" 'helm-select-action helm-map)
    (setq helm-man-or-woman-function 'man
          helm-google-suggest-use-curl-p t
          helm-quick-update t
          helm-idle-delay 0.01
          helm-input-idle-delay 0.01
          helm-split-window-in-side-p t
          helm-scroll-amount 4)
    (helm-mode)))

;; Paradox for better package viewing (use M-x paradox-list-packages)
(use-package paradox
  :config (setq paradox-execute-asynchronously t))

;; Magit for git integrated to emacs
(use-package magit)

;; Projectile for large project utilities
(use-package projectile
  :diminish projectile-mode
  :config
  (progn
    (projectile-global-mode t)
    (setq projectile-completion-system 'helm)
    (use-package helm-projectile
      :bind ("C-x f" . helm-projectile-find-file)
      :config (helm-projectile-on))))

;; Better text replacement
(use-package visual-regexp :bind (("C-c r" . vr/query-replace)))

;; To see matching delimiters in pretty colors
(use-package rainbow-delimiters :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; Auto-upload gists to Github -- see https://github.com/defunkt/gist.el
(use-package gist)

;; Who doesn't love zenburn?
(use-package zenburn-theme :config (load-theme 'zenburn t))

;; Fancy mode line
(use-package powerline
  :demand t
  :config
  (use-package airline-themes
    :demand
    :config
    (load-theme 'airline-base16-gui-dark t)))

;; Workspace management
(use-package perspective
  :config
  (progn
    (setq eshell-counter 0)
    (add-hook
     'persp-created-hook
     '(lambda()
        (make-shell (concat "*eshell" (number-to-string eshell-counter) "*"))
        (setq eshell-counter (+ eshell-counter 1))))
    (persp-mode t)))

;; Load in environment variables to shells we use
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns))
    (mapc 'exec-path-from-shell-copy-env
          '("LD_LIBRARY_PATH" "DYLD_LIBRARY_PATH" "CAML_LD_LIBRARY_PATH"))
    (exec-path-from-shell-initialize)))

;; Go back to where you were if you close a file
(use-package saveplace :config (setq-default save-place t))

;; Ensure we have ANSI colors in emacs
(use-package ansi-color
  :config
  (progn
    (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
    (add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
    (add-hook 'eshell-preoutput-filter-functions 'ansi-color-filter-apply)))

;; Highlight lines over 100 characters
(use-package whitespace
  :config
  (progn
    (setq whitespace-line-column 100)
    (setq whitespace-style '(face lines-tail))
    (add-hook 'prog-mode-hook 'whitespace-mode)))

;; Language-specific syntax highlighting
(use-package js2-mode     :mode ("\\.js$"     . js2-mode))
(use-package python-mode  :mode ("\\.py$"     . python-mode))
(use-package scss-mode    :mode ("\\.scss$"   . scss-mode))
(use-package rust-mode    :mode ("\\.rs$"     . rust-mode))
(use-package go-mode      :mode ("\\.go$"     . go-mode))
(use-package lua-mode     :mode ("\\.lua$"    . lua-mode))
(use-package haskell-mode :mode ("\\.hs$"     . haskell-mode))
(use-package coffee-mode  :mode ("\\.coffee$" . coffee-mode))
(use-package tuareg       :mode ("\\.mli?$"   . tuareg-mode)
  :config (use-package ocp-indent))


;; Language-specific extensions
(use-package merlin
  :config
  (progn
    (setq opam-share
          (substring (shell-command-to-string "opam config var share 2> /dev/null") 0 -1))
    (push (concat opam-share "/emacs/site-lisp") load-path)
    (add-hook 'tuareg-mode-hook 'merlin-mode)
    (add-to-list 'company-backends 'merlin-company-backend)))

;; Typing for JS
(use-package tern
  :config
  (progn
    (setq tern-command '("cmd" "/c" "tern"))
    (add-hook 'js2-mode-hook 'tern-mode)
    (use-package company-tern
      :config (add-to-list 'company-backends 'company-tern))))

;; Autocompletion for Python
(use-package company-jedi :config (add-to-list 'company-backends 'company-jedi))

(provide 'config-packages)

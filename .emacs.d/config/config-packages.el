;;; Set up package management, adding various sources to search
(package-initialize nil)
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
  :demand t
  :bind (("C-." . company-complete))
  :diminish company-mode)

;; Helm for file navigation and autocomplete UI
(use-package helm
  :demand t
  :diminish helm-mode
  :bind (("M-x" . helm-M-x)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("C-h SPC" . helm-all-mark-rings)
         ("M-y" . helm-show-kill-ring))
  :config
  (progn
    (bind-key "<tab>" 'helm-execute-persistent-action helm-map)
    (bind-key "C-i" 'helm-execute-persistent-action helm-map)
    (bind-key "C-z" 'helm-select-action helm-map)
    (setq helm-google-suggest-use-curl-p t
          helm-quick-update t
          helm-split-window-in-side-p t
          helm-scroll-amount 4
          helm-buffers-fuzzy-matching t
          helm-recentf-fuzzy-match t
          helm-M-x-fuzzy-match t)
    (helm-mode)))

;; Search through uses of a phrase in the same file
;; M-x helm-swoop
(use-package helm-swoop)

;; Projectile for large project utilities
(use-package projectile
  :demand t
  :diminish projectile-mode
  :config
  (progn
    (projectile-global-mode t)
    (setq projectile-completion-system 'helm)
    (setq projectile-enable-caching t)))

(use-package helm-projectile
  :bind (("C-x f" . helm-projectile-find-file)
         ("C-x g" . helm-projectile-grep))
  :config (helm-projectile-on))

;; Paradox for better package viewing (use M-x paradox-list-packages)
(use-package paradox :config (setq paradox-execute-asynchronously t))

;; Better text replacement
(use-package visual-regexp
  :bind (("M-%" . vr/query-replace))
  :config
  (use-package visual-regexp-steroids))

;; Magit for git integrated to emacs
(use-package magit :bind (("C-c C-g" . magit-status)))

;; Auto-upload gists to Github -- see https://github.com/defunkt/gist.el
(use-package gist)

;; For fun looking delimeters
(use-package rainbow-delimiters
  :demand t
  :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; Load in environment variables to shells we use
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (mapc 'exec-path-from-shell-copy-env
          '("LD_LIBRARY_PATH" "DYLD_LIBRARY_PATH" "CAML_LD_LIBRARY_PATH"))
    (exec-path-from-shell-initialize)))

;; Go back to where you were if you close a file
(use-package saveplace :config (setq-default save-place t))

;; Highlight lines over N characters
(use-package whitespace
  :diminish whitespace-mode
  :config
  (progn
    (setq whitespace-line-column 120)
    (setq whitespace-style '(face lines-tail))
    (add-hook 'prog-mode-hook 'whitespace-mode)))

;; Language-specific syntax highlighting
(use-package scss-mode    :mode ("\\.scss$"   . scss-mode))
(use-package rust-mode    :mode ("\\.rs$"     . rust-mode))
(use-package go-mode      :mode ("\\.go$"     . go-mode))
(use-package haskell-mode :mode ("\\.hs$"     . haskell-mode))
(use-package tuareg
  :mode ("\\.mli?$"   . tuareg-mode)
  :bind (("C-c C-t" . tuareg-eval-region)))
(use-package web-mode     :mode ("\\.((html)|(jsx?)|(php))$" . web-mode))
(use-package yaml-mode)
(use-package toml-mode)
(use-package markdown-mode)
(use-package sml-mode
  :mode ("\\.sml$" . sml-mode)
  :config (setq sml-indent-level 2))
(use-package c++-mode
  :ensure nil
  :mode ("\\.h$"   . c++-mode)
  :mode ("\\.inl$" . c++-mode)
  :config
  (progn
    (setq c-default-style "k&r")
    (c-set-offset 'innamespace 0) ; No indent in namespace
    (c-set-offset 'arglist-intro '+)
    (c-set-offset 'arglist-close 0)
    (setq c-basic-offset 2)))
(use-package lua-mode
  :bind (("C-c C-r" . lua-send-region))
  :config (setq lua-indent-level 2))


;; Language-specific extensions

;; Python auto-formatting
(use-package py-yapf
  :config (add-hook 'python-mode-hook 'py-yapf-enable-on-save))

;; Utilities for OCaml
(use-package ocp-indent
  :config (setq ocp-indent-config "JaneStreet"))
(use-package merlin
  :diminish merlin-mode
  :config
  (progn
    (let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
      (when (and opam-share (file-directory-p opam-share))
        ;; Register Merlin
        (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
        (autoload 'merlin-mode "merlin" nil t nil)
        ;; Automatically start it in OCaml buffers
        (add-hook 'tuareg-mode-hook 'merlin-mode t)
        (add-hook 'caml-mode-hook 'merlin-mode t)
        ;; Use opam switch to lookup ocamlmerlin binary
        (setq merlin-command 'opam)))))


(provide 'config-packages)

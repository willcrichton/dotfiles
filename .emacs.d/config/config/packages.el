; Set up package management, adding various sources to search
(package-initialize nil)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

; Get the "use-package" package for simple package configuration
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-verbose t)
(require 'use-package)
(setq load-prefer-newer t)

;;;; PACKAGES ;;;;

;; Paradox for better package viewing (use M-x paradox-list-packages)
(use-package paradox
  :ensure t
  :config
  (progn
    (setq paradox-execute-asynchronously t)
    (setq paradox-automatically-star t)))

;; Company-mode for autocompletion
(use-package company
  :ensure t
  :defer t
  :config (global-company-mode))

;; Helm for file navigation
(use-package helm-config
  :diminish helm-mode
  :demand t
  :ensure helm
  :bind (("C-c h" . helm-command-prefix)
	 ("M-x" . helm-M-x)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("C-h SPC" . helm-all-mark-rings)
         ("M-y" . helm-show-kill-ring))
  :config
  (progn
    (use-package helm-eshell)
    (use-package helm-files)
    (use-package helm-grep)
    (use-package helm-ls-git)
    (use-package helm-descbinds
      :ensure t
      :config (bind-key "d" 'helm-descbinds helm-command-map))
    (bind-key "<tab>" 'helm-execute-persistent-action helm-map)
    ;; make tab work in terminal
    (bind-key "C-i" 'helm-execute-persistent-action helm-map)
    (bind-key "C-z" 'helm-select-action helm-map)
    (setq helm-man-or-woman-function 'woman
          helm-google-suggest-use-curl-p t
          helm-quick-update t
          helm-idle-delay 0.01
          helm-input-idle-delay 0.01
          helm-split-window-in-side-p t
          helm-scroll-amount 4)
    (helm-mode)))
(use-package helm-ag
  :ensure t)

;; Magit for git integrated to emacs
(use-package magit :ensure t)

;; Projectile for large project utilities
(use-package projectile :ensure t
  :init
  (projectile-global-mode t)
  (setq projectile-completion-system 'helm)
  :config
  (use-package helm-projectile
    :ensure t
    :config (helm-projectile-on)))

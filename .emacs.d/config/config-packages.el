;;; -*- lexical-binding: t -*-

;; == FIRST-TIME INSTALL COMMANDS ==
;; (treesit-auto-install-all)
;; (nerd-icons-install-fonts)

(require 'package)

(use-package exec-path-from-shell
  :init
  (when (daemonp)
    (exec-path-from-shell-initialize)))

;; ======= COMPLETION ======

;; MELPA isn't searched by default, so we need to add it to the archive list.
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Fetch the latest package contents from ELPA / MELPA
(unless package-archive-contents
  (package-refresh-contents))

;; :ensure t by default.
;; explicit :ensure nil refers to built-in packages.
(setq use-package-always-ensure t)

;; Framework for a block completion buffer
(use-package vertico
  :init
  (setq vertico-resize t)              ; resize buffer height to match results
  (setq vertico-multiform-categories   ; per-completion-type configuration
	'((file grid reverse indexed (:keymap . vertico-directory-map))
	  (consult-locate buffer)
	  (consult-grep buffer)
	  (minor-mode reverse)
	  (imenu buffer)))
  (vertico-mode)                       ; enable vertico
  (vertico-mouse-mode)                 ; allow mouse selection
  (vertico-multiform-mode)             ; allow per-completion-type config
  (savehist-mode))                     ; persist completion history across sessions

;; Completion context for some built-in types
(use-package marginalia :init (marginalia-mode))

;; Better fuzzy matching for vertico
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil)
  (completion-pcm-leading-wildcard t))

;; Framework for an inline completion buffer
(use-package corfu :init (global-corfu-mode))

(use-package consult
  :bind (("C-x b" . consult-buffer)
	 ("C-x g" . consult-ripgrep)
	 ("C-x l" . consult-locate))
  :config
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (setq consult-buffer-list-function #'tabspaces--buffer-list)
  (setq consult-narrow-key "<"))

(use-package embark
  :bind (("M-."   . embark-dwim)      ; the "right-click menu"
         ("M-;"   . embark-act)       ; act with the top default action
         ("C-h B" . embark-bindings)) ; discover what's available
  :config
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; ======= WORKSPACES ======

;; Handles LSP integration, configure it to automatically run for any source code.
;; note that eglot integrates w/ project.el to find project roots
(use-package eglot
  :ensure nil
  :hook (prog-mode . eglot-ensure)
  :custom
  (eglot-code-action-indications nil))

(use-package tabspaces
  :custom
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-include-buffers nil)
  (tabspaces-default-tab "Home")
  :init (tabspaces-mode)
  :config
  (setq enable-recursive-minibuffers t)
  (setq tab-bar-show nil)
  (project-known-project-roots))

(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  :bind (("s-b" . dirvish-side)
	 :map dirvish-mode-map
         ("TAB" . dirvish-subtree-toggle))
  :config
  (require 'dirvish-side)
  (require 'dirvish-subtree)
  (setq dirvish-side-window-parameters '((no-delete-other-windows . t)))
  (setq dirvish-use-header-line nil)
  (setq dired-free-space nil)
  (setq dired-listing-switches "-lA")
  (dirvish-side-follow-mode 1)
  :custom
  (dirvish-side-auto-expand t)
  (dirvish-attributes '(nerd-icons file-size vc-state git-msg subtree-state))
  (dirvish-side-attributes '(nerd-icons file-size vc-state subtree-state)))

(when (executable-find "gls")
  (setq insert-directory-program "gls"
        dired-use-ls-dired t))

(use-package consult-eglot
  :after consult
  :bind ("s-t" . consult-eglot-symbols))

;; familiar IDE keybindings
(bind-keys ("M-," . eglot-find-typeDefinition)
	   ("M-/" . xref-find-references)
	   ("M--" . xref-go-back)
	   ("s-p" . project-find-file)
	   ("s-o" . ide-open-workspace))

;; (use-package project
;;   :config
;;   (setq project-switch-commands 'project-find-file))

(use-package magit :bind (("C-c C-g" . magit-status)))

(defun ide--name-default-tab (&rest _)
  "Give the initial, unnamed tab an explicit name.
The first tab of a frame never runs through
`tab-bar-tab-post-open-functions', so without this it stays an
unnamed catch-all that accumulates every startup buffer."
  (unless (cdr (assq 'explicit-name (tab-bar--current-tab)))
    (tab-bar-rename-tab tabspaces-default-tab)))

(add-hook 'server-after-make-frame-hook #'ide--name-default-tab)
(ide--name-default-tab)

(defun ide-workspace-ibuffer ()
  "Like `ibuffer', but scoped to the current tabspace.
`list-buffers'/`ibuffer' walk the global buffer list and know
nothing about tab-local buffer lists."
  (interactive)
  (ibuffer nil "*Workspace Buffers*"
           '((predicate . (tabspaces--local-buffer-p (current-buffer))))))

(global-set-key (kbd "C-x C-b") #'ide-workspace-ibuffer)

(defun ide-workspace-layout (&optional path)
  (interactive)
  (delete-other-windows)
  (save-selected-window
    (unless (dirvish-side--session-visible-p)
      (dirvish-side path))))

(defun ide--workspace-scratch (dir)
  "Return a scratch buffer local to the workspace rooted at DIR."
  (let ((buf (get-buffer-create
              (format "*scratch: %s*"
                      (file-name-nondirectory (directory-file-name dir))))))
    (with-current-buffer buf
      (setq-local default-directory dir)
      (unless (derived-mode-p 'lisp-interaction-mode)
        (lisp-interaction-mode)))
    buf))

(defvar ide-workspace-lsp-modes
  '(("Cargo.toml"     . rustic-mode)
    ("pyproject.toml" . python-ts-mode)))

(defun ide--start-lsp (dir)
  "Start eglot for the workspace rooted at DIR, if we recognize it."
  (when-let* ((cell (seq-find (lambda (c) (file-exists-p (expand-file-name (car c) dir)))
                              ide-workspace-lsp-modes))
              (mode (cdr cell)))
    (with-temp-buffer
      (setq-local default-directory dir)
      (let ((major-mode mode))                       ; pretend to be a Rust buffer
        (unless (eglot-current-server)
          (apply #'eglot (let ((buffer-file-name (expand-file-name "x" dir)))
                           (eglot--guess-contact))))))))

(defun ide-open-workspace (dir)
  (interactive (list (read-directory-name "Open workspace: ")))
  (let* ((dir (file-name-as-directory (expand-file-name dir)))
         (tabspaces-project-switch-commands #'ignore))
    (tabspaces-open-or-create-project-and-workspace dir)
    ;; `tabspaces-open-or-create-project-and-workspace' let-binds
    ;; `tab-bar-new-tab-choice' to the *global* *scratch* internally, so we
    ;; can't pre-bind it -- swap the buffer out afterwards instead.  Sharing
    ;; one *scratch* means sharing one `default-directory', which makes
    ;; project-find-file/grep/eglot in a fresh tab resolve against whatever
    ;; workspace was opened last.
    ;;
    ;; Use `switch-to-buffer', not `set-window-buffer': only the former
    ;; records the buffer in the frame's `buffer-list', which is what
    ;; tabspaces filters on.
    (let ((scratch (get-buffer "*scratch*")))
      (when (eq (window-buffer (selected-window)) scratch)
        (switch-to-buffer (ide--workspace-scratch dir) nil t)
        ;; ...and evict the global one that `tab-bar-new-tab' just added.
        (set-frame-parameter nil 'buffer-list
                             (delq scratch (frame-parameter nil 'buffer-list)))))
    ;; Only lay out a tab we just created; re-opening an existing workspace
    ;; must not `delete-other-windows' on top of its saved layout.
    (unless (dirvish-side--session-visible-p)
      (ide-workspace-layout dir))
    (ide--start-lsp dir)))

;; ======= LANGUAGES ======

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  (treesit-enabled-modes t)
  (treesit-font-lock-level 4)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all))

(use-package rustic
  :config
  (setq rustic-format-on-save nil)
  (setq rustic-lsp-client 'eglot)
  :custom
  (rustic-cargo-use-last-stored-arguments t))

;; ======= UTILITIES ======

(use-package solarized-theme
  :config (load-theme 'solarized-light t))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (set-face-attribute 'mode-line nil          :strike-through nil :underline nil :overline nil)
  (set-face-attribute 'mode-line-active nil   :strike-through nil :underline nil :overline nil)
  (set-face-attribute 'mode-line-inactive nil :strike-through nil :underline nil :overline nil))

(use-package pcre2el)

(use-package visual-regexp
  :after pcre2el
  :init (setq vr/engine 'pcre2el)
  :bind (("M-%" . vr/query-replace)))

(provide 'config-packages)

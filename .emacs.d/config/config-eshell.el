(setq comint-prompt-read-only t)
(require 'eshell)
(with-eval-after-load "esh-opt"

  ;; Allow me to erase the buffer
  (put 'erase-buffer 'disabled nil)

  ;; Defines "clear" command
  (defun eshell-clear-buffer ()
    "Clear terminal"
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer)))

  (require 'em-term)

  ;; Make these commands show in a separate window
  (append-to-list
   'eshell-visual-commands
   '("vim" "top" "less" "htop" "swipl" "lua" "gdb" "lldb" "aspell"))

  (append-to-list
   'eshell-visual-subcommands
   '(("git" "log" "diff" "show")))

  ;; Various convenient aliases
  (require 'em-alias)
  (append-to-list
   'eshell-command-aliases-list
   '(("gst" "magit-status")
     ("emacs" "find-file $1")
     ("e" "find-file $1")
     ("clear" "eshell-clear-buffer")
     ("ssh" "essh $1")))

  (defun remove-remote-prefix (path)
    (if (epe-remote-p)
        (let ((trimmed (replace-regexp-in-string "^/ssh:.*:" "" path)))
          (if (string-equal trimmed "")
              "/"
            trimmed))
      path))

  ;; TODO: replace (remove-remote-prefix default-directory) with ~
  (defun eshell/pwd-local ()
    (remove-remote-prefix (eshell/pwd)))

  ;; Improves dakrone by removing redundant info in remote prompts
  (defun epe-theme-dakrone-plus ()
    "A eshell-prompt lambda theme with directory shrinking."
    (setq eshell-prompt-regexp "^[^#\nλ]*[#λ] ")
    (let* ((pwd-repl-home (lambda (pwd)
                            (let* ((home (expand-file-name (getenv "HOME")))
                                   (home-len (length home)))
                              (if (and
                                   (>= (length pwd) home-len)
                                   (equal home (substring pwd 0 home-len)))
                                  (concat "~" (substring pwd home-len))
                                pwd))))
           (shrink-paths (lambda (p-lst)
                           (if (> (length p-lst) 3) ;; shrink paths deeper than 3 dirs
                               (concat
                                (mapconcat (lambda (elm)
                                             (if (zerop (length elm)) ""
                                               (substring elm 0 1)))
                                           (butlast p-lst 3)
                                           "/")
                                "/"
                                (mapconcat (lambda (elm) elm)
                                           (last p-lst 3)
                                           "/"))
                             (mapconcat (lambda (elm) elm)
                                        p-lst
                                        "/")))))
      (concat
       (when (epe-remote-p)
         (epe-colorize-with-face
          (concat (epe-remote-user) "@" (epe-remote-host) " ")
          'epe-remote-face))
       (when epe-show-python-info
         (when (fboundp 'epe-venv-p)
           (when (and (epe-venv-p) venv-current-name)
             (epe-colorize-with-face (concat "(" venv-current-name ") ") 'epe-venv-face))))
       (epe-colorize-with-face (funcall
                                shrink-paths
                                (split-string
                                 (funcall pwd-repl-home (eshell/pwd-local)) "/"))
                               'epe-dir-face)
       (when (epe-git-p)
         (concat
          (epe-colorize-with-face ":" 'epe-dir-face)
          (epe-colorize-with-face
           (concat (epe-git-branch)
                   (epe-git-dirty)
                   (epe-git-untracked)
                   (unless (= (epe-git-unpushed-number) 0)
                     (concat ":" (number-to-string (epe-git-unpushed-number)))))
           'epe-git-face)))
       (epe-colorize-with-face " λ" 'epe-symbol-face)
       (epe-colorize-with-face (if (= (user-uid) 0) "#" "") 'epe-sudo-symbol-face)
       " ")))

  ;; Make the prompt look pretty
  (use-package eshell-prompt-extras
    :demand t
    :config
    (progn
      ;; TODO: virtualenvwrapper doesn't seem to work
      (use-package virtualenvwrapper :demand t)
      (venv-initialize-eshell)
      (setq venv-location '("~/.env"))
      (autoload 'epe-theme-dakrone "eshell-prompt-extras")
      (setq eshell-prompt-function 'epe-theme-dakrone-plus)))

  ;; No welcome banner necessary
  (setq eshell-banner-message "")

  (require 'helm-eshell)

  (add-hook
   'eshell-mode-hook
   '(lambda ()
      ;; Reasonable autocompletion
      (setq pcomplete-cycle-completions nil)

      ;; Like C-r in regular shell (but better!)
      (define-key eshell-mode-map (kbd "M-r") 'helm-eshell-history)))

  ;; M-x make-shell for multiple shells in one emacs instance
  (defun make-shell (name)
    "Create a shell buffer named NAME."
    (interactive "sName: ")
    (eshell)
    (rename-buffer name))

  ;; Better cd for remote sessions
  (defun eshell/cdd (path)
    (let* ((prefix (file-remote-p default-directory))
          (home (getenv "HOME")))
      (if prefix
          (if (string-match home path)
              (eshell/cd prefix)
            (if (string-match "^/" path)
                (eshell/cd (concat prefix path))
              (eshell/cd path)))
        (eshell/cd path))))


  (defun eshell/essh (name)
    (eshell/cd (concat "/ssh:" name ":~")))

  (defun eshell-curr-name ()
    (if (fboundp 'persp-curr)
        (concat "*eshell-" (persp-name (persp-curr)) "*")
      "*eshell*"))

  ;; Workspace management
  ;; Note that we put this here and not in config-packages because it depends on
  ;; custom functions here
  (if (window-system)
      (use-package perspective
        :config
        (progn
          (persp-turn-off-modestring)
          (add-hook
           'persp-created-hook
           '(lambda () (make-shell (eshell-curr-name))))
          (persp-mode t)))
    (make-shell (eshell-curr-name))))

(provide 'config-eshell)

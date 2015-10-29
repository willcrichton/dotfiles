(setq comint-prompt-read-only t)
(require 'eshell)
(with-eval-after-load "esh-opt"

  (put 'erase-buffer 'disabled nil)
  (defun eshell-clear-buffer ()
    "Clear terminal"
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer)))

  (require 'em-term)

  (append-to-list
   'eshell-visual-commands
   '("vim" "top" "less"))

  (append-to-list
   'eshell-visual-subcommands
   '('("git" "log" "diff" "show")))

  ;; Various convenient aliases
  (require 'em-alias)
  (append-to-list
   'eshell-command-aliases-list
   '(("gst" "magit-status")
     ("emacs" "find-file $1")
     ("e" "find-file $1")
     ("clear" "eshell-clear-buffer")))

  ;; Make the prompt look pretty
  (use-package eshell-prompt-extras
    :demand t
    :config
    (progn
      ;; TODO: virtualenvwrapper doesn't seem to work
      (use-package virtualenvwrapper :demand t)
      (venv-initialize-eshell)
      (autoload 'epe-theme-dakrone "eshell-prompt-extras")
      (setq eshell-prompt-function 'epe-theme-dakrone)))

  ;; No welcome banner necessary
  (setq eshell-banner-message "")

  (require 'helm-eshell)

  (add-hook
   'eshell-mode-hook
   '(lambda ()
      ;; Use helm's autocomplete. "<tab>" is for GUI, "TAB" is for terminal
      (define-key eshell-mode-map (kbd "<tab>") 'helm-esh-pcomplete)
      (define-key eshell-mode-map (kbd "TAB") 'helm-esh-pcomplete)

      ;; Like C-r in regular shell (but better!)
      (define-key eshell-mode-map (kbd "M-r") 'helm-eshell-history)
      ))

  ;; M-x make-shell for multiple shells in one emacs instance
  (defun make-shell (name)
    "Create a shell buffer named NAME."
    (interactive "sName: ")
    (eshell)
    (rename-buffer name))

  ;; Better cd for remote sessions
  (defun eshell/cdd (&rest args)
    (let ((path (car args))
          (prefix (file-remote-p default-directory))
          (home (getenv "HOME")))
      (if prefix
          (if (string-match home path)
              (eshell/cd (cons prefix (cdr args)))
            (if (string-match "^/" path)
                (eshell/cd (cons (concat prefix "/") (cdr args)))
              (eshell/cd args))
            (eshell/cd args))
        (eshell/cd args))))

  (defun eshell-curr-name ()
    (concat "*eshell-" (persp-name persp-curr) "*"))

  ;; Workspace management
  ;; Note that we put this here and not in config-packages because it depends on
  ;; custom functions here
  (use-package perspective
    :config
    (progn
      (persp-turn-off-modestring)
      (add-hook
       'persp-created-hook
       '(lambda () (make-shell (eshell-curr-name))))
      (persp-mode t))))

(provide 'config-eshell)

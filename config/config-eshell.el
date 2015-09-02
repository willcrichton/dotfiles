;; C-l clears the buffer in eshell
(put 'erase-buffer 'disabled nil)
(defun eshell-clear-buffer ()
  "Clear terminal"
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(add-hook
 'eshell-mode-hook
 '(lambda()
    ;; Commands that heavily modify the terminal like top or vim sometimes needs to be
    ;; explicitly added to a list of "visual commands" that are run outside of eshell.
    (add-to-list
     'eshell-visual-commands
     "vim")
    (add-to-list
     'eshell-visual-subcommands
     '("git" "log" "diff" "show"))

    ;; C-l is like running "clear" in a terminal
    (local-set-key (kbd "C-l") 'eshell-clear-buffer)

    ;; Reasonable autocompletion
    (setq pcomplete-cycle-completions nil)

    ;; Some executables like ocamlbuild use `tput` which needs this in eshell
    (setenv "TERM" "dumb")))

(global-set-key (kbd "C-c C-u") 'eshell)

;; Various convenient aliases
(require 'em-alias)
(setq
 eshell-command-aliases-list
 (append
  eshell-command-aliases-list
  '('("gst" "magit-status")
    '("emacs" "find-file $1")
    '("e" "find-file $1")
    '("clear" "eshell-clear-buffer"))))

;; BEGIN CUSTOM ESHELL COMMAND PROMPT
(setq eshell-history-size 1024)
(setq eshell-prompt-regexp "^[^#$]*[#$] ")

(load "em-hist")           ; So the history vars are defined
(if (boundp 'eshell-save-history-on-exit)
    (setq eshell-save-history-on-exit t)) ; Don't ask, just save
                                        ;(message "eshell-ask-to-save-history is %s" eshell-ask-to-save-history)
(if (boundp 'eshell-ask-to-save-history)
    (setq eshell-ask-to-save-history 'always)) ; For older(?) version
                                        ;(message "eshell-ask-to-save-history is %s" eshell-ask-to-save-history)

(defun eshell/ef (fname-regexp &rest dir) (ef fname-regexp default-directory))

(defun pwd-repl-home (pwd)
  (interactive)
  (let* ((home (expand-file-name (getenv "HOME")))
         (home-len (length home)))
    (if (and
         (>= (length pwd) home-len)
         (equal home (substring pwd 0 home-len)))
        (concat "~" (substring pwd home-len))
      pwd)))

(defun curr-dir-git-branch-string (pwd)
  "Returns current git branch as a string, or the empty string if
PWD is not in a git repo (or the git command is not found)."
  (interactive)
  (when (and (eshell-search-path "git")
             (locate-dominating-file pwd ".git")
             (not (tramp-tramp-file-p pwd)))
    (let ((git-output (shell-command-to-string (concat "cd \"" pwd "\" && git branch | grep '\\*' | sed -e 's/^\\* //'"))))
      (propertize (concat "["
                          (if (> (length git-output) 0)
                              (substring git-output 0 -1)
                            "(no branch)")
                          "]") 'face `(:foreground "DarkOliveGreen3"))
      )))

(setq eshell-prompt-function
      (lambda ()
        (concat
         (propertize ((lambda (p-lst)
                        (if (> (length p-lst) 3)
                            (concat
                             (mapconcat (lambda (elm) (if (zerop (length elm)) ""
                                                        (substring elm 0 1)))
                                        (butlast p-lst 3)
                                        "/")
                             "/"
                             (mapconcat (lambda (elm) elm)
                                        (last p-lst 3)
                                        "/"))
                          (mapconcat (lambda (elm) elm)
                                     p-lst
                                     "/")))
                      (split-string (pwd-repl-home (eshell/pwd)) "/"))
                     'face `(:foreground "NavajoWhite1"))
         (or (curr-dir-git-branch-string (eshell/pwd)))
         (propertize "# " 'face 'default))))

(setq eshell-highlight-prompt nil)
;; END CUSTOM ESHELL PROMPT ;;

(provide 'config-eshell)

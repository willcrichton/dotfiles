;;; -*- lexical-binding: t -*-

(use-package eca
  :vc (:url "https://github.com/editor-code-assistant/eca-emacs" :rev :newest)
  :after tabspaces
  :preface
  (defun eca-chat-configure ()
    (face-remap-add-relative 'header-line :box nil :underline nil)
    (setq-local scroll-conservatively 101
                scroll-margin 0
                scroll-step 1
                auto-window-vscroll nil))
  (defun my/eca-extra-space-before-user (session buffer role content roots &rest _)
    "Insert a blank line before each user message in the transcript."
    (when (and (string= role "user")
               (string= (plist-get content :type) "text"))
      (with-current-buffer buffer
	(let ((inhibit-read-only t))
          (save-excursion
            (goto-char (1- (eca-chat--prompt-area-start-point)))
            (insert "\n"))))))
  ;; Shrink all completed tool calls except fileChange (diffs), which stay open
  ;; so the inline diff is visible without a click.
  ;;
  ;; `eca-chat-shrink-called-tools' is set to nil so ECA doesn't auto-shrink
  ;; everything.  This advice fires after every toolCalled render and shrinks
  ;; only the non-diff types (shellCommand, jsonOutputs, MCP generics, etc.).
  (defun my/eca-shrink-non-diff-tools (_session _buffer _role content _roots &rest _)
    "After a toolCalled event, shrink non-fileChange tool blocks."
    (when (string= (plist-get content :type) "toolCalled")
      (let* ((id (plist-get content :id))
             (details (plist-get content :details))
             (type (plist-get details :type)))
        (unless (string= type "fileChange")
          (when id
            (eca-chat--expandable-content-toggle id t t))))))
  :hook (eca-chat-mode-hook . eca-chat-configure)
  :bind (:map eca-chat-mode-map
	      ("C-c C-s" . eca-chat-stop-prompt)
	      ("C-c C-u" . my/eca-anthropic-usage))
  :config
  (advice-add 'eca-chat--render-content :before #'my/eca-extra-space-before-user)
  (advice-add 'eca-chat--render-content :after #'my/eca-shrink-non-diff-tools)
  :custom
  (eca-chat-shrink-called-tools nil)
  (eca-chat-trust-enable t)
  ;; Don't merge same-repo git worktrees into one shared session.
  (eca-worktree-mode 'isolated))

;;; Claude/Anthropic subscription usage (Max/Pro) --------------------------------
;; Queries Anthropic's undocumented OAuth usage endpoint (the same data behind
;; Claude Code's /usage) using the OAuth access token ECA saved after `/login'.
;; Shows the rolling 5-hour and 7-day utilization for your subscription.

(defgroup my/eca-usage nil
  "Query Anthropic subscription (Max/Pro) usage using ECA's OAuth token."
  :group 'eca)

(defcustom my/eca-usage-db-file (expand-file-name "~/.cache/eca/db.transit.json")
  "Path to ECA's credential store (Transit JSON)."
  :type 'file :group 'my/eca-usage)

(defcustom my/eca-usage-endpoint "https://api.anthropic.com/api/oauth/usage"
  "Undocumented Anthropic OAuth usage endpoint (same data as Claude Code's /usage)."
  :type 'string :group 'my/eca-usage)

(defcustom my/eca-usage-beta-header "oauth-2025-04-20"
  "Value for the required `anthropic-beta' header.
If Anthropic bumps this string you'll get 401s until you update it."
  :type 'string :group 'my/eca-usage)

(defcustom my/eca-usage-user-agent "claude-code/1.0.100"
  "User-Agent sent to the usage endpoint.
Anthropic aggressively rate-limits (429) requests whose UA is not
the string `claude-code/...', so keep the prefix."
  :type 'string :group 'my/eca-usage)

(declare-function iso8601-parse "iso8601")

(defun my/eca-usage--anthropic-creds ()
  "Extract the Anthropic OAuth creds ECA saved after /login.
Return a plist (:token :refresh-token :expires-at :mode :type)."
  (unless (file-readable-p my/eca-usage-db-file)
    (user-error "ECA credential db not found: %s" my/eca-usage-db-file))
  (let ((s (with-temp-buffer
             (insert-file-contents my/eca-usage-db-file)
             (buffer-string))))
    (let ((start (string-match "\"anthropic\",\\[\"\\^ \"" s)))
      (unless start
        (user-error "No Anthropic auth in ECA db — run /login in ECA (choose anthropic) first"))
      ;; The anthropic map is flat, so the first `]' after START closes it.
      (let* ((end (or (string-match "\\]" s start) (length s)))
             (r (substring s start end))
             (grab (lambda (re) (and (string-match re r) (match-string 1 r)))))
        (list :token         (funcall grab "\"~:api-key\",\"\\([^\"]+\\)\"")
              :refresh-token (funcall grab "\"~:refresh-token\",\"\\([^\"]+\\)\"")
              :mode          (funcall grab "\"~:mode\",\"~:\\([a-z0-9]+\\)\"")
              :type          (funcall grab "\"~:type\",\"~:\\([a-z/]+\\)\"")
              :expires-at    (let ((e (funcall grab "\"~:expires-at\",\\([0-9]+\\)")))
                               (and e (string-to-number e))))))))

(defun my/eca-usage--expired-p (exp)
  "Non-nil if EXP (unix seconds or ms) is already in the past."
  (when exp
    (let ((sec (if (> exp 1e12) (/ exp 1000.0) exp)))
      (< sec (float-time)))))

(defun my/eca-usage--fetch (token)
  "GET the usage endpoint with bearer TOKEN; return parsed JSON as an alist."
  (with-temp-buffer
    (let ((rc (call-process
               "curl" nil t nil
               "-sS" "--max-time" "15"
               "-H" (concat "Authorization: Bearer " token)
               "-H" (concat "anthropic-beta: " my/eca-usage-beta-header)
               "-H" (concat "User-Agent: " my/eca-usage-user-agent)
               "-H" "Content-Type: application/json"
               my/eca-usage-endpoint)))
      (unless (eq rc 0)
        (user-error "curl exited %s: %s" rc (string-trim (buffer-string))))
      (goto-char (point-min))
      (condition-case err
          (json-parse-buffer :object-type 'alist :array-type 'list
                             :null-object nil :false-object nil)
        (error (user-error "Could not parse usage response: %s\n%s"
                           (error-message-string err)
                           (string-trim (buffer-string))))))))

(defun my/eca-usage--bar (pct &optional width)
  "Render a text progress bar for PCT (0-100) of WIDTH chars."
  (let* ((width (or width 22))
         (p (max 0.0 (min 100.0 (or pct 0.0))))
         (n (round (* (/ p 100.0) width))))
    (concat "[" (make-string n ?█) (make-string (- width n) ?░) "]")))

(defun my/eca-usage--reset (iso)
  "Human-readable countdown until ISO 8601 timestamp ISO."
  (when (stringp iso)
    (condition-case nil
        (let* ((tgt (float-time (encode-time (iso8601-parse iso))))
               (d (- tgt (float-time))))
          (if (<= d 0) "now"
            (let* ((s (floor d)) (dd (/ s 86400)) (hh (/ (% s 86400) 3600))
                   (mm (/ (% s 3600) 60)))
              (cond ((> dd 0) (format "%dd %dh" dd hh))
                    ((> hh 0) (format "%dh %dm" hh mm))
                    (t (format "%dm" mm))))))
      (error "?"))))

(defun my/eca-usage--line (label obj)
  "Format one usage window LABEL from alist OBJ, or nil if OBJ is nil."
  (when obj
    (let ((u (alist-get 'utilization obj))
          (r (alist-get 'resets_at obj)))
      (format "  %-14s %s %5.1f%%   resets in %s\n"
              label (my/eca-usage--bar u) (or u 0.0) (my/eca-usage--reset r)))))

;;;###autoload
(defun my/eca-anthropic-usage ()
  "Show your Claude (Anthropic) subscription usage — 5-hour and 7-day limits.
Reads the OAuth token ECA stored after `/login' and queries Anthropic's
undocumented usage endpoint (the same data behind Claude Code's /usage)."
  (interactive)
  (require 'iso8601)
  (let* ((creds (my/eca-usage--anthropic-creds))
         (token (plist-get creds :token)))
    (unless token
      (user-error "No Anthropic OAuth token in ECA db (are you logged in with a subscription?)"))
    (when (my/eca-usage--expired-p (plist-get creds :expires-at))
      (message "Note: ECA's cached token looks expired; send a prompt in ECA to refresh if this fails."))
    (let* ((data (my/eca-usage--fetch token))
           (err (alist-get 'error data)))
      (when err
        (let ((type (alist-get 'type err)) (msg (alist-get 'message err)))
          (user-error "%s"
                      (cond ((equal type "authentication_error")
                             (format "Token rejected/expired — send any message in an ECA chat to refresh, then retry. [%s]" msg))
                            ((equal type "rate_limit_error")
                             (format "Usage API rate-limited (429) — wait and retry. [%s]" msg))
                            (t (format "Usage API error: %s" (or msg type)))))))
      (let ((buf (get-buffer-create "*ECA Anthropic Usage*")))
        (with-current-buffer buf
          (special-mode)
          (setq-local revert-buffer-function
                      (lambda (&rest _) (my/eca-anthropic-usage)))
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (format "Claude subscription usage   (plan: %s)\n"
                            (or (plist-get creds :mode) "?")))
            (insert (format "as of %s\n\n" (format-time-string "%Y-%m-%d %H:%M")))
            (dolist (spec '(("5-hour"       five_hour)
                            ("7-day total"  seven_day)
                            ("7-day Opus"   seven_day_opus)
                            ("7-day Sonnet" seven_day_sonnet)))
              (let ((line (my/eca-usage--line (car spec)
                                              (alist-get (cadr spec) data))))
                (when line (insert line))))
            (let ((extra (alist-get 'extra_usage data)))
              (when (and extra (alist-get 'is_enabled extra))
                (insert (format "\n  extra usage    %s / %s credits\n"
                                (or (alist-get 'used_credits extra) 0)
                                (or (alist-get 'monthly_limit extra) "?")))))
            (insert "\n(g to refresh, q to quit)\n")
            (goto-char (point-min))))
        (display-buffer buf)))))

(provide 'config-agents)

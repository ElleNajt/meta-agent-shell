;;; meta-agent-shell.el --- Supervisory agent for agent-shell sessions -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Elle Najt

;; Author: Elle Najt
;; URL: https://github.com/ElleNajt/meta-agent-shell
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (agent-shell "0.33.1"))
;; Keywords: convenience, tools, ai

;; This file is not part of GNU Emacs.

;;; Commentary:

;; meta-agent-shell provides a supervisory "meta-Claude" agent that monitors
;; all your active agent-shell sessions.  It can inspect outputs, search across
;; sessions, send messages between agents, and manage your fleet of AI agents.
;;
;; Quick start:
;;    (use-package meta-agent-shell
;;      :after agent-shell
;;      :config
;;      (setq meta-agent-shell-heartbeat-file "~/heartbeat.org")
;;      (meta-agent-shell-start)
;;      (meta-agent-shell-heartbeat-start))
;;
;; See README.md for full setup instructions.

;;; Code:

(require 'agent-shell)
(require 'shell-maker)
(require 'cl-lib)

(defgroup meta-agent-shell nil
  "Supervisory agent for agent-shell sessions."
  :group 'agent-shell
  :prefix "meta-agent-shell-")

;;; Configuration

(defcustom meta-agent-shell-heartbeat-file "~/heartbeat.org"
  "Path to org file with standing instructions for meta-agent.
This file is sent periodically to the meta session."
  :type 'string
  :group 'meta-agent-shell)

(defcustom meta-agent-shell-heartbeat-interval 900
  "Interval in seconds between heartbeat messages.
Default is 900 (15 minutes)."
  :type 'integer
  :group 'meta-agent-shell)

(defcustom meta-agent-shell-heartbeat-watch-projects nil
  "List of project paths to include detailed recent output for in heartbeat.
If nil, includes summary for all sessions."
  :type '(repeat string)
  :group 'meta-agent-shell)

(defcustom meta-agent-shell-heartbeat-recent-lines 50
  "Number of recent lines to include from watched project buffers."
  :type 'integer
  :group 'meta-agent-shell)

(defcustom meta-agent-shell-heartbeat-cooldown 300
  "Seconds to wait after user interaction before sending heartbeat.
If you've messaged the meta session within this time, heartbeat is delayed."
  :type 'integer
  :group 'meta-agent-shell)

(defcustom meta-agent-shell-directory "~/.claude-meta/"
  "Directory for the meta-agent session."
  :type 'string
  :group 'meta-agent-shell)

(defcustom meta-agent-shell-start-function #'agent-shell
  "Function to start a new agent-shell session."
  :type 'function
  :group 'meta-agent-shell)

(defcustom meta-agent-shell-start-function-args nil
  "Arguments to pass to `meta-agent-shell-start-function'."
  :type 'list
  :group 'meta-agent-shell)

;;; State

(defvar meta-agent-shell--heartbeat-timer nil
  "Timer for periodic heartbeat messages.")

(defvar meta-agent-shell--buffer nil
  "The dedicated meta-agent buffer.")

(defvar meta-agent-shell--last-user-interaction nil
  "Timestamp of last user message to meta session.
Used to implement cooldown before sending heartbeat.")

;;; Helper functions

(defun meta-agent-shell--get-project-path ()
  "Get project path for current buffer.
Uses projectile if available, otherwise `default-directory'."
  (or (and (fboundp 'projectile-project-root)
           (projectile-project-root))
      default-directory))

(defun meta-agent-shell--active-buffers ()
  "Return list of active agent-shell buffers, excluding meta buffer."
  (cl-remove-if (lambda (buf)
                  (eq buf meta-agent-shell--buffer))
                (agent-shell-buffers)))

(defun meta-agent-shell--find-buffer-by-project (project-name)
  "Find agent-shell buffer for PROJECT-NAME.
Returns the buffer or nil if not found."
  (cl-find-if (lambda (buf)
                (with-current-buffer buf
                  (let* ((project-path (meta-agent-shell--get-project-path))
                         (name (file-name-nondirectory (directory-file-name project-path))))
                    (string-equal-ignore-case name project-name))))
              (meta-agent-shell--active-buffers)))

(defun meta-agent-shell--get-buffer-status (buffer)
  "Get status info for BUFFER as a plist."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let* ((project-path (meta-agent-shell--get-project-path))
             (project-name (file-name-nondirectory (directory-file-name project-path)))
             (busy (and (fboundp 'shell-maker-busy) (shell-maker-busy)))
             (mode-id (and (boundp 'agent-shell--state)
                           (map-nested-elt agent-shell--state '(:session :mode-id)))))
        (list :buffer (buffer-name buffer)
              :project project-name
              :project-path project-path
              :status (if busy "working" "ready")
              :mode (or mode-id "default"))))))

(defun meta-agent-shell--get-buffer-recent-output (buffer n-lines)
  "Get last N-LINES of output from BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-max))
        (forward-line (- n-lines))
        (buffer-substring-no-properties (point) (point-max))))))

(defun meta-agent-shell--format-heartbeat ()
  "Format heartbeat message with session status and user's heartbeat.org."
  (let* ((heartbeat-file (expand-file-name meta-agent-shell-heartbeat-file))
         (user-instructions (when (file-exists-p heartbeat-file)
                              (with-temp-buffer
                                (insert-file-contents heartbeat-file)
                                (buffer-string))))
         (active-sessions (meta-agent-shell--active-buffers))
         (session-infos (mapcar #'meta-agent-shell--get-buffer-status active-sessions))
         (timestamp (format-time-string "%Y-%m-%d %H:%M:%S")))
    (with-temp-buffer
      (insert (format "* Heartbeat %s\n\n" timestamp))
      ;; Session status
      (insert "** Active Sessions\n\n")
      (if (null session-infos)
          (insert "No active sessions.\n\n")
        (dolist (info session-infos)
          (let* ((buffer (get-buffer (plist-get info :buffer)))
                 (recent (meta-agent-shell--get-buffer-recent-output
                          buffer meta-agent-shell-heartbeat-recent-lines)))
            (insert (format "*** %s [%s]\n\n"
                            (plist-get info :project)
                            (plist-get info :status)))
            (when (and recent (> (length recent) 0))
              (insert "#+begin_example\n")
              (insert recent)
              (unless (string-suffix-p "\n" recent)
                (insert "\n"))
              (insert "#+end_example\n\n")))))
      ;; User instructions
      (when user-instructions
        (insert "** Instructions\n\n")
        (insert user-instructions)
        (unless (string-suffix-p "\n" user-instructions)
          (insert "\n")))
      (buffer-string))))

(defun meta-agent-shell--buffer-alive-p ()
  "Return non-nil if the meta buffer is alive."
  (and meta-agent-shell--buffer
       (buffer-live-p meta-agent-shell--buffer)))

(defun meta-agent-shell--cooldown-elapsed-p ()
  "Return non-nil if enough time has passed since last user interaction."
  (or (not meta-agent-shell--last-user-interaction)
      (> (- (float-time) meta-agent-shell--last-user-interaction)
         meta-agent-shell-heartbeat-cooldown)))

(defun meta-agent-shell--record-interaction ()
  "Record that the user just interacted with meta session."
  (setq meta-agent-shell--last-user-interaction (float-time)))

(defun meta-agent-shell--send-heartbeat ()
  "Send heartbeat message to meta session.
Only sends if session is alive and cooldown has elapsed."
  (when (meta-agent-shell--buffer-alive-p)
    (when (meta-agent-shell--cooldown-elapsed-p)
      (let ((heartbeat-content (meta-agent-shell--format-heartbeat)))
        (with-current-buffer meta-agent-shell--buffer
          (shell-maker-submit :input (format "HEARTBEAT:\n\n%s" heartbeat-content)))))))

;;; Interactive commands

;;;###autoload
(defun meta-agent-shell-start ()
  "Start or switch to the meta-agent session.
Only one meta session can be active at a time.
The session mode is determined by your agent-shell configuration
\(consider using directory-based mode settings for safety)."
  (interactive)
  (if (meta-agent-shell--buffer-alive-p)
      ;; Already have a meta session, switch to it
      (pop-to-buffer meta-agent-shell--buffer)
    ;; Start a new meta session
    (let ((default-directory (expand-file-name meta-agent-shell-directory)))
      ;; Ensure directory exists
      (make-directory default-directory t)
      (apply meta-agent-shell-start-function meta-agent-shell-start-function-args)
      ;; Track this as the meta buffer
      (setq meta-agent-shell--buffer (current-buffer))
      (message "Meta-agent session started in %s" default-directory))))

;;;###autoload
(defun meta-agent-shell-heartbeat-start ()
  "Start the heartbeat timer."
  (interactive)
  (meta-agent-shell-heartbeat-stop)
  (setq meta-agent-shell--heartbeat-timer
        (run-with-timer meta-agent-shell-heartbeat-interval
                        meta-agent-shell-heartbeat-interval
                        #'meta-agent-shell--send-heartbeat))
  (message "Meta heartbeat started (every %d seconds)" meta-agent-shell-heartbeat-interval))

;;;###autoload
(defun meta-agent-shell-heartbeat-stop ()
  "Stop the heartbeat timer."
  (interactive)
  (when meta-agent-shell--heartbeat-timer
    (cancel-timer meta-agent-shell--heartbeat-timer)
    (setq meta-agent-shell--heartbeat-timer nil)
    (message "Meta heartbeat stopped")))

;;;###autoload
(defun meta-agent-shell-heartbeat-send-now ()
  "Send a heartbeat immediately (for testing or manual trigger)."
  (interactive)
  (if (meta-agent-shell--buffer-alive-p)
      (let ((meta-agent-shell--last-user-interaction nil)) ; bypass cooldown
        (meta-agent-shell--send-heartbeat)
        (message "Heartbeat sent"))
    (user-error "No meta session active. Use `meta-agent-shell-start`")))

;;; Tools - for meta-agent to call via emacsclient

;;;###autoload
(defun meta-agent-shell-list-sessions ()
  "List active agent-shell sessions with details.
Returns list of plists with :project, :buffer, :status."
  (let ((sessions nil))
    (dolist (buf (meta-agent-shell--active-buffers))
      (let ((info (meta-agent-shell--get-buffer-status buf)))
        (when info
          (push (list :project (plist-get info :project)
                      :buffer (plist-get info :buffer)
                      :status (plist-get info :status))
                sessions))))
    (nreverse sessions)))

;;;###autoload
(defun meta-agent-shell-view-session (buffer-name &optional num-lines)
  "View recent output from session BUFFER-NAME.
Returns last NUM-LINES (default 100) of the buffer content."
  (let ((buffer (get-buffer buffer-name))
        (n (or num-lines 100)))
    (if (and buffer (buffer-live-p buffer))
        (meta-agent-shell--get-buffer-recent-output buffer n)
      nil)))

;;;###autoload
(defun meta-agent-shell-view-project (project-name &optional num-lines)
  "View recent output from session for PROJECT-NAME.
Returns last NUM-LINES (default 100) of the buffer content."
  (let ((target-buffer (meta-agent-shell--find-buffer-by-project project-name))
        (n (or num-lines 100)))
    (when target-buffer
      (meta-agent-shell--get-buffer-recent-output target-buffer n))))

;;;###autoload
(defun meta-agent-shell-close-session (buffer-name)
  "Close/kill the agent-shell session BUFFER-NAME.
Returns t on success, nil if buffer not found."
  (let ((buffer (get-buffer buffer-name)))
    (if (and buffer
             (buffer-live-p buffer)
             (memq buffer (agent-shell-buffers)))
        (progn
          (kill-buffer buffer)
          t)
      nil)))

;;;###autoload
(defun meta-agent-shell-close-project (project-name)
  "Close/kill the agent-shell session for PROJECT-NAME.
Returns t on success, nil if no matching session found."
  (let ((target-buffer (meta-agent-shell--find-buffer-by-project project-name)))
    (when target-buffer
      (kill-buffer target-buffer)
      t)))

;;;###autoload
(defun meta-agent-shell-search-sessions (pattern &optional context-lines)
  "Search all active sessions for PATTERN (regexp).
Returns list of matches with :project, :buffer, :line, :context.
CONTEXT-LINES (default 2) controls lines of context around each match."
  (let ((results nil)
        (ctx (or context-lines 2)))
    (dolist (buf (meta-agent-shell--active-buffers))
      (with-current-buffer buf
        (let* ((project-path (meta-agent-shell--get-project-path))
               (project-name (file-name-nondirectory (directory-file-name project-path))))
          (save-excursion
            (goto-char (point-min))
            (while (re-search-forward pattern nil t)
              (let* ((line-num (line-number-at-pos))
                     (start (save-excursion
                              (forward-line (- ctx))
                              (point)))
                     (end (save-excursion
                            (forward-line (1+ ctx))
                            (point)))
                     (context (buffer-substring-no-properties start end)))
                (push (list :project project-name
                            :buffer (buffer-name buf)
                            :line line-num
                            :context context)
                      results)))))))
    (nreverse results)))

;;;###autoload
(defun meta-agent-shell-search-project (project-name pattern &optional context-lines)
  "Search session for PROJECT-NAME for PATTERN (regexp).
Returns list of matches with :line and :context."
  (let ((target-buffer (meta-agent-shell--find-buffer-by-project project-name))
        (results nil)
        (ctx (or context-lines 2)))
    (when target-buffer
      (with-current-buffer target-buffer
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward pattern nil t)
            (let* ((line-num (line-number-at-pos))
                   (start (save-excursion
                            (forward-line (- ctx))
                            (point)))
                   (end (save-excursion
                          (forward-line (1+ ctx))
                          (point)))
                   (context (buffer-substring-no-properties start end)))
              (push (list :line line-num :context context) results))))))
    (nreverse results)))

;;;###autoload
(defun meta-agent-shell-send-to-session (buffer-name message)
  "Send MESSAGE to the agent-shell session in BUFFER-NAME.
Returns t on success, nil if buffer not found or not an active session."
  (let ((buffer (get-buffer buffer-name)))
    (if (and buffer
             (buffer-live-p buffer)
             (memq buffer (agent-shell-buffers)))
        (with-current-buffer buffer
          (shell-maker-submit :input message)
          t)
      nil)))

;;;###autoload
(defun meta-agent-shell-send-to-project (project-name message)
  "Send MESSAGE to the agent-shell session for PROJECT-NAME.
PROJECT-NAME is matched against the project directory name.
Returns t on success, nil if no matching session found."
  (let ((target-buffer (meta-agent-shell--find-buffer-by-project project-name)))
    (when target-buffer
      (with-current-buffer target-buffer
        (shell-maker-submit :input message)
        t))))

;;;###autoload
(defun meta-agent-shell-start-agent (folder &optional initial-message)
  "Start a new agent-shell session in FOLDER.
If INITIAL-MESSAGE is provided, send it to the agent after starting.
Returns the buffer name of the new session, or nil if folder doesn't exist."
  (let ((dir (expand-file-name folder)))
    (if (file-directory-p dir)
        (let ((default-directory dir))
          (apply meta-agent-shell-start-function meta-agent-shell-start-function-args)
          (when initial-message
            (run-at-time 0.5 nil
                         (lambda (buf msg)
                           (when (buffer-live-p buf)
                             (with-current-buffer buf
                               (shell-maker-submit :input msg))))
                         (current-buffer) initial-message))
          (buffer-name (current-buffer)))
      (message "Directory does not exist: %s" dir)
      nil)))

;;;###autoload
(defun meta-agent-shell-interrupt-session (buffer-name)
  "Interrupt the agent-shell session in BUFFER-NAME.
Returns t on success, nil if buffer not found or not an active session."
  (let ((buffer (get-buffer buffer-name)))
    (if (and buffer
             (buffer-live-p buffer)
             (memq buffer (agent-shell-buffers)))
        (with-current-buffer buffer
          (agent-shell-interrupt t)  ; force=t to skip y-or-n-p prompt
          t)
      nil)))

;;;###autoload
(defun meta-agent-shell-interrupt-project (project-name)
  "Interrupt the agent-shell session for PROJECT-NAME.
PROJECT-NAME is matched against the project directory name.
Returns t on success, nil if no matching session found."
  (let ((target-buffer (meta-agent-shell--find-buffer-by-project project-name)))
    (when target-buffer
      (with-current-buffer target-buffer
        (agent-shell-interrupt t)  ; force=t to skip y-or-n-p prompt
        t))))

(provide 'meta-agent-shell)
;;; meta-agent-shell.el ends here

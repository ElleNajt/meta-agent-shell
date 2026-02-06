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
  "Function to start a new agent-shell session.
Should accept optional BUFFER-NAME as second argument for named agents."
  :type 'function
  :group 'meta-agent-shell)

(defcustom meta-agent-shell-start-function-args nil
  "Arguments to pass to `meta-agent-shell-start-function'.
These are passed as the first argument (e.g., prefix arg)."
  :type 'list
  :group 'meta-agent-shell)

(defcustom meta-agent-shell-log-directory "~/.meta-agent-shell/logs/"
  "Directory for ICC (inter-Claude communication) logs."
  :type 'string
  :group 'meta-agent-shell)

(defcustom meta-agent-shell-restrict-targets nil
  "When non-nil, only allow messaging buffers in `meta-agent-shell-allowed-targets'.
Set to t for sandboxed workflows where dispatcher has limited permissions."
  :type 'boolean
  :group 'meta-agent-shell)

;;; State

(defvar meta-agent-shell--heartbeat-timer nil
  "Timer for periodic heartbeat messages.")

(defvar meta-agent-shell--buffer nil
  "The dedicated meta-agent buffer.")

(defvar meta-agent-shell--last-user-interaction nil
  "Timestamp of last user message to meta session.
Used to implement cooldown before sending heartbeat.")

(defvar meta-agent-shell--dispatchers nil
  "Alist of (project-path . dispatcher-buffer) for active project dispatchers.
Dispatchers route messages to the appropriate agent within a project.")

(defvar meta-agent-shell--allowed-targets nil
  "List of buffer names that agents are allowed to message.
Only checked when `meta-agent-shell-restrict-targets' is non-nil.
Use `meta-agent-shell-allow-target' to add buffers.")

;;; Target Restrictions

(defun meta-agent-shell-allow-target (buffer-name)
  "Add BUFFER-NAME to the list of allowed messaging targets.
Returns t if added, nil if already present."
  (if (member buffer-name meta-agent-shell--allowed-targets)
      nil
    (push buffer-name meta-agent-shell--allowed-targets)
    t))

(defun meta-agent-shell-disallow-target (buffer-name)
  "Remove BUFFER-NAME from the list of allowed messaging targets."
  (setq meta-agent-shell--allowed-targets
        (delete buffer-name meta-agent-shell--allowed-targets)))

(defun meta-agent-shell-list-allowed-targets ()
  "Return list of allowed messaging targets."
  meta-agent-shell--allowed-targets)

(defun meta-agent-shell--target-allowed-p (buffer-name)
  "Return non-nil if BUFFER-NAME is allowed as a messaging target.
Always returns t if `meta-agent-shell-restrict-targets' is nil."
  (or (not meta-agent-shell-restrict-targets)
      (member buffer-name meta-agent-shell--allowed-targets)))

;;; ICC Logging

(defun meta-agent-shell--log-icc (from to message &optional type)
  "Log inter-Claude communication as JSONL.
FROM is the sender, TO is the recipient, MESSAGE is the content.
TYPE is optional, defaults to \"ask\"."
  (let* ((log-dir (expand-file-name meta-agent-shell-log-directory))
         (log-file (expand-file-name
                    (format "%s-icc.jsonl" (format-time-string "%Y-%m-%d"))
                    log-dir))
         (timestamp (format-time-string "%Y-%m-%dT%H:%M:%S"))
         (msg-type (or type "ask"))
         (entry (json-encode
                 `((timestamp . ,timestamp)
                   (type . ,msg-type)
                   (from . ,from)
                   (to . ,to)
                   (message . ,message)))))
    (make-directory log-dir t)
    (with-temp-buffer
      (insert entry "\n")
      (append-to-file (point-min) (point-max) log-file))))

;;; Helper functions

(defun meta-agent-shell--get-ppid (pid)
  "Get parent PID of PID using ps command."
  (let ((output (shell-command-to-string (format "ps -o ppid= -p %d" pid))))
    (when (string-match "\\([0-9]+\\)" output)
      (string-to-number (match-string 1 output)))))

(defun meta-agent-shell--get-ancestor-pids (pid)
  "Get list of ancestor PIDs for PID, walking up the process tree."
  (let ((ancestors nil)
        (current-pid pid)
        (max-depth 10))
    (while (and current-pid (> current-pid 1) (> max-depth 0))
      (push current-pid ancestors)
      (setq current-pid (meta-agent-shell--get-ppid current-pid))
      (cl-decf max-depth))
    (nreverse ancestors)))

(defun meta-agent-shell--find-buffer-by-client-pid (pid)
  "Find agent-shell buffer whose ACP client process is an ancestor of PID.
Returns buffer name or nil if not found."
  (let ((ancestors (meta-agent-shell--get-ancestor-pids pid)))
    (cl-loop for buf in (agent-shell-buffers)
             for client-proc = (with-current-buffer buf
                                 (when (boundp 'agent-shell--state)
                                   (map-nested-elt agent-shell--state '(:client :process))))
             for client-pid = (when (and client-proc (process-live-p client-proc))
                                (process-id client-proc))
             when (and client-pid (member client-pid ancestors))
             return (buffer-name buf))))

;;;###autoload
(defun meta-agent-shell-whoami (pid)
  "Return the buffer name of the agent-shell session that owns process PID.
PID should be the shell's $$ or a descendant process.
Returns buffer name or nil if not found."
  (meta-agent-shell--find-buffer-by-client-pid pid))

(defun meta-agent-shell--get-project-path ()
  "Get project path for current buffer.
Uses projectile if available, otherwise `default-directory'."
  (or (and (fboundp 'projectile-project-root)
           (projectile-project-root))
      default-directory))

(defun meta-agent-shell--dispatcher-buffer-p (buf)
  "Return non-nil if BUF is a dispatcher buffer."
  (cl-some (lambda (entry) (eq (cdr entry) buf))
           meta-agent-shell--dispatchers))

(defun meta-agent-shell--active-buffers ()
  "Return list of active agent-shell buffers, excluding meta and dispatchers."
  (cl-remove-if (lambda (buf)
                  (or (eq buf meta-agent-shell--buffer)
                      (meta-agent-shell--dispatcher-buffer-p buf)))
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
         (dispatcher-infos (meta-agent-shell-list-dispatchers))
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
      ;; Dispatchers
      (when dispatcher-infos
        (insert "** Active Dispatchers\n\n")
        (dolist (info dispatcher-infos)
          (let* ((buffer (get-buffer (plist-get info :buffer)))
                 (busy (and buffer
                            (buffer-live-p buffer)
                            (with-current-buffer buffer
                              (and (fboundp 'shell-maker-busy) (shell-maker-busy))))))
            (insert (format "*** %s [%s]\n"
                            (plist-get info :project)
                            (if busy "working" "ready")))
            (insert (format "Project path: %s\n\n" (plist-get info :project-path))))))
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
(defun meta-agent-shell-send-to-session (buffer-name message &optional from calling-pid)
  "Send MESSAGE to the agent-shell session in BUFFER-NAME.
Prepends the message with sender info. FROM specifies the sender name.
If FROM is nil and CALLING-PID is provided, auto-detects sender from PID.
If both are nil, defaults to \"an agent\".
Returns t on success, nil if buffer not found, not allowed, or not an active session."
  (unless (meta-agent-shell--target-allowed-p buffer-name)
    (error "Target %s not in allowed list (meta-agent-shell-restrict-targets is enabled)" buffer-name))
  (let* ((from-name (or from
                        (when calling-pid
                          (meta-agent-shell-whoami calling-pid))
                        "an agent"))
         (buffer (get-buffer buffer-name))
         (formatted-message (format "Message from %s:\n\n%s" from-name message)))
    (if (and buffer
             (buffer-live-p buffer)
             (memq buffer (agent-shell-buffers)))
        (progn
          (meta-agent-shell--log-icc from-name buffer-name message "send")
          (with-current-buffer buffer
            (shell-maker-submit :input formatted-message))
          t)
      nil)))

;;;###autoload
(defun meta-agent-shell-send-to-project (project-name message from)
  "Send MESSAGE to the agent-shell session for PROJECT-NAME.
PROJECT-NAME is matched against the project directory name.
FROM specifies the sender name (required).
Returns t on success, nil if no matching session found."
  (let ((target-buffer (meta-agent-shell--find-buffer-by-project project-name)))
    (when target-buffer
      (let ((formatted-message (format "Message from %s:\n\n%s" from message)))
        (meta-agent-shell--log-icc from (buffer-name target-buffer) message "send")
        (with-current-buffer target-buffer
          (shell-maker-submit :input formatted-message)
          t)))))

;;;###autoload
(defun meta-agent-shell-ask-project (project-name question &optional from)
  "Ask QUESTION to the agent-shell session for PROJECT-NAME.
The question is wrapped with instructions to send the reply back.
FROM specifies who is asking (buffer name for reply); required.
Returns t on success, nil if not found."
  (let ((target-buffer (meta-agent-shell--find-buffer-by-project project-name)))
    (when (and target-buffer from)
      (let ((target-name (buffer-name target-buffer)))
        (meta-agent-shell--log-icc from target-name question)
        (with-current-buffer target-buffer
          (shell-maker-submit
           :input (format "Question from %s:

%s

Reply with: agent-send \"%s\" \"YOUR_ANSWER\""
                          from question from))
          t)))))

;;;###autoload
(defun meta-agent-shell-ask-session (buffer-name question &optional from)
  "Ask QUESTION to the agent-shell session in BUFFER-NAME.
The question is wrapped with instructions to send the reply back.
FROM specifies who is asking (buffer name for reply); required.
Returns t on success, nil if not found."
  (unless (meta-agent-shell--target-allowed-p buffer-name)
    (error "Target %s not in allowed list (meta-agent-shell-restrict-targets is enabled)" buffer-name))
  (let ((buffer (get-buffer buffer-name)))
    (if (and buffer
             (buffer-live-p buffer)
             (memq buffer (agent-shell-buffers))
             from)
        (progn
          (meta-agent-shell--log-icc from buffer-name question)
          (with-current-buffer buffer
            (shell-maker-submit
             :input (format "Question from %s:

%s

Reply with: agent-send \"%s\" \"YOUR_ANSWER\""
                            from question from)))
          t)
      nil)))

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
(defun meta-agent-shell-start-named-agent (folder name &optional initial-message auto-allow)
  "Start a new named agent-shell session in FOLDER with NAME.
The buffer will be named \"(ProjectName)-NAME\".
If INITIAL-MESSAGE is provided, send it to the agent after starting.
If AUTO-ALLOW is non-nil (or `meta-agent-shell-restrict-targets' is t),
automatically add the new buffer to the allowed targets list.
Returns the buffer name of the new session, or nil if folder doesn't exist.

Note: `meta-agent-shell-start-function' should accept an optional buffer-name
as its second argument for this to work cleanly."
  (let ((dir (expand-file-name folder)))
    (if (file-directory-p dir)
        (let* ((default-directory dir)
               (project-name (file-name-nondirectory (directory-file-name dir)))
               (buffer-name name))
          ;; Pass buffer-name as second arg to start function
          ;; Retry once on failure (workaround for race condition after killing buffers)
          (condition-case err
              (funcall meta-agent-shell-start-function
                       (car meta-agent-shell-start-function-args)
                       buffer-name)
            (error
             (message "First agent start attempt failed (%s), retrying..." err)
             (sleep-for 0.5)
             (funcall meta-agent-shell-start-function
                      (car meta-agent-shell-start-function-args)
                      buffer-name)))
          ;; Auto-register if restrictions are enabled or explicitly requested
          (when (or auto-allow meta-agent-shell-restrict-targets)
            (meta-agent-shell-allow-target buffer-name))
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

;;;###autoload
(defun meta-agent-shell-interrupt-all ()
  "Interrupt ALL agent-shell sessions immediately.
Panic button - stops all agents including meta-agent and dispatchers.
Returns the number of sessions interrupted."
  (interactive)
  (let ((count 0))
    (dolist (buf (agent-shell-buffers))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (agent-shell-interrupt t))
        (cl-incf count)))
    (message "Interrupted %d agent sessions" count)
    count))

;;; Dispatcher functions - project-level message routing

;;;###autoload
(defconst meta-agent-shell--dispatcher-instructions
  "You are a **project dispatcher**. Your job is to route work to agents and coordinate them.

## Key Principles

1. **Route work, don't do it yourself.** Find the right agent and delegate.
2. **Use `agent-ask` when you need a response.** The reply arrives automatically.
3. **Be available for conversation.** The user may want to discuss strategy or priorities.

## Your Tools

List agents in this project:
```bash
emacsclient --eval '(meta-agent-shell-get-project-agents \"%s\")'
```

Spawn a new named agent:
```bash
emacsclient --eval '(meta-agent-shell-start-named-agent \"%s\" \"AgentName\" \"initial task\")'
```

Send a message to an agent:
```bash
agent-send \"BUFFER-NAME\" \"message\"
```

Ask an agent (they'll reply back):
```bash
agent-ask \"BUFFER-NAME\" \"question\"
```

## Workflow

1. Check which agents exist with the list command above
2. Route to existing agent, or spawn a new named agent for the task
3. For status checks, use `agent-ask` to query agents"
  "Instructions sent to dispatchers at startup.
Contains %s placeholders for project-path.")

(defun meta-agent-shell-start-dispatcher (project-path)
  "Start a dispatcher for PROJECT-PATH.
The dispatcher runs in PROJECT-PATH itself (same as other agents).
Returns the dispatcher buffer name, or nil if already exists.
When called interactively, uses the current buffer's project."
  (interactive (list (meta-agent-shell--get-project-path)))
  (let* ((project-path (expand-file-name project-path))
         (project-name (file-name-nondirectory (directory-file-name project-path)))
         (existing (assoc project-path meta-agent-shell--dispatchers)))
    (if (and existing (buffer-live-p (cdr existing)))
        (progn
          (message "Dispatcher for %s already exists" project-name)
          (pop-to-buffer (cdr existing))
          nil)
      ;; Remove stale entry if buffer is dead
      (when existing
        (setq meta-agent-shell--dispatchers
              (assoc-delete-all project-path meta-agent-shell--dispatchers)))
      ;; Create dispatcher in the project directory itself
      (let* ((dispatcher-buffer-name "Dispatcher")
             (default-directory project-path)
             (instructions (format meta-agent-shell--dispatcher-instructions
                                   project-path project-path))
             (buf nil))
        ;; Pass buffer-name as second arg to start function
        ;; Retry once on failure (workaround for race condition after killing buffers)
        (condition-case err
            (funcall meta-agent-shell-start-function
                     (car meta-agent-shell-start-function-args)
                     dispatcher-buffer-name)
          (error
           (message "First dispatcher start attempt failed (%s), retrying..." err)
           (sleep-for 0.5)
           (funcall meta-agent-shell-start-function
                    (car meta-agent-shell-start-function-args)
                    dispatcher-buffer-name)))
        (setq buf (current-buffer))
        ;; Register dispatcher
        (push (cons project-path buf) meta-agent-shell--dispatchers)
        ;; Send instructions after a short delay for initialization
        (run-at-time 2 nil
                     (lambda (buffer msg)
                       (when (buffer-live-p buffer)
                         (with-current-buffer buffer
                           (condition-case err
                               (shell-maker-submit :input msg)
                             (error (message "Failed to send dispatcher instructions: %s" err))))))
                     buf instructions)
        (message "Dispatcher started for %s" project-name)
        (buffer-name buf)))))

;;;###autoload
(defun meta-agent-shell-get-project-agents (project-path)
  "Get all agent sessions working in PROJECT-PATH.
Returns list of buffer names for agents in that project."
  (let ((project-path (expand-file-name project-path))
        (results nil))
    (dolist (buf (meta-agent-shell--active-buffers))
      (with-current-buffer buf
        (let ((buf-project (expand-file-name (meta-agent-shell--get-project-path))))
          (when (string-prefix-p project-path buf-project)
            (push (buffer-name buf) results)))))
    (nreverse results)))

;;;###autoload
(defun meta-agent-shell-list-dispatchers ()
  "List active dispatchers with their project paths.
Returns alist of (project-name . buffer-name) for live dispatchers."
  ;; Clean up dead buffers first
  (setq meta-agent-shell--dispatchers
        (cl-remove-if-not (lambda (entry) (buffer-live-p (cdr entry)))
                          meta-agent-shell--dispatchers))
  (mapcar (lambda (entry)
            (let ((project-name (file-name-nondirectory
                                 (directory-file-name (car entry)))))
              (list :project project-name
                    :project-path (car entry)
                    :buffer (buffer-name (cdr entry)))))
          meta-agent-shell--dispatchers))

;;;###autoload
(defun meta-agent-shell-send-to-dispatcher (project-name message from)
  "Send MESSAGE to the dispatcher for PROJECT-NAME.
The dispatcher will route it to the appropriate agent.
FROM specifies the sender name (required).
Returns t on success, nil if no dispatcher found."
  (let ((entry (cl-find-if
                (lambda (e)
                  (string-equal-ignore-case
                   project-name
                   (file-name-nondirectory (directory-file-name (car e)))))
                meta-agent-shell--dispatchers)))
    (if (and entry (buffer-live-p (cdr entry)))
        (let* ((dispatcher-name (buffer-name (cdr entry)))
               (formatted-message (format "Message from %s:\n\n%s" from message)))
          (meta-agent-shell--log-icc from dispatcher-name message "send")
          (with-current-buffer (cdr entry)
            (shell-maker-submit :input formatted-message)
            t))
      nil)))

;;;###autoload
(defun meta-agent-shell-ask-dispatcher (project-name question &optional from)
  "Ask QUESTION to the dispatcher for PROJECT-NAME.
The dispatcher is instructed to send the reply back.
FROM specifies who is asking (buffer name for reply); required.
Returns t on success, nil if no dispatcher found."
  (let ((entry (cl-find-if
                (lambda (e)
                  (string-equal-ignore-case
                   project-name
                   (file-name-nondirectory (directory-file-name (car e)))))
                meta-agent-shell--dispatchers)))
    (if (and entry (buffer-live-p (cdr entry)) from)
        (let ((dispatcher-name (buffer-name (cdr entry))))
          (meta-agent-shell--log-icc from dispatcher-name question)
          (with-current-buffer (cdr entry)
            (shell-maker-submit
             :input (format "Question from %s:

%s

Reply with: agent-send \"%s\" \"YOUR_ANSWER\""
                            from question from))
            t))
      nil)))

;;;###autoload
(defun meta-agent-shell-close-dispatcher (project-name)
  "Close the dispatcher for PROJECT-NAME.
Returns t on success, nil if no dispatcher found."
  (interactive
   (list (completing-read "Close dispatcher for project: "
                          (mapcar (lambda (e)
                                    (file-name-nondirectory
                                     (directory-file-name (car e))))
                                  meta-agent-shell--dispatchers))))
  (let ((entry (cl-find-if
                (lambda (e)
                  (string-equal-ignore-case
                   project-name
                   (file-name-nondirectory (directory-file-name (car e)))))
                meta-agent-shell--dispatchers)))
    (if entry
        (progn
          (when (buffer-live-p (cdr entry))
            (kill-buffer (cdr entry)))
          (setq meta-agent-shell--dispatchers
                (cl-remove entry meta-agent-shell--dispatchers))
          (message "Dispatcher for %s closed" project-name)
          t)
      (message "No dispatcher found for %s" project-name)
      nil)))

;;;###autoload
(defun meta-agent-shell-view-dispatcher (project-name &optional num-lines)
  "View recent output from the dispatcher for PROJECT-NAME.
Returns last NUM-LINES (default 100) of the buffer content."
  (let ((entry (cl-find-if
                (lambda (e)
                  (string-equal-ignore-case
                   project-name
                   (file-name-nondirectory (directory-file-name (car e)))))
                meta-agent-shell--dispatchers))
        (n (or num-lines 100)))
    (when (and entry (buffer-live-p (cdr entry)))
      (meta-agent-shell--get-buffer-recent-output (cdr entry) n))))

(provide 'meta-agent-shell)
;;; meta-agent-shell.el ends here

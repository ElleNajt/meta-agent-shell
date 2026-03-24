;;; meta-agent-shell.el --- Supervisory agent for agent-shell sessions -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Elle Najt

;; Author: Elle Najt
;; URL: https://github.com/ElleNajt/meta-agent-shell
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (agent-shell "0.46.1"))
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
;; See README.org for full setup instructions.

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

(defcustom meta-agent-shell-start-function #'meta-agent-shell-default-start-function
  "Function to start a new agent-shell session.
Defaults to `meta-agent-shell-default-start-function', a package-owned
wrapper around `agent-shell-start' that preserves meta-agent-shell's
buffer-naming and directory-selection behavior.

Custom overrides should accept (ARG &optional BUFFER-NAME)."
  :type 'function
  :group 'meta-agent-shell)

(defcustom meta-agent-shell-start-function-args nil
  "Arguments to pass to `meta-agent-shell-start-function'.
These are passed as the first argument (e.g., prefix arg)."
  :type 'list
  :group 'meta-agent-shell)

(defcustom meta-agent-shell-before-start-hook nil
  "Hook run before `meta-agent-shell-default-start-function' starts a session.

Hooks can inspect or update the dynamic variables
`meta-agent-shell-start-context', `meta-agent-shell-start-directory',
`meta-agent-shell-start-buffer-name', `meta-agent-shell-start-session-policy',
`meta-agent-shell-start-command-prefix',
`meta-agent-shell-start-path-resolver-function', and
`meta-agent-shell-start-config'."
  :type 'hook
  :group 'meta-agent-shell)

(defcustom meta-agent-shell-after-start-hook nil
  "Hook run after `meta-agent-shell-default-start-function' starts a session.

Runs with the started session buffer current. The same dynamic start variables
remain available for post-start setup."
  :type 'hook
  :group 'meta-agent-shell)

(defcustom meta-agent-shell-session-mode-map
  '((claude-code :safe "default" :aggressive "bypassPermissions")
    (codex :safe "auto" :aggressive "full-access"))
  "Mapping from provider identifier and abstract policy to session mode id.

Each entry is of the form:

  (IDENTIFIER :safe SAFE-MODE-ID :aggressive AGGRESSIVE-MODE-ID)

When a start spec supplies `:session-policy', `meta-agent-shell' translates
that abstract value into the provider-specific mode id using this map.
IDENTIFIER comes from the resolved agent config's `:identifier' entry."
  :type '(repeat (list symbol
                       (const :safe)
                       (choice (const nil) string)
                       (const :aggressive)
                       (choice (const nil) string)))
  :group 'meta-agent-shell)

(defcustom meta-agent-shell-config-file "~/.meta-agent-shell/config.org"
  "Path to config file with context for the meta-agent.
Supports @file references (like heartbeat.org) which are expanded inline.
Contents are included in each heartbeat message, so changes are picked up live."
  :type 'string
  :group 'meta-agent-shell)

(defcustom meta-agent-shell-log-directory "~/.meta-agent-shell/logs/"
  "Directory for ICC (inter-Claude communication) logs."
  :type 'string
  :group 'meta-agent-shell)

(defcustom meta-agent-shell-killed-agents-directory "~/.agent-shell/meta-agent-shell/"
  "Directory for storing killed agent state.
Agent state is stored as JSON files under <dir>/<project>/<agent-name>.json."
  :type 'string
  :group 'meta-agent-shell)

(defcustom meta-agent-shell-before-spawn-hook nil
  "Hook run before spawning a new named agent.
Called before the agent is created, useful for setting up window layout.
The current buffer and `default-directory' are already set to the project."
  :type 'hook
  :group 'meta-agent-shell)

(defcustom meta-agent-shell-after-spawn-hook nil
  "Hook run after spawning a new named agent.
Called with the new agent buffer as the current buffer.
Useful for post-spawn setup that needs the agent buffer."
  :type 'hook
  :group 'meta-agent-shell)

(defcustom meta-agent-shell-restrict-targets nil
  "When t, only allow messaging buffers in `meta-agent-shell-allowed-targets'.
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

(defvar meta-agent-shell--pending-decorator nil
  "Dynamically bound decorator to inject into `agent-shell-start' calls.
Used by `meta-agent-shell-start' and `meta-agent-shell-start-dispatcher'
to pass the outgoing-request-decorator through custom start functions.")

(defvar meta-agent-shell-start-context nil
  "Dynamically bound context for the current package-managed session start.

Intended values include nil, `meta', `dispatcher', and `named-agent'. User
customization code can inspect this from
`meta-agent-shell-before-start-hook' and `meta-agent-shell-after-start-hook'.")

(defvar meta-agent-shell-start-directory nil
  "Dynamically bound startup directory for the current session start.

`meta-agent-shell-before-start-hook' may set this to redirect the start
directory while preserving the package-owned start flow.")

(defvar meta-agent-shell-start-buffer-name nil
  "Dynamically bound buffer name override for the current session start.

When non-nil, `meta-agent-shell-default-start-function' inserts this into the
resolved `agent-shell' config before calling `agent-shell-start'.")

(defvar meta-agent-shell-start-session-policy nil
  "Dynamically bound abstract session policy for the current session start.

Supported values are `safe', `aggressive', or nil. When non-nil, the default
wrapper translates it to a provider-specific session mode id using
`meta-agent-shell-session-mode-map'.")

(defvar meta-agent-shell-start-command-prefix nil
  "Dynamically bound command prefix override for the current session start.")

(defvar meta-agent-shell-start-path-resolver-function nil
  "Dynamically bound path resolver override for the current session start.")

(defvar meta-agent-shell-start-config nil
  "Dynamically bound base `agent-shell' config for the current session start.

`meta-agent-shell-before-start-hook' may replace or adjust this config before
the default wrapper applies buffer-name or session-policy overrides.")

(defun meta-agent-shell--inject-decorator (orig-fn &rest args)
  "Call ORIG-FN with ARGS.
Injects `:outgoing-request-decorator' when `meta-agent-shell--pending-decorator' is non-nil."
  (if meta-agent-shell--pending-decorator
      (apply orig-fn :outgoing-request-decorator meta-agent-shell--pending-decorator args)
    (apply orig-fn args)))

(advice-add 'agent-shell-start :around #'meta-agent-shell--inject-decorator)

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

;;;###autoload
(defun meta-agent-shell-project-path (pid)
  "Return the `default-directory' of the agent-shell session that owns process PID.
PID should be the shell's $$ or a descendant process.
Returns directory path or nil if not found."
  (when-let ((buf-name (meta-agent-shell--find-buffer-by-client-pid pid)))
    (buffer-local-value 'default-directory (get-buffer buf-name))))

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
Prefers the dispatcher for the project if one exists.
Returns the buffer or nil if not found."
  (let* ((project-path
          (cl-loop for (path . buf) in meta-agent-shell--dispatchers
                   when (string-equal-ignore-case
                         (file-name-nondirectory (directory-file-name path))
                         project-name)
                   return path))
         (dispatcher (cdr (assoc project-path meta-agent-shell--dispatchers))))
    (if (and dispatcher (buffer-live-p dispatcher))
        dispatcher
      (cl-find-if (lambda (buf)
                    (with-current-buffer buf
                      (let* ((pp (meta-agent-shell--get-project-path))
                             (name (file-name-nondirectory (directory-file-name pp))))
                        (string-equal-ignore-case name project-name))))
                  (meta-agent-shell--active-buffers)))))

(defvar meta-agent-shell--initial-tasks (make-hash-table :test 'equal)
  "Hash table mapping buffer names to their initial task messages.
Populated when agents are spawned via `meta-agent-shell-start-named-agent'.")

(defun meta-agent-shell--sanitize-filename (name)
  "Sanitize NAME for use as a filename.
Replaces problematic characters with underscores."
  (replace-regexp-in-string "[/\\\\:*?\"<>|]" "_" name))

(defun meta-agent-shell--save-killed-agent (buffer)
  "Save the state of BUFFER to disk before it's killed.
Only saves if BUFFER is an agent-shell buffer (not meta or dispatcher)."
  (when (and (buffer-live-p buffer)
             (memq buffer (agent-shell-buffers))
             (not (eq buffer meta-agent-shell--buffer))
             (not (meta-agent-shell--dispatcher-buffer-p buffer)))
    (with-current-buffer buffer
      (let* ((buf-name (buffer-name buffer))
             (project-path (meta-agent-shell--get-project-path))
             (project-name (file-name-nondirectory (directory-file-name project-path)))
             (busy (and (fboundp 'shell-maker-busy) (shell-maker-busy)))
             (initial-task (gethash buf-name meta-agent-shell--initial-tasks))
             (last-output (let ((str (buffer-substring-no-properties
                                      (max (point-min) (- (point-max) 500))
                                      (point-max))))
                            (string-trim str)))
             (timestamp (format-time-string "%Y-%m-%dT%H:%M:%S"))
             (safe-name (meta-agent-shell--sanitize-filename buf-name))
             (dir (expand-file-name project-name
                                    (expand-file-name meta-agent-shell-killed-agents-directory)))
             (file (expand-file-name (concat safe-name ".json") dir))
             (data `((name . ,buf-name)
                     (project . ,project-name)
                     (project_path . ,project-path)
                     (initial_task . ,initial-task)
                     (last_output . ,last-output)
                     (timestamp . ,timestamp)
                     (was_busy . ,busy))))
        ;; Clean up initial-task tracking
        (remhash buf-name meta-agent-shell--initial-tasks)
        ;; Save to disk
        (make-directory dir t)
        (with-temp-file file
          (insert (json-encode data)))))))

(defun meta-agent-shell--get-buffer-status (buffer)
  "Get status info for BUFFER as a plist."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let* ((project-path (meta-agent-shell--get-project-path))
             (project-name (file-name-nondirectory (directory-file-name project-path)))
             (busy (and (boundp 'shell-maker-config)
                        shell-maker-config
                        (fboundp 'shell-maker-busy)
                        (shell-maker-busy)))
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

(defun meta-agent-shell--expand-file-refs (text base-dir)
  "Expand @file references in TEXT, resolving paths relative to BASE-DIR.
Lines starting with @ followed by a file path are replaced with the
file's contents. Missing files are silently skipped."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (while (not (eobp))
      (if (looking-at "^@\\(.+\\)$")
          (let* ((ref (string-trim (match-string 1)))
                 (path (expand-file-name ref base-dir)))
            (if (file-exists-p path)
                (let ((content (with-temp-buffer
                                 (insert-file-contents path)
                                 (buffer-string))))
                  (delete-region (line-beginning-position) (line-end-position))
                  (insert content)
                  (unless (string-suffix-p "\n" content)
                    (insert "\n")))
              (delete-region (line-beginning-position)
                             (min (1+ (line-end-position)) (point-max)))))
        (forward-line 1)))
    (buffer-string)))

(defun meta-agent-shell--load-config ()
  "Load config from `meta-agent-shell-config-file'.
Expands @file references relative to the config file's directory.
Returns expanded contents as a string, or nil if file doesn't exist."
  (let ((config-file (expand-file-name meta-agent-shell-config-file)))
    (when (file-exists-p config-file)
      (let ((raw (with-temp-buffer
                   (insert-file-contents config-file)
                   (buffer-string))))
        (meta-agent-shell--expand-file-refs
         raw (file-name-directory config-file))))))

(defun meta-agent-shell--meta-instructions ()
  "Load meta-agent instructions from meta-claude.md in the package directory.
Expands @file references relative to the package directory."
  (let* ((lib (or load-file-name buffer-file-name
                  (locate-library "meta-agent-shell.el")))
         (pkg-dir (file-name-directory (file-truename lib)))
         (instructions-file (expand-file-name "meta-claude.md" pkg-dir)))
    (when (file-exists-p instructions-file)
      (let ((raw (with-temp-buffer
                   (insert-file-contents instructions-file)
                   (buffer-string))))
        (meta-agent-shell--expand-file-refs raw pkg-dir)))))

(defun meta-agent-shell--package-directory ()
  "Return the package directory for meta-agent-shell resources."
  (let ((library-file
         (or load-file-name
             (and (fboundp 'symbol-file)
                  (or (symbol-file 'meta-agent-shell--package-directory 'defun)
                      (symbol-file 'meta-agent-shell-setup 'defun)))
             (locate-library "meta-agent-shell.el"))))
    (unless library-file
      (error "Could not determine meta-agent-shell package directory"))
    (file-name-directory (file-truename library-file))))

(defun meta-agent-shell--resolve-start-config ()
  "Resolve the base `agent-shell' config for starting a new session."
  (or (and (fboundp 'agent-shell-get-config)
           (derived-mode-p 'agent-shell-mode)
           (agent-shell-get-config (current-buffer)))
      (and (fboundp 'agent-shell--resolve-preferred-config)
           (agent-shell--resolve-preferred-config))
      (and (fboundp 'agent-shell-select-config)
           (agent-shell-select-config :prompt "Start agent: "))
      (user-error "No agent-shell config available; configure `agent-shell-preferred-agent-config' or start from an existing agent-shell buffer")))

(defun meta-agent-shell-default-session-mode-function (config session-policy)
  "Return a session mode id for CONFIG and SESSION-POLICY, or nil.
When SESSION-POLICY is nil, preserve the provider's configured default mode."
  (and session-policy
       (meta-agent-shell--session-mode-id-for-policy
        (map-elt config :identifier)
        session-policy)))

(defun meta-agent-shell--session-mode-id-for-policy (identifier policy)
  "Return session mode id for IDENTIFIER and POLICY, or nil."
  (when-let ((entry (assq identifier meta-agent-shell-session-mode-map)))
    (plist-get (cdr entry)
               (pcase policy
                 ('safe :safe)
                 ('aggressive :aggressive)
                 (_ nil)))))

(defun meta-agent-shell--call-start-function (&rest args)
  "Call `meta-agent-shell-start-function' with ARGS and return a buffer.
If the configured function does not return a buffer, fall back to the
current buffer after the call."
  (let ((result (apply meta-agent-shell-start-function args)))
    (cond
     ((bufferp result) result)
     ((and (stringp result) (get-buffer result)) (get-buffer result))
     (t (current-buffer)))))

(defun meta-agent-shell-default-start-function (&optional arg buffer-name)
  "Default start function used by `meta-agent-shell'.

ARG supports these call patterns:
- nil: use the current `default-directory'
- \='use-current-dir: package-internal marker for named-agent starts

Optional BUFFER-NAME overrides the config buffer name."
  (interactive "P")
  (let* ((use-current-dir (eq arg 'use-current-dir))
         (meta-agent-shell-start-directory default-directory)
         (meta-agent-shell-start-buffer-name buffer-name)
         (meta-agent-shell-start-session-policy nil)
         (meta-agent-shell-start-command-prefix nil)
         (meta-agent-shell-start-path-resolver-function nil)
         (meta-agent-shell-start-config
          (copy-tree (meta-agent-shell--resolve-start-config))))
    (run-hooks 'meta-agent-shell-before-start-hook)
    (let* ((directory meta-agent-shell-start-directory)
           (command-prefix meta-agent-shell-start-command-prefix)
           (path-resolver meta-agent-shell-start-path-resolver-function)
           (config (copy-tree meta-agent-shell-start-config))
           (session-mode-id (meta-agent-shell-default-session-mode-function
                             config meta-agent-shell-start-session-policy))
           (agent-shell-cwd-function (when use-current-dir
                                       (lambda () directory)))
           (agent-shell-command-prefix
            (or command-prefix agent-shell-command-prefix))
           (agent-shell-path-resolver-function
            (or path-resolver agent-shell-path-resolver-function)))
      (when meta-agent-shell-start-buffer-name
        (setq config (map-insert config :buffer-name
                                 meta-agent-shell-start-buffer-name)))
      (when session-mode-id
        (setq config (map-insert config :default-session-mode-id
                                 (lambda () session-mode-id))))
      (let* ((default-directory directory)
             (buf (agent-shell-start :config config)))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (when use-current-dir
              (setq-local agent-shell-cwd-function (lambda () directory)))
            (when command-prefix
              (setq-local agent-shell-command-prefix command-prefix))
            (when path-resolver
              (setq-local agent-shell-path-resolver-function path-resolver))
            (let ((meta-agent-shell-start-config config))
              (run-hooks 'meta-agent-shell-after-start-hook))))
        buf))))

(defun meta-agent-shell--format-heartbeat ()
  "Format heartbeat message with session status and user's heartbeat.org.
The heartbeat file supports @file references (relative to the file's directory)
which are expanded inline."
  (let* ((heartbeat-file (expand-file-name meta-agent-shell-heartbeat-file))
         (user-instructions (when (file-exists-p heartbeat-file)
                              (let ((raw (with-temp-buffer
                                           (insert-file-contents heartbeat-file)
                                           (buffer-string))))
                                (meta-agent-shell--expand-file-refs
                                 raw (file-name-directory heartbeat-file)))))
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
      ;; Config file context (refreshed each heartbeat)
      (let ((config-content (meta-agent-shell--load-config)))
        (when config-content
          (insert "\n** Context\n\n")
          (insert config-content)
          (unless (string-suffix-p "\n" config-content)
            (insert "\n"))))
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
(defun meta-agent-shell--make-session-meta-decorator (session-meta)
  "Return an outgoing-request-decorator that injects SESSION-META on session/new."
  (lambda (request)
    (when (equal (map-elt request :method) "session/new")
      (map-put! request :params
                (cons (cons '_meta session-meta)
                      (map-elt request :params))))
    request))

(defconst meta-agent-shell--setup-config-template
  "# Meta-agent config - add @file references to include in the system prompt
# Example:
# @/absolute/path/to/priorities.org
# @relative/path/from/here.org
"
  "Template written to `meta-agent-shell-config-file' during setup.")

(defun meta-agent-shell--setup-support-directory ()
  "Return the stable support directory for setup-managed assets."
  (directory-file-name
   (file-name-directory (expand-file-name meta-agent-shell-config-file))))

(defun meta-agent-shell--ensure-support-bin-on-path ()
  "Ensure the setup-managed support bin dir is present in PATH and `exec-path`."
  (when-let* ((bin-dir (expand-file-name "bin" (meta-agent-shell--setup-support-directory)))
              ((file-directory-p bin-dir)))
    (let* ((current-path (parse-colon-path (or (getenv "PATH") "")))
           (path-parts (if (member bin-dir current-path)
                           current-path
                         (cons bin-dir current-path)))
           (clean-exec-path (delete-dups (append exec-path nil))))
      (setenv "PATH" (mapconcat #'identity path-parts path-separator))
      (unless (member bin-dir clean-exec-path)
        (setq exec-path (cons bin-dir clean-exec-path)))
      (setq-default eshell-path-env (getenv "PATH")))))

;; Ensure the support bin dir is present in PATH when the package loads.
(meta-agent-shell--ensure-support-bin-on-path)

(defun meta-agent-shell--copy-setup-file (source target created updated)
  "Copy SOURCE to TARGET, updating CREATED and UPDATED lists.
Returns a list of the form (CREATED UPDATED)."
  (let* ((target-dir (file-name-directory target))
         (source-modes (file-modes source))
         (target-exists (file-exists-p target))
         (target-modes (and target-exists (file-modes target)))
         (target-content (and target-exists
                              (with-temp-buffer
                                (insert-file-contents target)
                                (buffer-string))))
         (source-content (with-temp-buffer
                           (insert-file-contents source)
                           (buffer-string))))
    (unless (file-directory-p target-dir)
      (make-directory target-dir t))
    (cond
     ((not target-exists)
      (copy-file source target t)
      (when source-modes
        (set-file-modes target source-modes))
      (setq created (cons target created)))
     ((or (not (equal source-content target-content))
          (not (equal source-modes target-modes)))
      (copy-file source target t)
      (when source-modes
        (set-file-modes target source-modes))
      (setq updated (cons target updated))))
    (list created updated)))

(defun meta-agent-shell--sync-setup-assets (support-dir created updated)
  "Sync setup-managed assets into SUPPORT-DIR.
CREATED and UPDATED track resulting file changes.
Returns a list of the form (CREATED UPDATED)."
  (let* ((package-dir (meta-agent-shell--package-directory))
         (source-bin-dir (expand-file-name "bin" package-dir))
         (target-bin-dir (expand-file-name "bin" support-dir))
         (source-overview (expand-file-name "agent-overview.md" package-dir))
         (target-overview (expand-file-name "agent-overview.md" support-dir)))
    (unless (file-directory-p target-bin-dir)
      (make-directory target-bin-dir t))
    (dolist (source (directory-files source-bin-dir t "^[^.].*"))
      (when (file-regular-p source)
        (pcase-let ((`(,new-created ,new-updated)
                     (meta-agent-shell--copy-setup-file
                      source
                      (expand-file-name (file-name-nondirectory source) target-bin-dir)
                      created updated)))
          (setq created new-created
                updated new-updated))))
    (pcase-let ((`(,new-created ,new-updated)
                 (meta-agent-shell--copy-setup-file
                  source-overview target-overview created updated)))
      (setq created new-created
            updated new-updated))
    (list created updated)))

(defun meta-agent-shell--format-setup-summary (created updated existing removed support-dir)
  "Format a setup summary from CREATED, UPDATED, EXISTING, REMOVED, and SUPPORT-DIR."
  (let ((agent-overview (expand-file-name "agent-overview.md" support-dir))
        (bin-dir (expand-file-name "bin" support-dir)))
    (with-temp-buffer
      (insert "meta-agent-shell setup complete\n\n")
      (insert "Created:\n")
      (if created
          (dolist (path created)
            (insert (format "- %s\n" path)))
        (insert "- Nothing created\n"))
      (insert "\nUpdated:\n")
      (if updated
          (dolist (path updated)
            (insert (format "- %s\n" path)))
        (insert "- Nothing updated\n"))
      (insert "\nLeft untouched:\n")
      (if existing
          (dolist (path existing)
            (insert (format "- %s\n" path)))
        (insert "- Nothing to report\n"))
      (insert "\nRemoved legacy items:\n")
      (if removed
          (dolist (path removed)
            (insert (format "- %s\n" path)))
        (insert "- No legacy items removed\n"))
      (insert (format "\nmeta-agent-shell will add its support script directory %s/bin to Emacs PATH and exec-path automatically.\n" support-dir))
      (insert "\nManual next steps:\n")
      (insert "1. Add the following line to your agent instructions file:\n")
      (insert "   (for example, ~/.codex/AGENTS.md or ~/.claude/CLAUDE.md)\n")
      (insert (format "   @%s\n\n" agent-overview))
      (insert (format "2. Edit %s to add @file references for persistent meta-agent context.\n"
                      (expand-file-name meta-agent-shell-config-file)))
      (buffer-string))))

(defun meta-agent-shell--display-setup-summary (summary)
  "Display setup SUMMARY in a dedicated results buffer."
  (let ((buf (get-buffer-create "*meta-agent-shell setup*")))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert summary)
      (goto-char (point-min))
      (view-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun meta-agent-shell-setup ()
  "Set up meta-agent-shell support files and directories.
This replaces the filesystem work previously done by `./setup.sh'."
  (interactive)
  (let* ((meta-dir (expand-file-name meta-agent-shell-directory))
         (legacy-claude (expand-file-name "CLAUDE.md" meta-dir))
         (log-dir (expand-file-name meta-agent-shell-log-directory))
         (config-file (expand-file-name meta-agent-shell-config-file))
         (config-dir (file-name-directory config-file))
         (support-dir (meta-agent-shell--setup-support-directory))
         (created nil)
         (updated nil)
         (existing nil)
         (removed nil))
    (if (file-directory-p meta-dir)
        (push meta-dir existing)
      (make-directory meta-dir t)
      (push meta-dir created))
    (if (file-directory-p log-dir)
        (push log-dir existing)
      (make-directory log-dir t)
      (push log-dir created))
    (unless (file-directory-p config-dir)
      (make-directory config-dir t))
    (if (file-exists-p config-file)
        (push config-file existing)
      (with-temp-file config-file
        (insert meta-agent-shell--setup-config-template))
      (push config-file created))
    (pcase-let ((`(,new-created ,new-updated)
                 (meta-agent-shell--sync-setup-assets support-dir created updated)))
      (setq created new-created
            updated new-updated))
    (cond
     ((file-symlink-p legacy-claude)
      (delete-file legacy-claude)
      (push legacy-claude removed))
     ((file-exists-p legacy-claude)
      (push legacy-claude existing)))
    (meta-agent-shell--ensure-support-bin-on-path)
    (meta-agent-shell--display-setup-summary
     (meta-agent-shell--format-setup-summary
      (nreverse created)
      (nreverse updated)
      (nreverse existing)
      (nreverse removed)
      support-dir))))

(defun meta-agent-shell-start ()
  "Start or switch to the meta-agent session.
Only one meta session can be active at a time.
Instructions are injected via outgoing-request-decorator.
Config from `meta-agent-shell-config-file' is included."
  (interactive)
  (if (meta-agent-shell--buffer-alive-p)
      ;; Already have a meta session, switch to it
      (pop-to-buffer meta-agent-shell--buffer)
    ;; Start a new meta session
    (let* ((default-directory (expand-file-name meta-agent-shell-directory))
           (base-instructions (meta-agent-shell--meta-instructions))
           (session-meta `((systemPrompt . ((append . ,base-instructions)))))
           (meta-agent-shell--pending-decorator
            (meta-agent-shell--make-session-meta-decorator session-meta))
           (buf nil))
      ;; Ensure directory exists
      (make-directory default-directory t)
      (let ((meta-agent-shell-start-context 'meta))
        (setq buf
              (apply #'meta-agent-shell--call-start-function
                     meta-agent-shell-start-function-args)))
      ;; Track this as the meta buffer
      (setq meta-agent-shell--buffer buf)
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
(defun meta-agent-shell-view-session (buffer-name &optional num-lines from calling-pid)
  "View recent output from session BUFFER-NAME.
Returns last NUM-LINES (default 100) of the buffer content.
FROM or CALLING-PID identify the requester for logging."
  (let* ((buffer (get-buffer buffer-name))
         (n (or num-lines 100))
         (from-name (or from
                        (when calling-pid
                          (meta-agent-shell-whoami calling-pid))
                        "an agent")))
    (when (and buffer (buffer-live-p buffer))
      (meta-agent-shell--log-icc from-name buffer-name
                                 (format "viewed last %d lines" n) "view")
      (meta-agent-shell--get-buffer-recent-output buffer n))))

;;;###autoload
(defun meta-agent-shell-view-project (project-name &optional num-lines)
  "View recent output from session for PROJECT-NAME.
Returns last NUM-LINES (default 100) of the buffer content."
  (let ((target-buffer (meta-agent-shell--find-buffer-by-project project-name))
        (n (or num-lines 100)))
    (when target-buffer
      (meta-agent-shell--get-buffer-recent-output target-buffer n))))

;;;###autoload
(defun meta-agent-shell-close-session (buffer-name &optional calling-pid)
  "Close/kill the agent-shell session BUFFER-NAME.
CALLING-PID identifies the requester for logging.
Returns t on success, nil if buffer not found."
  (let* ((buffer (get-buffer buffer-name))
         (from-name (if calling-pid
                        (or (meta-agent-shell-whoami calling-pid) "an agent")
                      "an agent")))
    (if (and buffer
             (buffer-live-p buffer)
             (memq buffer (agent-shell-buffers)))
        (progn
          (meta-agent-shell--log-icc from-name buffer-name "closed session" "close")
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
If the target agent is busy, the message is queued for delivery when ready.
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
            (if (shell-maker-busy)
                ;; Agent is busy - queue the message for later delivery
                (agent-shell--enqueue-request :prompt formatted-message)
              ;; Agent is ready - send immediately
              (shell-maker-submit :input formatted-message)))
          t)
      nil)))

;;;###autoload
(defun meta-agent-shell-send-to-project (project-name message from)
  "Send MESSAGE to the agent-shell session for PROJECT-NAME.
PROJECT-NAME is matched against the project directory name.
FROM specifies the sender name (required).
If the target agent is busy, the message is queued for delivery when ready.
Returns t on success, nil if no matching session found."
  (let ((target-buffer (meta-agent-shell--find-buffer-by-project project-name)))
    (when target-buffer
      (let ((formatted-message (format "Message from %s:\n\n%s" from message)))
        (meta-agent-shell--log-icc from (buffer-name target-buffer) message "send")
        (with-current-buffer target-buffer
          (if (shell-maker-busy)
              (agent-shell--enqueue-request :prompt formatted-message)
            (shell-maker-submit :input formatted-message))
          t)))))

;;;###autoload
(defun meta-agent-shell-ask-project (project-name question &optional from)
  "Ask QUESTION to the agent-shell session for PROJECT-NAME.
The question is wrapped with instructions to send the reply back.
FROM specifies who is asking (buffer name for reply); required.
If the target agent is busy, the question is queued for delivery when ready.
Returns t on success, nil if not found."
  (let ((target-buffer (meta-agent-shell--find-buffer-by-project project-name)))
    (when (and target-buffer from)
      (let* ((target-name (buffer-name target-buffer))
             (formatted-question (format "Question from %s:

%s

Reply with: agent-shell-send \"%s\" \"YOUR_ANSWER\""
                                         from question from)))
        (meta-agent-shell--log-icc from target-name question)
        (with-current-buffer target-buffer
          (if (shell-maker-busy)
              (agent-shell--enqueue-request :prompt formatted-question)
            (shell-maker-submit :input formatted-question)))
        t))))

;;;###autoload
(defun meta-agent-shell-ask-session (buffer-name question &optional from)
  "Ask QUESTION to the agent-shell session in BUFFER-NAME.
The question is wrapped with instructions to send the reply back.
FROM specifies who is asking (buffer name for reply); required.
If the target agent is busy, the question is queued for delivery when ready.
Returns t on success, nil if not found."
  (unless (meta-agent-shell--target-allowed-p buffer-name)
    (error "Target %s not in allowed list (meta-agent-shell-restrict-targets is enabled)" buffer-name))
  (let ((buffer (get-buffer buffer-name)))
    (if (and buffer
             (buffer-live-p buffer)
             (memq buffer (agent-shell-buffers))
             from)
        (let ((formatted-question (format "Question from %s:

%s

Reply with: agent-shell-send \"%s\" \"YOUR_ANSWER\""
                                          from question from)))
          (meta-agent-shell--log-icc from buffer-name question)
          (with-current-buffer buffer
            (if (shell-maker-busy)
                (agent-shell--enqueue-request :prompt formatted-question)
              (shell-maker-submit :input formatted-question)))
          t)
      nil)))

;;;###autoload
(defun meta-agent-shell-start-agent (folder &optional initial-message)
  "Start a new agent-shell session in FOLDER.
If INITIAL-MESSAGE is provided, send it to the agent after starting.
Returns the buffer name of the new session, or nil if folder doesn't exist."
  (let ((dir (expand-file-name folder)))
    (if (file-directory-p dir)
        (let ((default-directory dir)
              (buf nil))
          (setq buf
                (apply #'meta-agent-shell--call-start-function
                       meta-agent-shell-start-function-args))
          (when initial-message
            (run-at-time 0.5 nil
                         (lambda (buf msg)
                           (when (buffer-live-p buf)
                             (with-current-buffer buf
                               (shell-maker-submit :input msg))))
                         buf initial-message))
          (buffer-name buf))
      (message "Directory does not exist: %s" dir)
      nil)))

;;;###autoload
(defun meta-agent-shell-start-named-agent (folder name &optional initial-message auto-allow spawner-buffer)
  "Start a new named agent-shell session in FOLDER with NAME.
The buffer will be named \"(ProjectName)-NAME\".
If INITIAL-MESSAGE is provided, send it to the agent after starting.
If AUTO-ALLOW is non-nil (or `meta-agent-shell-restrict-targets' is t),
automatically add the new buffer to the allowed targets list.
If SPAWNER-BUFFER is provided, select its window before running hooks
so that window splits happen relative to the spawner, not the current window.
Returns the buffer name of the new session, or nil if folder doesn't exist.

Note: `meta-agent-shell-start-function' must:
- Accept (ARG &optional BUFFER-NAME), or tolerate an omitted ARG
- Recognize \\='use-current-dir as ARG to use `default-directory'
- Accept BUFFER-NAME as the second argument for naming the buffer"
  (let ((dir (expand-file-name folder)))
    (if (file-directory-p dir)
        (progn
          ;; Select spawner's window if provided, so hooks split relative to spawner
          (when spawner-buffer
            (let* ((buf (get-buffer spawner-buffer))
                   (spawner-win (when buf (get-buffer-window buf t))))
              (if spawner-win
                  ;; Spawner is visible - select its window
                  (progn
                    (select-frame-set-input-focus (window-frame spawner-win))
                    (select-window spawner-win))
                ;; Spawner not visible - find a frame showing same-project agents
                (when buf
                  (let* ((spawner-project (buffer-local-value 'default-directory buf))
                         (target-frame
                          (cl-loop for frame in (frame-list)
                                   when (cl-some
                                         (lambda (win)
                                           (let ((win-buf (window-buffer win)))
                                             (and (memq win-buf (agent-shell-buffers))
                                                  (string-prefix-p
                                                   spawner-project
                                                   (buffer-local-value 'default-directory win-buf)))))
                                         (window-list frame))
                                   return frame)))
                    (when target-frame
                      (select-frame-set-input-focus target-frame)))))))
          ;; Run before-spawn hook FIRST (e.g., for window layout setup)
          ;; This may change selected window/buffer, so run before binding default-directory
          (run-hooks 'meta-agent-shell-before-spawn-hook)
          (let* ((default-directory dir)
                 (buffer-name name)
                 ;; Signal "use current directory" without container mode
                 (start-arg 'use-current-dir)
                 (buf nil)
                 (actual-buffer-name nil))
            ;; Pass buffer-name as second arg to start function
            ;; Workaround: agent-shell may fail if a buffer with this name was
            ;; recently killed (async cleanup race). Retry once after brief delay.
            (condition-case err
                (let ((meta-agent-shell-start-context 'named-agent))
                  (setq buf
                        (meta-agent-shell--call-start-function
                         start-arg
                         buffer-name)))
              (error
               (message "Agent start failed (%s), retrying after cleanup delay..." err)
               (sit-for 0.5)  ; allow pending events to process
               (let ((meta-agent-shell-start-context 'named-agent))
                 (setq buf
                       (meta-agent-shell--call-start-function
                        start-arg
                        buffer-name)))))
            (setq actual-buffer-name (buffer-name buf))
            ;; Auto-register if restrictions are enabled or explicitly requested
            (when (or auto-allow meta-agent-shell-restrict-targets)
              (meta-agent-shell-allow-target actual-buffer-name))
            ;; Run after-spawn hook (e.g., for post-spawn setup)
            (with-current-buffer buf
              (run-hooks 'meta-agent-shell-after-spawn-hook))
            ;; Track initial task for killed agent persistence
            (when initial-message
              (puthash actual-buffer-name initial-message
                       meta-agent-shell--initial-tasks)
              (run-at-time 0.5 nil
                           (lambda (buf msg)
                             (when (buffer-live-p buf)
                               (with-current-buffer buf
                                 (shell-maker-submit :input msg))))
                           buf initial-message))
            actual-buffer-name))
      (message "Directory does not exist: %s" dir)
      nil)))

;;;###autoload
(defun meta-agent-shell-interrupt-session (buffer-name &optional calling-pid)
  "Interrupt the agent-shell session in BUFFER-NAME.
CALLING-PID identifies the requester for logging.
Returns t on success, nil if buffer not found or not an active session."
  (let* ((buffer (get-buffer buffer-name))
         (from-name (if calling-pid
                        (or (meta-agent-shell-whoami calling-pid) "an agent")
                      "an agent")))
    (if (and buffer
             (buffer-live-p buffer)
             (memq buffer (agent-shell-buffers)))
        (progn
          (meta-agent-shell--log-icc from-name buffer-name "interrupted" "interrupt")
          (with-current-buffer buffer
            (agent-shell-interrupt t))  ; force=t to skip y-or-n-p prompt
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
(defun meta-agent-shell-big-red-button ()
  "Interrupt ALL agent-shell sessions immediately.
Big red button - stops all agents including meta-agent and dispatchers.
Returns the number of sessions interrupted."
  (interactive)
  (let ((count 0))
    (dolist (buf (agent-shell-buffers))
      (when (and (buffer-live-p buf)
                 (get-buffer-process buf))
        (with-current-buffer buf
          (agent-shell-interrupt t))
        (cl-incf count)))
    (message "Interrupted %d agent sessions" count)
    count))

;;; Dispatcher functions - project-level message routing

;;;###autoload
(defun meta-agent-shell--read-packaged-markdown (filename)
  "Return the contents of packaged markdown FILENAME.
Expand `@file' references relative to the package directory."
  (let* ((pkg-dir (meta-agent-shell--package-directory))
         (path (expand-file-name filename pkg-dir)))
    (unless (file-exists-p path)
      (error "Missing meta-agent-shell resource: %s" path))
    (let ((raw (with-temp-buffer
                 (insert-file-contents path)
                 (buffer-string))))
      (meta-agent-shell--expand-file-refs raw pkg-dir))))

(defun meta-agent-shell--dispatcher-instructions ()
  "Return dispatcher instructions."
  (meta-agent-shell--read-packaged-markdown "dispatcher-instructions.md"))

(defun meta-agent-shell-start-dispatcher (project-path)
  "Start a dispatcher for PROJECT-PATH.
The dispatcher runs in PROJECT-PATH itself (same as other agents).
Returns the dispatcher buffer name, or nil if already exists.
When called interactively, uses the current buffer's project.

Dispatcher instructions are appended to the system prompt via
outgoing-request-decorator, so they persist across context compaction."
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
             (instructions (meta-agent-shell--dispatcher-instructions))
             (session-meta `((systemPrompt . ((append . ,instructions)))))
             (meta-agent-shell--pending-decorator
              (meta-agent-shell--make-session-meta-decorator session-meta))
             (buf nil))
        ;; Use the configured start function to create the buffer
        (condition-case err
            (let ((meta-agent-shell-start-context 'dispatcher))
              (setq buf
                    (meta-agent-shell--call-start-function
                     (car meta-agent-shell-start-function-args)
                     dispatcher-buffer-name)))
          (error
           (message "Dispatcher start failed (%s), retrying after cleanup delay..." err)
           (sit-for 0.5)
           (let ((meta-agent-shell-start-context 'dispatcher))
             (setq buf
                   (meta-agent-shell--call-start-function
                    (car meta-agent-shell-start-function-args)
                    dispatcher-buffer-name)))))
        ;; Register dispatcher
        (push (cons project-path buf) meta-agent-shell--dispatchers)
        (message "Dispatcher started for %s (instructions in system prompt)" project-name)
        (buffer-name buf)))))

;;;###autoload
(defun meta-agent-shell-jump-to-dispatcher ()
  "Jump to the dispatcher for the current buffer's project.
If no dispatcher exists, offer to create one."
  (interactive)
  (let* ((project-path (expand-file-name (meta-agent-shell--get-project-path)))
         (entry (assoc project-path meta-agent-shell--dispatchers)))
    (if (and entry (buffer-live-p (cdr entry)))
        (pop-to-buffer (cdr entry))
      (if (y-or-n-p (format "No dispatcher for %s. Create one? "
                            (file-name-nondirectory (directory-file-name project-path))))
          (meta-agent-shell-start-dispatcher project-path)
        (message "No dispatcher for this project")))))

;;;###autoload
(defun meta-agent-shell-start-or-dispatcher (arg)
  "Start a dispatcher or normal agent shell for the current project.
If in the meta-agent directory, start the meta-agent instead.
If no dispatcher exists for the project, start one.
If a dispatcher already exists, start a normal agent shell.
Prefix ARG is passed through to the start function."
  (interactive "P")
  (let* ((project-path (expand-file-name (meta-agent-shell--get-project-path)))
         (meta-path (expand-file-name meta-agent-shell-directory)))
    ;; Check if we're in the meta-agent directory
    (if (string-prefix-p meta-path project-path)
        (meta-agent-shell-start)
      ;; Normal project - check for dispatcher
      (let ((entry (assoc project-path meta-agent-shell--dispatchers)))
        (if (and entry (buffer-live-p (cdr entry)))
            ;; Dispatcher exists, start normal agent shell
            (let ((default-directory project-path)
                  (args (if arg
                            (cons arg (cdr meta-agent-shell-start-function-args))
                          meta-agent-shell-start-function-args)))
              (apply #'meta-agent-shell--call-start-function args))
          ;; No dispatcher, start one
          (meta-agent-shell-start-dispatcher project-path))))))

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
(defun meta-agent-shell-view-project-agents (project-path &optional num-chars)
  "View recent output from all agents in PROJECT-PATH.
Returns formatted summary with buffer name and last NUM-CHARS (default 500)
of output for each agent. Also shows recently killed agents.
Useful for dispatchers to get quick status."
  (let* ((project-path (expand-file-name project-path))
         (agents (meta-agent-shell-get-project-agents project-path))
         (killed-agents (meta-agent-shell--load-killed-agents project-path))
         (n (or num-chars 500))
         (results nil))
    (dolist (buf-name agents)
      (let* ((buf (get-buffer buf-name))
             (status (when buf (meta-agent-shell--get-buffer-status buf)))
             (content (when buf
                        (with-current-buffer buf
                          (let ((str (buffer-substring-no-properties
                                      (max (point-min) (- (point-max) n))
                                      (point-max))))
                            (string-trim str))))))
        (when (and status content)
          (push (list :buffer buf-name
                      :status (plist-get status :status)
                      :output content)
                results))))
    (with-temp-buffer
      (insert (format "=== Agents in %s ===\n\n"
                      (file-name-nondirectory (directory-file-name project-path))))
      (if (null results)
          (insert "No active agents.\n\n")
        (dolist (agent (nreverse results))
          (insert (format "--- %s [%s] ---\n"
                          (plist-get agent :buffer)
                          (plist-get agent :status)))
          (insert (plist-get agent :output))
          (insert "\n\n")))
      ;; Show killed agents
      (when killed-agents
        (insert "=== Recently Killed Agents ===\n\n")
        (dolist (agent killed-agents)
          (insert (format "--- %s [killed%s] ---\n"
                          (plist-get agent :name)
                          (if (plist-get agent :was-busy) ", was busy" "")))
          (when (plist-get agent :initial-task)
            (insert (format "Initial task: %s\n" (plist-get agent :initial-task))))
          (insert (format "Killed at: %s\n" (plist-get agent :timestamp)))
          (when (plist-get agent :last-output)
            (insert "Last output:\n")
            (insert (plist-get agent :last-output)))
          (insert "\n\n")))
      (buffer-string))))

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
If the dispatcher is busy, the message is queued for delivery when ready.
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
            (if (shell-maker-busy)
                (agent-shell--enqueue-request :prompt formatted-message)
              (shell-maker-submit :input formatted-message)))
          t)
      nil)))

;;;###autoload
(defun meta-agent-shell-ask-dispatcher (project-name question &optional from)
  "Ask QUESTION to the dispatcher for PROJECT-NAME.
The dispatcher is instructed to send the reply back.
FROM specifies who is asking (buffer name for reply); required.
If the dispatcher is busy, the question is queued for delivery when ready.
Returns t on success, nil if no dispatcher found."
  (let ((entry (cl-find-if
                (lambda (e)
                  (string-equal-ignore-case
                   project-name
                   (file-name-nondirectory (directory-file-name (car e)))))
                meta-agent-shell--dispatchers)))
    (if (and entry (buffer-live-p (cdr entry)) from)
        (let* ((dispatcher-name (buffer-name (cdr entry)))
               (formatted-question (format "Question from %s:

%s

Reply with: agent-shell-send \"%s\" \"YOUR_ANSWER\""
                                           from question from)))
          (meta-agent-shell--log-icc from dispatcher-name question)
          (with-current-buffer (cdr entry)
            (if (shell-maker-busy)
                (agent-shell--enqueue-request :prompt formatted-question)
              (shell-maker-submit :input formatted-question)))
          t)
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

;;; Killed agent tracking

(defun meta-agent-shell--kill-buffer-hook ()
  "Hook function to save agent state when buffer is killed."
  (meta-agent-shell--save-killed-agent (current-buffer)))

(add-hook 'kill-buffer-hook #'meta-agent-shell--kill-buffer-hook)

(defun meta-agent-shell--load-killed-agents (project-path)
  "Load killed agent records for PROJECT-PATH from disk.
Returns list of plists with :name, :initial-task, :last-output, :timestamp, :was-busy."
  (let* ((project-name (file-name-nondirectory (directory-file-name project-path)))
         (dir (expand-file-name project-name
                                (expand-file-name meta-agent-shell-killed-agents-directory)))
         (results nil))
    (when (file-directory-p dir)
      (dolist (file (directory-files dir t "\\.json$"))
        (condition-case nil
            (let* ((json-object-type 'alist)
                   (json-key-type 'symbol)
                   (data (json-read-file file)))
              (push (list :name (alist-get 'name data)
                          :initial-task (alist-get 'initial_task data)
                          :last-output (alist-get 'last_output data)
                          :timestamp (alist-get 'timestamp data)
                          :was-busy (alist-get 'was_busy data))
                    results))
          (error nil))))
    (nreverse results)))

(provide 'meta-agent-shell)
;;; meta-agent-shell.el ends here

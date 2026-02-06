;;; meta-agent-shell-test.el --- Tests for meta-agent-shell -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Elle Najt

;;; Commentary:

;; ERT tests for meta-agent-shell.el
;;
;; Run tests:
;;   emacs -batch -l ert -l meta-agent-shell-test.el -f ert-run-tests-batch-and-exit
;;
;; Or from Emacs:
;;   M-x ert RET t RET

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)

;; Stub dependencies before loading meta-agent-shell
;; These provide minimal definitions so the file can load in batch mode
(unless (featurep 'agent-shell)
  (provide 'agent-shell)
  (defun agent-shell (&optional _prefix _buffer-name)
    "Stub for agent-shell."
    (get-buffer-create "*agent-shell*"))
  (defun agent-shell-buffers ()
    "Stub returning empty list."
    nil)
  (defun agent-shell-interrupt (&optional _force)
    "Stub for interrupt."
    nil))

(unless (featurep 'shell-maker)
  (provide 'shell-maker)
  (defun shell-maker-submit (&rest _args)
    "Stub for shell-maker-submit."
    nil)
  (defun shell-maker-busy ()
    "Stub for shell-maker-busy."
    nil))

;; Load the package under test
(load-file (expand-file-name "meta-agent-shell.el"
                             (file-name-directory
                              (or load-file-name buffer-file-name))))

;;; Test utilities and mocks

(defvar meta-agent-shell-test--mock-buffers nil
  "List of mock buffers created during tests.")

(defvar meta-agent-shell-test--submitted-messages nil
  "List of messages submitted via shell-maker-submit during tests.")

(defvar meta-agent-shell-test--temp-files nil
  "List of temp files created during tests.")

(defmacro meta-agent-shell-test--with-clean-state (&rest body)
  "Execute BODY with fresh meta-agent-shell state."
  (declare (indent 0))
  `(let ((meta-agent-shell--buffer nil)
         (meta-agent-shell--heartbeat-timer nil)
         (meta-agent-shell--last-user-interaction nil)
         (meta-agent-shell--dispatchers nil)
         (meta-agent-shell--allowed-targets nil)
         (meta-agent-shell-restrict-targets nil)
         (meta-agent-shell-test--mock-buffers nil)
         (meta-agent-shell-test--submitted-messages nil)
         (meta-agent-shell-test--temp-files nil))
     (unwind-protect
         (progn ,@body)
       ;; Cleanup
       (dolist (buf meta-agent-shell-test--mock-buffers)
         (when (buffer-live-p buf)
           (kill-buffer buf)))
       (dolist (file meta-agent-shell-test--temp-files)
         (when (file-exists-p file)
           (delete-file file))))))

(defun meta-agent-shell-test--make-mock-buffer (name &optional project-path)
  "Create a mock agent-shell buffer with NAME.
Optional PROJECT-PATH sets the default-directory."
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (setq default-directory (or project-path "/tmp/test-project/"))
      ;; Mock agent-shell--state for status detection
      (setq-local agent-shell--state
                  '(:client (:process nil)
                    :session (:mode-id "code"))))
    (push buf meta-agent-shell-test--mock-buffers)
    buf))

(defun meta-agent-shell-test--mock-agent-shell-buffers ()
  "Return the list of mock buffers (simulating agent-shell-buffers)."
  meta-agent-shell-test--mock-buffers)

(defun meta-agent-shell-test--mock-shell-maker-submit (&rest args)
  "Mock shell-maker-submit that records the :input."
  (let ((input (plist-get args :input)))
    (push input meta-agent-shell-test--submitted-messages)))

(defun meta-agent-shell-test--mock-shell-maker-busy ()
  "Mock shell-maker-busy that returns nil (not busy)."
  nil)

(defun meta-agent-shell-test--create-temp-file (content)
  "Create a temp file with CONTENT, return the path."
  (let ((file (make-temp-file "meta-agent-shell-test-")))
    (with-temp-file file
      (insert content))
    (push file meta-agent-shell-test--temp-files)
    file))


;;; Target Restriction Tests

(ert-deftest meta-agent-shell-test-allow-target ()
  "Test adding targets to allowed list."
  (meta-agent-shell-test--with-clean-state
   ;; First add should succeed
   (should (eq t (meta-agent-shell-allow-target "Buffer1")))
   (should (member "Buffer1" (meta-agent-shell-list-allowed-targets)))
   ;; Duplicate add should return nil
   (should (eq nil (meta-agent-shell-allow-target "Buffer1")))
   ;; Add another
   (should (eq t (meta-agent-shell-allow-target "Buffer2")))
   (should (= 2 (length (meta-agent-shell-list-allowed-targets))))))

(ert-deftest meta-agent-shell-test-disallow-target ()
  "Test removing targets from allowed list."
  (meta-agent-shell-test--with-clean-state
   (meta-agent-shell-allow-target "Buffer1")
   (meta-agent-shell-allow-target "Buffer2")
   (meta-agent-shell-disallow-target "Buffer1")
   (should-not (member "Buffer1" (meta-agent-shell-list-allowed-targets)))
   (should (member "Buffer2" (meta-agent-shell-list-allowed-targets)))))

(ert-deftest meta-agent-shell-test-target-allowed-p-unrestricted ()
  "Test target checking when restrictions are disabled."
  (meta-agent-shell-test--with-clean-state
   (setq meta-agent-shell-restrict-targets nil)
   ;; Everything should be allowed when restrictions are off
   (should (meta-agent-shell--target-allowed-p "AnyBuffer"))
   (should (meta-agent-shell--target-allowed-p "RandomName"))))

(ert-deftest meta-agent-shell-test-target-allowed-p-restricted ()
  "Test target checking when restrictions are enabled."
  (meta-agent-shell-test--with-clean-state
   (setq meta-agent-shell-restrict-targets t)
   ;; Nothing allowed initially
   (should-not (meta-agent-shell--target-allowed-p "SomeBuffer"))
   ;; Add to allowed list
   (meta-agent-shell-allow-target "AllowedBuffer")
   (should (meta-agent-shell--target-allowed-p "AllowedBuffer"))
   (should-not (meta-agent-shell--target-allowed-p "OtherBuffer"))))


;;; Helper Function Tests

(ert-deftest meta-agent-shell-test-get-buffer-status ()
  "Test extracting status info from a buffer."
  (meta-agent-shell-test--with-clean-state
   (cl-letf (((symbol-function 'shell-maker-busy)
              #'meta-agent-shell-test--mock-shell-maker-busy))
     (let* ((buf (meta-agent-shell-test--make-mock-buffer
                  "Test Agent @ myproject"
                  "/home/user/myproject/"))
            (status (meta-agent-shell--get-buffer-status buf)))
       (should (plist-get status :buffer))
       (should (equal "myproject" (plist-get status :project)))
       (should (equal "/home/user/myproject/" (plist-get status :project-path)))
       (should (equal "ready" (plist-get status :status)))
       (should (equal "code" (plist-get status :mode)))))))

(ert-deftest meta-agent-shell-test-get-buffer-recent-output ()
  "Test getting recent output from a buffer."
  (meta-agent-shell-test--with-clean-state
   (let ((buf (meta-agent-shell-test--make-mock-buffer "TestBuffer")))
     (with-current-buffer buf
       (insert "Line 1\nLine 2\nLine 3\nLine 4\nLine 5\n"))
     (let ((output (meta-agent-shell--get-buffer-recent-output buf 3)))
       (should (stringp output))
       (should (string-match-p "Line 3" output))
       (should (string-match-p "Line 4" output))
       (should (string-match-p "Line 5" output))))))

(ert-deftest meta-agent-shell-test-find-buffer-by-project ()
  "Test finding buffer by project name."
  (meta-agent-shell-test--with-clean-state
   (cl-letf (((symbol-function 'agent-shell-buffers)
              #'meta-agent-shell-test--mock-agent-shell-buffers))
     (meta-agent-shell-test--make-mock-buffer
      "Agent @ project-one" "/home/user/project-one/")
     (meta-agent-shell-test--make-mock-buffer
      "Agent @ project-two" "/home/user/project-two/")
     ;; Find by exact name
     (let ((found (meta-agent-shell--find-buffer-by-project "project-one")))
       (should found)
       (should (equal "Agent @ project-one" (buffer-name found))))
     ;; Find by case-insensitive name
     (let ((found (meta-agent-shell--find-buffer-by-project "PROJECT-TWO")))
       (should found)
       (should (equal "Agent @ project-two" (buffer-name found))))
     ;; Not found
     (should-not (meta-agent-shell--find-buffer-by-project "nonexistent")))))

(ert-deftest meta-agent-shell-test-active-buffers-excludes-meta ()
  "Test that active-buffers excludes the meta buffer."
  (meta-agent-shell-test--with-clean-state
   (cl-letf (((symbol-function 'agent-shell-buffers)
              #'meta-agent-shell-test--mock-agent-shell-buffers))
     (let ((agent-buf (meta-agent-shell-test--make-mock-buffer "Agent @ project"))
           (meta-buf (meta-agent-shell-test--make-mock-buffer "Meta Agent")))
       (setq meta-agent-shell--buffer meta-buf)
       (let ((active (meta-agent-shell--active-buffers)))
         (should (memq agent-buf active))
         (should-not (memq meta-buf active)))))))

(ert-deftest meta-agent-shell-test-active-buffers-excludes-dispatchers ()
  "Test that active-buffers excludes dispatcher buffers."
  (meta-agent-shell-test--with-clean-state
   (cl-letf (((symbol-function 'agent-shell-buffers)
              #'meta-agent-shell-test--mock-agent-shell-buffers))
     (let ((agent-buf (meta-agent-shell-test--make-mock-buffer "Agent @ project"))
           (dispatcher-buf (meta-agent-shell-test--make-mock-buffer "Dispatcher @ project")))
       (push (cons "/path/to/project/" dispatcher-buf) meta-agent-shell--dispatchers)
       (let ((active (meta-agent-shell--active-buffers)))
         (should (memq agent-buf active))
         (should-not (memq dispatcher-buf active)))))))


;;; ICC Logging Tests

(ert-deftest meta-agent-shell-test-log-icc ()
  "Test that ICC logging writes valid JSONL."
  (meta-agent-shell-test--with-clean-state
   (let* ((temp-dir (make-temp-file "meta-agent-shell-log-" t))
          (meta-agent-shell-log-directory temp-dir)
          (expected-file (expand-file-name
                          (format "%s-icc.jsonl" (format-time-string "%Y-%m-%d"))
                          temp-dir)))
     (push expected-file meta-agent-shell-test--temp-files)
     (push temp-dir meta-agent-shell-test--temp-files)
     ;; Log a message
     (meta-agent-shell--log-icc "Sender" "Receiver" "Test message" "send")
     ;; Check file was created and contains valid JSON
     (should (file-exists-p expected-file))
     (with-temp-buffer
       (insert-file-contents expected-file)
       (let* ((content (buffer-string))
              (json-object-type 'alist)
              (parsed (json-read-from-string (car (split-string content "\n" t)))))
         (should (assoc 'from parsed))
         (should (assoc 'to parsed))
         (should (assoc 'message parsed))
         (should (assoc 'type parsed))
         (should (equal "Sender" (cdr (assoc 'from parsed))))
         (should (equal "Receiver" (cdr (assoc 'to parsed))))
         (should (equal "Test message" (cdr (assoc 'message parsed))))
         (should (equal "send" (cdr (assoc 'type parsed))))))
     ;; Cleanup temp dir
     (delete-directory temp-dir t))))


;;; Session Management Tests

(ert-deftest meta-agent-shell-test-list-sessions ()
  "Test listing active sessions."
  (meta-agent-shell-test--with-clean-state
   (cl-letf (((symbol-function 'agent-shell-buffers)
              #'meta-agent-shell-test--mock-agent-shell-buffers)
             ((symbol-function 'shell-maker-busy)
              #'meta-agent-shell-test--mock-shell-maker-busy))
     (meta-agent-shell-test--make-mock-buffer
      "Agent @ project-a" "/home/user/project-a/")
     (meta-agent-shell-test--make-mock-buffer
      "Agent @ project-b" "/home/user/project-b/")
     (let ((sessions (meta-agent-shell-list-sessions)))
       (should (= 2 (length sessions)))
       (should (cl-some (lambda (s) (equal "project-a" (plist-get s :project)))
                        sessions))
       (should (cl-some (lambda (s) (equal "project-b" (plist-get s :project)))
                        sessions))))))

(ert-deftest meta-agent-shell-test-view-session ()
  "Test viewing session output."
  (meta-agent-shell-test--with-clean-state
   (let ((buf (meta-agent-shell-test--make-mock-buffer "TestSession")))
     (with-current-buffer buf
       (insert "Session output line 1\nSession output line 2\n"))
     (let ((output (meta-agent-shell-view-session "TestSession" 10)))
       (should (stringp output))
       (should (string-match-p "Session output" output))))))

(ert-deftest meta-agent-shell-test-view-session-nonexistent ()
  "Test viewing output from nonexistent session."
  (meta-agent-shell-test--with-clean-state
   (should-not (meta-agent-shell-view-session "NonexistentBuffer"))))

(ert-deftest meta-agent-shell-test-close-session ()
  "Test closing a session."
  (meta-agent-shell-test--with-clean-state
   (cl-letf (((symbol-function 'agent-shell-buffers)
              #'meta-agent-shell-test--mock-agent-shell-buffers))
     (let ((buf (meta-agent-shell-test--make-mock-buffer "ToClose")))
       (should (buffer-live-p buf))
       (should (eq t (meta-agent-shell-close-session "ToClose")))
       (should-not (buffer-live-p buf))))))

(ert-deftest meta-agent-shell-test-close-session-nonexistent ()
  "Test closing nonexistent session returns nil."
  (meta-agent-shell-test--with-clean-state
   (should-not (meta-agent-shell-close-session "NonexistentBuffer"))))


;;; Message Sending Tests

(ert-deftest meta-agent-shell-test-send-to-session ()
  "Test sending message to a session."
  (meta-agent-shell-test--with-clean-state
   (cl-letf (((symbol-function 'agent-shell-buffers)
              #'meta-agent-shell-test--mock-agent-shell-buffers)
             ((symbol-function 'shell-maker-submit)
              #'meta-agent-shell-test--mock-shell-maker-submit))
     (let* ((temp-dir (make-temp-file "meta-agent-shell-log-" t))
            (meta-agent-shell-log-directory temp-dir))
       (push temp-dir meta-agent-shell-test--temp-files)
       (meta-agent-shell-test--make-mock-buffer "TargetBuffer")
       (should (eq t (meta-agent-shell-send-to-session
                      "TargetBuffer" "Hello!" "SenderAgent")))
       ;; Check message was submitted
       (should (= 1 (length meta-agent-shell-test--submitted-messages)))
       (should (string-match-p "Message from SenderAgent"
                               (car meta-agent-shell-test--submitted-messages)))
       (should (string-match-p "Hello!"
                               (car meta-agent-shell-test--submitted-messages)))
       (delete-directory temp-dir t)))))

(ert-deftest meta-agent-shell-test-send-to-session-restricted ()
  "Test that send-to-session respects target restrictions."
  (meta-agent-shell-test--with-clean-state
   (cl-letf (((symbol-function 'agent-shell-buffers)
              #'meta-agent-shell-test--mock-agent-shell-buffers))
     (setq meta-agent-shell-restrict-targets t)
     (meta-agent-shell-test--make-mock-buffer "RestrictedTarget")
     ;; Should error when target not allowed
     (should-error (meta-agent-shell-send-to-session
                    "RestrictedTarget" "Hello!" "Sender"))
     ;; Allow the target
     (meta-agent-shell-allow-target "RestrictedTarget")
     ;; Now mock shell-maker-submit and try again
     (cl-letf (((symbol-function 'shell-maker-submit)
                #'meta-agent-shell-test--mock-shell-maker-submit))
       (let* ((temp-dir (make-temp-file "meta-agent-shell-log-" t))
              (meta-agent-shell-log-directory temp-dir))
         (push temp-dir meta-agent-shell-test--temp-files)
         (should (eq t (meta-agent-shell-send-to-session
                        "RestrictedTarget" "Hello!" "Sender")))
         (delete-directory temp-dir t))))))

(ert-deftest meta-agent-shell-test-ask-session ()
  "Test asking a question to a session."
  (meta-agent-shell-test--with-clean-state
   (cl-letf (((symbol-function 'agent-shell-buffers)
              #'meta-agent-shell-test--mock-agent-shell-buffers)
             ((symbol-function 'shell-maker-submit)
              #'meta-agent-shell-test--mock-shell-maker-submit))
     (let* ((temp-dir (make-temp-file "meta-agent-shell-log-" t))
            (meta-agent-shell-log-directory temp-dir))
       (push temp-dir meta-agent-shell-test--temp-files)
       (meta-agent-shell-test--make-mock-buffer "TargetBuffer")
       (should (eq t (meta-agent-shell-ask-session
                      "TargetBuffer" "What is the status?" "Asker")))
       ;; Check message contains question and reply instructions
       (let ((msg (car meta-agent-shell-test--submitted-messages)))
         (should (string-match-p "Question from Asker" msg))
         (should (string-match-p "What is the status?" msg))
         (should (string-match-p "agent-send" msg))
         (should (string-match-p "Asker" msg)))
       (delete-directory temp-dir t)))))

(ert-deftest meta-agent-shell-test-ask-session-requires-from ()
  "Test that ask-session requires a FROM parameter."
  (meta-agent-shell-test--with-clean-state
   (cl-letf (((symbol-function 'agent-shell-buffers)
              #'meta-agent-shell-test--mock-agent-shell-buffers))
     (meta-agent-shell-test--make-mock-buffer "TargetBuffer")
     ;; Should return nil when from is not provided
     (should-not (meta-agent-shell-ask-session "TargetBuffer" "Question?" nil)))))


;;; Search Tests

(ert-deftest meta-agent-shell-test-search-sessions ()
  "Test searching across sessions."
  (meta-agent-shell-test--with-clean-state
   (cl-letf (((symbol-function 'agent-shell-buffers)
              #'meta-agent-shell-test--mock-agent-shell-buffers))
     (let ((buf1 (meta-agent-shell-test--make-mock-buffer
                  "Agent @ proj1" "/home/user/proj1/"))
           (buf2 (meta-agent-shell-test--make-mock-buffer
                  "Agent @ proj2" "/home/user/proj2/")))
       (with-current-buffer buf1
         (insert "Error: something went wrong\nWarning: be careful\n"))
       (with-current-buffer buf2
         (insert "All good here\nError: another issue\n"))
       (let ((results (meta-agent-shell-search-sessions "Error")))
         (should (= 2 (length results)))
         (should (cl-some (lambda (r) (equal "proj1" (plist-get r :project)))
                          results))
         (should (cl-some (lambda (r) (equal "proj2" (plist-get r :project)))
                          results)))))))


;;; Heartbeat Tests

(ert-deftest meta-agent-shell-test-buffer-alive-p ()
  "Test meta buffer alive check."
  (meta-agent-shell-test--with-clean-state
   (should-not (meta-agent-shell--buffer-alive-p))
   (let ((buf (meta-agent-shell-test--make-mock-buffer "Meta")))
     (setq meta-agent-shell--buffer buf)
     (should (meta-agent-shell--buffer-alive-p))
     (kill-buffer buf)
     (should-not (meta-agent-shell--buffer-alive-p)))))

(ert-deftest meta-agent-shell-test-cooldown-elapsed-p ()
  "Test cooldown logic."
  (meta-agent-shell-test--with-clean-state
   ;; No interaction yet - cooldown should be elapsed
   (should (meta-agent-shell--cooldown-elapsed-p))
   ;; Set recent interaction
   (setq meta-agent-shell--last-user-interaction (float-time))
   (should-not (meta-agent-shell--cooldown-elapsed-p))
   ;; Set old interaction (beyond cooldown)
   (setq meta-agent-shell--last-user-interaction
         (- (float-time) (1+ meta-agent-shell-heartbeat-cooldown)))
   (should (meta-agent-shell--cooldown-elapsed-p))))

(ert-deftest meta-agent-shell-test-format-heartbeat ()
  "Test heartbeat message formatting."
  (meta-agent-shell-test--with-clean-state
   (cl-letf (((symbol-function 'agent-shell-buffers)
              #'meta-agent-shell-test--mock-agent-shell-buffers)
             ((symbol-function 'shell-maker-busy)
              #'meta-agent-shell-test--mock-shell-maker-busy))
     ;; Create a temp heartbeat file
     (let ((heartbeat-file (meta-agent-shell-test--create-temp-file
                            "* Standing Instructions\nMonitor all agents.")))
       (setq meta-agent-shell-heartbeat-file heartbeat-file)
       ;; Add some mock sessions
       (meta-agent-shell-test--make-mock-buffer
        "Agent @ testproj" "/home/user/testproj/")
       (let ((heartbeat (meta-agent-shell--format-heartbeat)))
         (should (stringp heartbeat))
         (should (string-match-p "Heartbeat" heartbeat))
         (should (string-match-p "Active Sessions" heartbeat))
         (should (string-match-p "testproj" heartbeat))
         (should (string-match-p "Instructions" heartbeat))
         (should (string-match-p "Monitor all agents" heartbeat)))))))


;;; Spawn Hook Tests

;; Global vars for tracking hook calls in tests (hooks run in a different scope)
(defvar meta-agent-shell-test--before-hook-called nil)
(defvar meta-agent-shell-test--after-hook-called nil)
(defvar meta-agent-shell-test--after-hook-buffer nil)

(ert-deftest meta-agent-shell-test-spawn-hooks-called ()
  "Test that before and after spawn hooks are called."
  (meta-agent-shell-test--with-clean-state
   (setq meta-agent-shell-test--before-hook-called nil
         meta-agent-shell-test--after-hook-called nil
         meta-agent-shell-test--after-hook-buffer nil)
   (let ((spawn-called nil)
         (before-hook-fn (lambda ()
                           (setq meta-agent-shell-test--before-hook-called t)))
         (after-hook-fn (lambda ()
                          (setq meta-agent-shell-test--after-hook-called t
                                meta-agent-shell-test--after-hook-buffer (current-buffer)))))
     ;; Set up hooks
     (add-hook 'meta-agent-shell-before-spawn-hook before-hook-fn)
     (add-hook 'meta-agent-shell-after-spawn-hook after-hook-fn)
     ;; Override the start function variable (it's a defcustom, not a function)
     (let ((meta-agent-shell-start-function
            (lambda (&optional _prefix _buffer-name)
              (setq spawn-called t)
              (switch-to-buffer (get-buffer-create "Test Agent @ testdir")))))
       (make-directory "/tmp/testdir/" t)
       (unwind-protect
           (progn
             (meta-agent-shell-start-named-agent "/tmp/testdir/" "TestAgent")
             (should meta-agent-shell-test--before-hook-called)
             (should meta-agent-shell-test--after-hook-called)
             (should spawn-called)
             (should (bufferp meta-agent-shell-test--after-hook-buffer)))
         ;; Cleanup
         (when (get-buffer "Test Agent @ testdir")
           (kill-buffer "Test Agent @ testdir"))
         (ignore-errors (delete-directory "/tmp/testdir/" t))
         (remove-hook 'meta-agent-shell-before-spawn-hook before-hook-fn)
         (remove-hook 'meta-agent-shell-after-spawn-hook after-hook-fn))))))


;;; Dispatcher Tests

(ert-deftest meta-agent-shell-test-list-dispatchers-empty ()
  "Test listing dispatchers when none exist."
  (meta-agent-shell-test--with-clean-state
   (should (null (meta-agent-shell-list-dispatchers)))))

(ert-deftest meta-agent-shell-test-list-dispatchers ()
  "Test listing active dispatchers."
  (meta-agent-shell-test--with-clean-state
   (let ((buf (meta-agent-shell-test--make-mock-buffer "Dispatcher @ proj")))
     (push (cons "/home/user/proj/" buf) meta-agent-shell--dispatchers)
     (let ((dispatchers (meta-agent-shell-list-dispatchers)))
       (should (= 1 (length dispatchers)))
       (should (equal "proj" (plist-get (car dispatchers) :project)))))))

(ert-deftest meta-agent-shell-test-list-dispatchers-cleans-dead ()
  "Test that list-dispatchers removes dead buffers."
  (meta-agent-shell-test--with-clean-state
   (let ((live-buf (meta-agent-shell-test--make-mock-buffer "Live Dispatcher"))
         (dead-buf (get-buffer-create "Dead Dispatcher")))
     (push (cons "/home/user/live/" live-buf) meta-agent-shell--dispatchers)
     (push (cons "/home/user/dead/" dead-buf) meta-agent-shell--dispatchers)
     (kill-buffer dead-buf)
     (let ((dispatchers (meta-agent-shell-list-dispatchers)))
       (should (= 1 (length dispatchers)))
       (should (= 1 (length meta-agent-shell--dispatchers)))))))

(ert-deftest meta-agent-shell-test-get-project-agents ()
  "Test getting agents for a specific project."
  (meta-agent-shell-test--with-clean-state
   (cl-letf (((symbol-function 'agent-shell-buffers)
              #'meta-agent-shell-test--mock-agent-shell-buffers))
     (meta-agent-shell-test--make-mock-buffer
      "Agent1 @ proj" "/home/user/proj/")
     (meta-agent-shell-test--make-mock-buffer
      "Agent2 @ proj" "/home/user/proj/")
     (meta-agent-shell-test--make-mock-buffer
      "Agent @ other" "/home/user/other/")
     (let ((agents (meta-agent-shell-get-project-agents "/home/user/proj/")))
       (should (= 2 (length agents)))
       (should (member "Agent1 @ proj" agents))
       (should (member "Agent2 @ proj" agents))))))

(ert-deftest meta-agent-shell-test-send-to-dispatcher ()
  "Test sending message to a dispatcher."
  (meta-agent-shell-test--with-clean-state
   (cl-letf (((symbol-function 'shell-maker-submit)
              #'meta-agent-shell-test--mock-shell-maker-submit))
     (let* ((temp-dir (make-temp-file "meta-agent-shell-log-" t))
            (meta-agent-shell-log-directory temp-dir)
            (buf (meta-agent-shell-test--make-mock-buffer "Dispatcher")))
       (push temp-dir meta-agent-shell-test--temp-files)
       (push (cons "/home/user/myproj/" buf) meta-agent-shell--dispatchers)
       (should (eq t (meta-agent-shell-send-to-dispatcher
                      "myproj" "Route this task" "MetaAgent")))
       (should (= 1 (length meta-agent-shell-test--submitted-messages)))
       (should (string-match-p "Message from MetaAgent"
                               (car meta-agent-shell-test--submitted-messages)))
       (delete-directory temp-dir t)))))

(ert-deftest meta-agent-shell-test-close-dispatcher ()
  "Test closing a dispatcher."
  (meta-agent-shell-test--with-clean-state
   (let ((buf (meta-agent-shell-test--make-mock-buffer "Dispatcher @ proj")))
     (push (cons "/home/user/proj/" buf) meta-agent-shell--dispatchers)
     (should (= 1 (length meta-agent-shell--dispatchers)))
     (should (eq t (meta-agent-shell-close-dispatcher "proj")))
     (should (= 0 (length meta-agent-shell--dispatchers)))
     (should-not (buffer-live-p buf)))))


;;; Whoami Tests

(ert-deftest meta-agent-shell-test-whoami-not-found ()
  "Test whoami returns nil when buffer not found."
  (meta-agent-shell-test--with-clean-state
   (cl-letf (((symbol-function 'agent-shell-buffers)
              #'meta-agent-shell-test--mock-agent-shell-buffers))
     ;; No buffers, should return nil
     (should-not (meta-agent-shell-whoami 12345)))))


;;; Integration Tests

(ert-deftest meta-agent-shell-test-full-workflow ()
  "Test a typical workflow: spawn agent, send message, close."
  (meta-agent-shell-test--with-clean-state
   (cl-letf (((symbol-function 'agent-shell-buffers)
              #'meta-agent-shell-test--mock-agent-shell-buffers)
             ((symbol-function 'shell-maker-submit)
              #'meta-agent-shell-test--mock-shell-maker-submit)
             ((symbol-function 'shell-maker-busy)
              #'meta-agent-shell-test--mock-shell-maker-busy))
     (let* ((temp-dir (make-temp-file "meta-agent-shell-log-" t))
            (meta-agent-shell-log-directory temp-dir))
       (push temp-dir meta-agent-shell-test--temp-files)
       ;; Create a mock agent (simulating spawn)
       (let ((agent (meta-agent-shell-test--make-mock-buffer
                     "Worker @ testproj" "/home/user/testproj/")))
         ;; List should show the agent
         (let ((sessions (meta-agent-shell-list-sessions)))
           (should (= 1 (length sessions)))
           (should (equal "testproj" (plist-get (car sessions) :project))))
         ;; Send a message
         (should (eq t (meta-agent-shell-send-to-session
                        "Worker @ testproj" "Do the thing" "Dispatcher")))
         ;; Verify message was sent
         (should (string-match-p "Do the thing"
                                 (car meta-agent-shell-test--submitted-messages)))
         ;; Close the agent
         (should (eq t (meta-agent-shell-close-session "Worker @ testproj")))
         ;; List should be empty
         (should (= 0 (length (meta-agent-shell-list-sessions)))))
       (delete-directory temp-dir t)))))


(provide 'meta-agent-shell-test)
;;; meta-agent-shell-test.el ends here

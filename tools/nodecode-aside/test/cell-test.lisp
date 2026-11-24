;;;; cell-test.lisp --- the lifecycle, the five tools, the command, the gate.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here: that the cell installs its five tools, its command
;;;; and its manual and takes every one of them back on stop; that a section
;;;; naming no executable, a choice outside its options, or a number under its
;;;; floor refuses at start; that each tool builds the argument vector the CLI
;;;; expects, under the section's pins, and answers what it said; that a
;;;; missing executable, a timeout and a bad pin each answer a refusal the
;;;; model can act on rather than a crash; that a screenshot's base64 is
;;;; redacted; that archive and delete need confirm; and that the gate keeps
;;;; the built-in browser out only when the section says always and Aside
;;;; answers.

(in-package #:nodecode.test)

;;; --- lifecycle -------------------------------------------------------------------

(define-cell-lifecycle-tests "aside"
  (:help :aside)
  (:command "aside")
  (:running
   (is (equal "ask" (nodecode-aside::setting :permission)) "permission defaults to ask: what the CLI may do without asking is the operator's")
   (is (equal "prefer" (nodecode-aside::setting :browser-policy)) "and the built-in browser is only refused when the operator says always")
   (is (equal 300 (nodecode-aside::setting :timeout-seconds)) "one task's ceiling defaults to five minutes")
   (is (equal '("exec" "--permission" "ask" "go") (nodecode-aside::exec-argv "go")) "exec carries the permission and nothing the section left empty"))
  (:refused ("bin" nil) ("permission" "wild") ("timeout_seconds" 0))
  (:idle nodecode-aside::aside-error (nodecode-aside::find-aside)))

;;; --- the tools ---------------------------------------------------------------------

(deftest aside-cell-the-status-tool-reads-the-probe-and-the-pins ()
  (with-cell-stop ((aside-start "host" "local" "account" "u0"))
    (with-aside-cli (:answering (answering "repl" (list "ASIDE_READY" "" 0)))
      (let ((answer (aside-json (aside-tool "aside_status"))))
        (is (equal "READY" (nlk:json-value answer :string "status")) "the marker the probe looks for is what makes it ready")
        (is (equal "local" (nlk:json-value answer :string "profile" "host")) "the host pin is reported where the model reads it")
        (is (equal "ask" (nlk:json-value answer :string "profile" "permission")))
        (is (= 1 (length (aside-argv))) "one probe ran, and nothing else")))))

(deftest aside-cell-the-exec-tool-puts-the-pins-on-the-command-line ()
  (with-cell-stop ((aside-start "model" "aside/one" "speed" "fast"))
    (with-aside-cli (:answering (answering "exec" (list "the dashboard, read [ok] ses_abc123" "" 0)))
      (let ((answer (aside-json (aside-tool "aside_exec" "prompt" "read the dashboard"))))
        (is (eq t (nlk:json-value answer :boolean "ok")))
        (is (equal '("ses_abc123") (coerce (nlk:json-value answer :any "session_ids") 'list)) "the session id the answer named is handed back")
        (is (equal '("/usr/local/bin/aside" "exec" "--model" "aside/one" "--speed" "fast"
                      "--permission" "ask" "read the dashboard")
                   (first (aside-argv))) "the pins ride the call and the prompt is last")))))

(deftest aside-cell-a-call-wins-over-the-pin-for-this-turn-only ()
  (with-cell-stop ((aside-start "effort" "low"))
    (with-aside-cli ()
      (aside-tool "aside_exec" "prompt" "go" "effort" "high" "model" "aside/two")
      (is (equal '("exec" "--model" "aside/two" "--effort" "high" "--permission" "ask" "go")
                 (cdr (first (aside-argv)))) "what the call names wins over the matching pin")
      (aside-tool "aside_exec" "prompt" "again")
      (is (equal '("exec" "--effort" "low" "--permission" "ask" "again")
                 (cdr (second (aside-argv)))) "and the pin is back for the next call"))))

(deftest aside-cell-a-session-id-is-refused-rather-than-quietly-ignored ()
  (with-cell-stop ((aside-start))
    (with-aside-cli ()
      (let ((refusal (aside-refusal "aside_exec" "prompt" "go" "session_id" "ses_1")))
        (is (and refusal (search "session_id" (princ-to-string refusal))) "the call is refused, and names the member")
        (is (null (aside-argv)) "so no new session is started in its place")))))

(deftest aside-cell-a-pin-outside-its-values-is-a-refusal-to-read ()
  (with-cell-stop ((aside-start "effort" "hyper"))
    (with-aside-cli ()
      (let ((answer (aside-json (aside-tool "aside_exec" "prompt" "go"))))
        (is (equal "INVALID_INPUT" (nlk:json-value answer :string "status")))
        (is (search "effort" (nlk:json-value answer :string "message")) "the refusal names the setting")
        (is (null (aside-argv)) "and nothing ran")))))

(deftest aside-cell-with-no-executable-the-tools-say-so ()
  (with-cell-stop ((aside-start))
    (with-aside-cli (:binary nil)
      (let ((answer (aside-json (aside-tool "aside_status"))))
        (is (equal "NEEDS_ASIDE" (nlk:json-value answer :string "status")))
        (is (search "never runs an installer" (nlk:json-value answer :string "message")))
        (is (null (aside-argv)) "no process was started")))))

(deftest aside-cell-a-timeout-says-the-session-may-still-run ()
  (with-cell-stop ((aside-start))
    (with-aside-cli (:answering (always-answering "" "" :timeout))
      (let ((answer (aside-json (aside-tool "aside_exec" "prompt" "go"))))
        (is (equal "TIMEOUT" (nlk:json-value answer :string "status")))
        (is (null (nlk:json-value answer :boolean "ok")))
        (is (search "may still be running" (nlk:json-value answer :string "message")) "the model is told the session was not stopped")))))

(deftest aside-cell-a-repl-answer-redacts-a-screenshot ()
  (with-cell-stop ((aside-start))
    (with-aside-cli (:answering (always-answering (format nil "~a~%ASIDE_STEP_OK" (make-string 600 :initial-element #\A))))
      (let* ((answer (aside-json (aside-tool "aside_repl" "code" "console.log(1)")))
             (out (nlk:json-value answer :string "stdout")))
        (is (search "[redacted image or binary data, 600 chars]" out) "a long base64 line is named, not carried")
        (is (search "ASIDE_STEP_OK" out) "and the rest of the answer survives")
        (is (equal '("repl" "console.log(1)") (cdr (first (aside-argv)))) "one one-shot script was run")))))

(deftest aside-cell-destructive-session-actions-need-confirm ()
  (with-cell-stop ((aside-start))
    (with-aside-cli ()
      (let ((answer (aside-json (aside-tool "aside_session" "action" "delete" "session_id" "ses_1"))))
        (is (equal "CONFIRM_REQUIRED" (nlk:json-value answer :string "status")))
        (is (null (aside-argv)) "nothing ran before the confirmation"))
      (let ((answer (aside-json (aside-tool "aside_session" "action" "delete" "session_id" "ses_1"
                                             "confirm" t))))
        (is (eq t (nlk:json-value answer :boolean "ok")) "with confirm it runs")
        (is (equal '("session" "delete" "ses_1") (cdr (first (aside-argv)))) "and names the session and nothing else")))))

(deftest aside-cell-memory-searches-once-per-query-read-only ()
  (with-cell-stop ((aside-start "account" "u0"))
    (with-aside-cli (:answering (always-answering "[]"))
      (let ((answer (aside-json (aside-tool "aside_memory"
                                             "queries" (vector "ann" "the site")
                                             "max_results" 3))))
        (is (eq t (nlk:json-value answer :boolean "ok")))
        (is (= 2 (length (aside-argv))) "one search per query")
        (is (equal '("memory" "search" "--account" "u0" "--json" "-n" "3" "ann")
                   (cdr (first (aside-argv)))) "read-only, in the pinned account, asked for JSON")))))

;;; --- the command -------------------------------------------------------------------

(deftest aside-cell-the-command-shows-the-pins-then-does-what-it-is-told ()
  (with-cell-stop ((aside-start "host" "local"))
    (with-aside-cli (:answering (answering "repl" (list "ASIDE_READY" "" 0)
                                            "exec" (list "the dashboard, read" "" 0)
                                            "session" (list "ses_1 running" "" 0)))
      (multiple-value-bind (text dialog) (cell-entry "nodecode-aside" "aside" "")
        (is (search "READY" text) "bare /aside reads the probe")
        (is (search "local" text) "and says which host it would use")
        (is (plusp (length (princ-to-string dialog))) "and it is a panel"))
      (let ((line (cell-entry "nodecode-aside" "aside" "exec read the dashboard")))
        (is (equal "the dashboard, read" line) "exec answers the CLI's own output on one line"))
      (let ((line (cell-entry "nodecode-aside" "aside" "sessions")))
        (is (search "ses_1" line) "sessions answers what the CLI listed")))))

;;; --- the gate ----------------------------------------------------------------------

(deftest aside-cell-the-gate-refuses-the-built-in-browser-while-aside-answers ()
  (with-cell-stop ((aside-start "browser_policy" "always"))
    (with-aside-cli (:answering (answering "repl" (list "ASIDE_READY" "" 0)))
      (let* ((hook (cell-hook "nodecode-aside" :tool))
             (ran nil)
             (next (lambda (op) (declare (ignore op)) (setf ran t) "the built-in browser ran"))
             (refusal (funcall hook (list :name "browser_navigate" :arguments (nlk:json-object) :call-id "c1")
                               next)))
        (is (search "aside_exec" (princ-to-string refusal)) "the model is told what to call instead")
        (is (null ran) "and the built-in browser never ran"))
      (let ((hook (cell-hook "nodecode-aside" :tool))
            (next (lambda (op) (declare (ignore op)) "ran")))
        (is (equal "ran" (funcall hook (list :name "aside_exec" :arguments (nlk:json-object) :call-id "c2")
                                   next)) "this cell's own tools pass through")
        (is (equal "ran" (funcall hook (list :name "web_search" :arguments (nlk:json-object) :call-id "c3")
                                   next)) "so does a tool the section never blocked")))))

(deftest aside-cell-the-gate-is-open-unless-the-operator-says-always ()
  (with-cell-stop ((aside-start))
    (with-aside-cli (:answering (answering "repl" (list "ASIDE_READY" "" 0)))
      (let ((hook (cell-hook "nodecode-aside" :tool))
            (next (lambda (op) (declare (ignore op)) "ran")))
        (is (equal "ran" (funcall hook (list :name "browser_navigate" :arguments (nlk:json-object) :call-id "c4")
                                   next)) "prefer, the default, blocks nothing")))))

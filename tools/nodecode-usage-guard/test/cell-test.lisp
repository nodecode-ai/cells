;;;; cell-test.lisp --- lifecycle, the tool, the command, the cache and the round note.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here touches the network, the operator's files or a key: every
;;;; usage read is the function *FETCH* is bound to, each source's sign-in is
;;;; a temp file this file writes, and the reading is kept in a temp file.

(in-package #:nodecode.test)

;;; --- lifecycle ---------------------------------------------------------------------

(define-test-slice "usage-guard" "USAGE-GUARD-CELL-" :start nodecode-usage-guard:start-cell)

(define-cell-lifecycle-tests "usage-guard"
  (:config "sources" (vector "claude")
           "cache_file" (namestring (temp-path "usage-guard" "json")))
  (:hooks :round)
  (:help :usage-guard)
  (:command "usage-guard")
  (:running (is (nle::find-tool "usage_guard") "start registers the usage_guard tool"))
  (:stopped (is (null (nle::find-tool "usage_guard")) "stop unregisters the tool"))
  (:refused ("threshold_percent" 0) ("min_interval_s" -1)))

(deftest usage-guard-cell-a-section-that-names-no-source-refuses ()
  (is (signals-error nodecode-usage-guard::usage-guard-error (usage-guard-start "enabled" t)) "sources is required, so nothing starts")
  (is (signals-error nodecode-usage-guard::usage-guard-error (usage-guard-start "sources" (vector "nobody"))) "nor one naming a source it cannot read")
  (is (null (nle::find-tool "usage_guard")) "and nothing of it was installed"))

;;; --- fixtures --------------------------------------------------------------------

(defvar +reading-ok+
  "{\"five_hour\":{\"utilization\":12.5,\"resets_at\":\"2026-05-01T00:00:00+00:00\"},\"seven_day\":{\"utilization\":3.0}}"
  "A Claude usage answer with room left.")

(defvar +claude-sign-in+
  "{\"claudeAiOauth\":{\"accessToken\":\"claude-token\"}}"
  "What Claude Code's own login file holds, near enough.")

(defvar +codex-sign-in+
  "{\"tokens\":{\"access_token\":\"codex-token\",\"account_id\":\"acct-1\"}}"
  "What the Codex login file holds, near enough.")

(defvar *reads* 0
  "How many usage reads the answer under test made.")

(defun guard-reading (percent)
  "The Claude usage answer of one window PERCENT used."
  (format nil "{\"five_hour\":{\"utilization\":~,1f}}" (float percent)))

(defun call-with-guard (config thunk)
  "Call THUNK with the cell started on CONFIG over a temp cache and a temp
sign-in for each source, *READS* counting what the answer reads."
  (with-temp-file (cache)
    (with-temp-file (claude :contents +claude-sign-in+)
      (with-temp-file (codex :contents +codex-sign-in+)
        (let ((*reads* 0))
          (with-cell-stop ((apply #'usage-guard-start
                                  (append (list "sources" (vector "claude")
                                                "cache_file" cache
                                                "claude_credentials" claude
                                                "codex_credentials" codex)
                                          config)))
            (funcall thunk)))))))

(defun guard-ask (body &optional (status 200))
  "Ask for the report with every usage read answering BODY -- JSON text, or
NIL for an empty body -- and STATUS."
  (let ((nodecode-usage-guard::*fetch*
          (lambda (url headers)
            (declare (ignore url headers))
            (incf *reads*)
            (values (or body "") status))))
    (nth-value 0 (nodecode-usage-guard:report))))

(defun guard-command (&optional (argument ""))
  "/usage-guard ARGUMENT, run the way a shell with no model runs it."
  (let ((entry (nle::find-registered-command "usage-guard")))
    (unless entry (error "/usage-guard is not registered"))
    (funcall (nle::slash-command-headless entry) argument "s1")))

;;; --- the tool ---------------------------------------------------------------------

(deftest usage-guard-cell-the-tool-answers-through-its-registration ()
  (call-with-guard
   nil
   (lambda ()
     (let ((tool (nle::find-tool "usage_guard"))
           (nodecode-usage-guard::*fetch*
             (lambda (url headers)
               (declare (ignore url headers))
               (values +reading-ok+ 200))))
       (is (nle::find-tool "usage_guard") "the tool is registered")
       (flet ((ask (&rest pairs)
                (funcall (third tool)
                         (nlk:contract-decode (nlk:contract 'nodecode-usage-guard::usage-guard-request)
                                              (apply #'nlk:make-json-object pairs)))))
         (is (search "usage-guard OK" (ask "action" "status")) "and the tool answers the report")
         (is (search "usage-guard OK" (ask)) "with an action left out, status is what it does"))))))

(deftest usage-guard-cell-the-tool-reads-and-keeps-the-reading ()
  (call-with-guard
   nil
   (lambda ()
     (let ((text (guard-ask +reading-ok+)))
       (is (search "usage-guard OK" text) "the verdict is the first line")
       (is (search "5 hour 12.5%" text) "the window and its percent are named")
       (is (search "7 day 3.0%" text) "and every window is named")
       (is (= 1 *reads*) "one read answered it")
       (guard-ask +reading-ok+)
       (is (= 1 *reads*) "a second ask reuses the reading instead of reading again")
       (is (probe-file (nodecode-usage-guard::cache-path)) "the reading is kept in the cache file")))))

(deftest usage-guard-cell-the-tool-says-blocked-at-the-limit ()
  (call-with-guard
   nil
   (lambda ()
     (let ((text (guard-ask (guard-reading 100))))
       (is (search "usage-guard BLOCKED" text) "a window at the threshold is blocked")
       (is (search "AT THE LIMIT" text) "and the line names it")
       (is (search "Do not start new work" text) "with what to do about it")))))

(deftest usage-guard-cell-a-lower-threshold-blocks-earlier ()
  (call-with-guard
   (list "threshold_percent" 90)
   (lambda ()
     (is (search "usage-guard BLOCKED" (guard-ask (guard-reading 92))) "92% used is past a 90% threshold"))))

(deftest usage-guard-cell-no-reading-is-unconfirmed-unless-open ()
  (call-with-guard
   nil
   (lambda ()
     (let ((text (guard-ask nil 500)))
       (is (search "usage-guard UNVERIFIED" text) "a failing endpoint leaves nothing to decide on")
       (is (search "cannot confirm" text) "and says the source cannot be confirmed"))))
  (call-with-guard
   (list "fail_open" t)
   (lambda ()
     (is (search "usage-guard OK" (guard-ask nil 500)) "fail_open answers OK on no reading"))))

(deftest usage-guard-cell-a-failed-refresh-keeps-the-last-good-reading ()
  (call-with-guard
   (list "max_age_s" 0 "min_interval_s" 0)
   (lambda ()
     (is (search "12.5%" (guard-ask +reading-ok+)) "the first read is reported")
     (let ((text (guard-ask nil 429)))
       (is (search "5 hour 12.5%" text) "a rate-limited refresh keeps the last good windows")
       (is (not (search "BLOCKED" text)) "and does not refuse by itself")
       (is (= 2 *reads*) "the refresh was attempted")))))

(deftest usage-guard-cell-a-window-older-than-the-floor-is-not-reread ()
  (call-with-guard
   (list "max_age_s" 0 "min_interval_s" 300)
   (lambda ()
     (guard-ask +reading-ok+)
     (let ((text (guard-ask +reading-ok+)))
       (is (= 1 *reads*) "a read younger than the floor stands, however old it is")
       (is (search "read 0s ago" text) "and the report says how old it is")))))

(deftest usage-guard-cell-the-codex-answer-is-read-too ()
  (call-with-guard
   (list "sources" (vector "codex"))
   (lambda ()
     (let ((text (guard-ask "{\"plan_type\":\"plus\",\"rate_limit\":{\"allowed\":true,\"limit_reached\":false,\"primary_window\":{\"used_percent\":41.0,\"window_minutes\":300}}}")))
       (is (search "41.0%" text) "a Codex window's percent is read")
       (is (search "5 hours" text) "and named by the window length it reports")
       (is (search "usage-guard OK" text) "and the verdict is read off it")))))

(deftest usage-guard-cell-the-command-answers ()
  (call-with-guard
   nil
   (lambda ()
     (guard-ask +reading-ok+)
     (is (search "usage-guard OK" (guard-command)) "/usage-guard answers the same report")
     (is (search "status, check" (guard-command "now")) "and refuses a word it does not know"))))

;;; --- the round note ---------------------------------------------------------------

(deftest usage-guard-cell-a-full-window-rides-the-first-round ()
  (call-with-guard
   nil
   (lambda ()
     (guard-ask (guard-reading 92))
     (let ((seen '())
           (hook (cell-hook "nodecode-usage-guard" :round)))
       (is (functionp hook) "the round hook is installed")
       (with-stubbed-fdefinitions
           ((nlk:record-turn-context-prompt (turn &key text)
              (declare (ignore turn))
              (push text seen)))
         (flet ((one-round ()
                  (funcall hook (list :session-id "ug-round-1" :turn "t1" :round 0)
                           (lambda (op) (declare (ignore op)) nil))))
           (one-round)
           (one-round)))
       (is (= 1 (length seen)) "one note for one turn, not one per round")
       (is (search "[usage-guard]" (first seen)) "the note names the guard")
       (is (search "92" (first seen)) "and the level it crossed")))))

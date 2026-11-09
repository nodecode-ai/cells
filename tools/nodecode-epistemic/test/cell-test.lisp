;;;; cell-test.lisp --- lifecycle, the prediction, the observation, the two hooks, /case.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring over a real store under the
;;;; scratch home: that a start installs two tools, /case, the manual and the
;;;; two hooks and a stop takes every one back; that a case opens, commits a
;;;; prediction before a check, refuses a second pending one, compares the
;;;; observation and marks the claim stale on a mismatch; that the committed
;;;; tool's own result is kept for that comparison; that the running case
;;;; reaches the model at its next request and nobody else's; that /case
;;;; answers the session's case; that a call missing a required member is
;;;; refused; and that a store directory that cannot be made refuses by name.

(in-package #:nodecode.test)

;;; --- lifecycle ---------------------------------------------------------------------

(define-cell-lifecycle-tests "epistemic"
  (:hooks :round :tool)
  (:help :epistemic)
  (:command "case")
  (:running (is (nle::find-tool "epistemic_case") "the case tool is registered")
            (is (nle::find-tool "epistemic_probe") "and the probe tool")
            (is (equal "epistemic" nodecode-epistemic::*dir*) "the section's dir is what it runs on"))
  (:stopped (is (null (nle::find-tool "epistemic_case")) "stop unregisters the case tool")
            (is (null (nle::find-tool "epistemic_probe")) "and the probe tool"))
  (:refused ("remind_chars" 20) ("observe_chars" 5) ("keep" 0)))

;;; --- the case, the prediction, the observation ---------------------------------------

(deftest epistemic-cell-a-case-records-a-prediction-and-what-the-check-showed ()
  (with-epistemic ()
    (let ((opened (open-a-case)))
      (is (ok-p opened) "a case opens")
      (is (search "cfg-change" (answer-text opened)) "and the receipt names it")
      (let ((committed (as-session ("s1")
                          (probe-answer "operation" "commit" "tool_name" "read_file"
                                        "arguments" "log.txt"
                                        "predicted" (vector "the first error is after the reload")
                                        "would_change" "an error before any reload"
                                        "claim_id" "C1"))))
        (is (ok-p committed) "a prediction commits before the check")
        (is (equal "q1" (gethash "check" committed)) "as the case's first check")
        (let ((second (as-session ("s1")
                        (probe-answer "operation" "commit" "tool_name" "sh"
                                      "predicted" (vector "something")))))
          (is (null (ok-p second)) "a second one is refused while the first is pending")
          (is (search "still pending" (answer-text second)) "in words naming the first"))
        (let ((compared (as-session ("s1")
                          (probe-answer "operation" "compare" "disposition" "mismatch"
                                        "belief_change" "changed"
                                        "rationale" "the first error is before the reload"
                                        "evidence" "log.txt line 12 is earlier than the reload"
                                        "source" "log.txt"))))
          (is (ok-p compared) "the observation compares")
          (is (search "stale" (answer-text compared))
              "and the claim the check was bound to is marked stale")
          (is (search "revise" (gethash "next" compared)) "with the next step named"))
        (let ((revised (as-session ("s1")
                         (case-answer "operation" "revise" "claim_id" "C1"
                                      "claim_status" "refuted"
                                      "reason" "the error precedes the reload"))))
          (is (ok-p revised) "a revision addresses the mismatch")
          (is (null (search "stale" (answer-text revised))) "and clears the stale mark"))
        (let ((closed (as-session ("s1")
                        (case-answer "operation" "close" "outcome" "refuted"
                                     "summary" "The log contradicts the reload hypothesis."))))
          (is (ok-p closed) "the case closes")
          (is (equal "closed" (gethash "status" (gethash "case" closed))) "as closed")
          (is (null (nodecode-epistemic::session-case "s1"))
              "and stops being the session's running case"))))))

(deftest epistemic-cell-a-close-waits-for-the-pending-check ()
  (with-epistemic ()
    (open-a-case)
    (as-session ("s1")
      (probe-answer "operation" "commit" "tool_name" "read_file"
                    "predicted" (vector "the first error is after the reload"))
      (let ((answer (case-answer "operation" "close" "outcome" "supported"
                                 "summary" "It held.")))
        (is (null (ok-p answer)) "a case with a pending check does not close")
        (is (search "pending check" (answer-text answer)) "and says why")))))

(deftest epistemic-cell-a-call-missing-a-required-member-is-refused ()
  (with-epistemic ()
    (is (signals-error nlk:contract-refusal
          (nlk:contract-decode (nlk:contract 'nodecode-epistemic::case-request)
                               (nlk:make-json-object "case_id" "x")))
        "the operation is required")
    (is (signals-error nlk:contract-refusal
          (nlk:contract-decode (nlk:contract 'nodecode-epistemic::probe-request)
                               (nlk:make-json-object "case_id" "x")))
        "of both tools")))

;;; --- the two hooks ------------------------------------------------------------------

(deftest epistemic-cell-the-committed-check-keeps-its-own-result ()
  (with-epistemic ()
    (open-a-case)
    (as-session ("s1")
      (probe-answer "operation" "commit" "tool_name" "read_file"
                    "predicted" (vector "the first error is after the reload")))
    (let ((ran nil))
      (as-session ("s1")
        (funcall (cell-hook "nodecode-epistemic" :tool)
                 (list :name "read_file" :arguments (nlk:make-json-object "path" "log.txt")
                       :call-id "c1")
                 (lambda (op) (declare (ignore op)) (setf ran t)
                   "04:12 reloading config")))
      (is ran "the call runs through the hook")
      (let ((shown (as-session ("s1") (case-answer "operation" "show")))
            (probes (gethash "probes" (gethash "case" (as-session ("s1")
                                                         (case-answer "operation" "show"))))))
        (is (ok-p shown) "the case still answers")
        (is (search "04:12" (gethash "observed" (aref probes 0)))
            "and the tool's own result is what the comparison is against")))))

(deftest epistemic-cell-the-running-case-reaches-the-model-and-nobody-else ()
  (with-epistemic ()
    (open-a-case)
    (flet ((ask (session)
             (with-turn-prompts (seen)
               (funcall (cell-hook "nodecode-epistemic" :round)
                        (round-op :session session) #'identity))))
      (let ((mine (ask "s1"))
            (theirs (ask "s2")))
      (is (= 1 (length mine)) "one reminder at the session's next request")
      (is (search "cfg-change" (first mine)) "carrying the case")
      (is (search "stop when" (string-downcase (first mine))) "and its stopping condition")
        (is (null theirs) "a session with no case is told nothing")))))

;;; --- /case --------------------------------------------------------------------------

(deftest epistemic-cell-the-command-answers-the-sessions-case ()
  (with-epistemic ()
    (is (search "No case" (nodecode-epistemic::run-slash "" "s1")) "nothing yet, said plainly")
    (open-a-case)
    (let ((text (nodecode-epistemic::run-slash "" "s1")))
      (is (search "cfg-change" text) "/case answers this session's case")
      (is (search "cfg-change" (nodecode-epistemic::run-slash "list" "s2")) "and lists it to anyone"))))

;;; --- a directory that cannot be made -------------------------------------------------

(deftest epistemic-cell-a-store-that-cannot-be-made-refuses-by-name ()
  (let ((blocked (nlk:home "epistemic-blocked")))
    (ensure-directories-exist (nlk:home "seed/x"))
    (with-open-file (stream blocked :direction :output :if-exists :supersede
                                     :if-does-not-exist :create)
      (write-string "not a directory" stream))
    (is (signals-error nlk:config-refusal (epistemic-start "dir" "epistemic-blocked"))
        "a dir that is a file refuses")
    (is (search "dir" (refusal-text nlk:config-refusal
                                    (epistemic-start "dir" "epistemic-blocked")))
        "naming the setting")))

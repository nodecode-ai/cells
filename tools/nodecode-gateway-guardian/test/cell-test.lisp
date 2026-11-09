;;;; cell-test.lisp --- the three states, the counts, the plan, the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring over real log files: that a
;;;; clean log reads OK and a 409 storm BLOCKED with its conflicts counted,
;;;; that only the tail the section names is read, that a repeated freeze is
;;;; DEGRADED and one warning is not BLOCKED, that the recovery verb prints
;;;; the operator's checklist, never runs it and records the intent, that an
;;;; unknown action and an unreadable home refuse in words, and that
;;;; /gateway-guardian answers the one line.

(in-package #:nodecode.test)

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "gateway-guardian"
  (:fixture with-log-files)
  (:config "logs" (vector (write-log "gateway.log" '("gateway started" "polling ok"))))
  (:help :gateway-guardian)
  (:command "gateway-guardian")
  (:running
   (is (= 400 (nodecode-gateway-guardian::setting :tail-lines)) "tail_lines defaults to 400")
   (is (= 262144 (nodecode-gateway-guardian::setting :tail-bytes)) "a quarter-megabyte tail")
   (is (null (nodecode-gateway-guardian::setting :watch-processes)) "the process scan is off here")
   (is (equal "OK" (gethash "state" (answer (gateway-guardian:status)))) "a clean log reads OK"))
  (:stopped
   (is (null nodecode-gateway-guardian::*gateway-guardian*) "the settings are gone"))
  (:refused ("tail_lines" 0) ("tail_bytes" 100))
  (:idle gateway-guardian:gateway-guardian-error
         (gateway-guardian:status) (gateway-guardian:recover "status")))

;;; --- the three states ------------------------------------------------------------

(deftest gateway-guardian-cell-a-clean-log-reads-ok (with-log-files)
  (with-cell-stop ((gateway-guardian-start
                    "logs" (vector (write-log "gateway.log"
                                               '("gateway started"
                                                 "polling ok"
                                                 "message delivered")))))
    (let ((report (answer (gateway-guardian:status))))
      (is (equal "OK" (gethash "state" report)))
      (is (equal "no gateway fault pattern in recent logs" (gethash "cause" report)) "the cause says what was looked for")
      (is (search "gateway.log"
                  (format nil "~{~a~}" (coerce (gethash "log_files_scanned" report) 'list))) "and names the file it read")
      (is (nth-value 1 (gethash "note" report)) "the note says it is read-only"))))

(deftest gateway-guardian-cell-a-409-storm-is-blocked (with-log-files)
  (with-cell-stop ((gateway-guardian-start
                    "logs" (vector (write-log "gateway.log"
                                               '("409 Conflict: terminated by other getUpdates request"
                                                 "409 Conflict"
                                                 "terminated by other getUpdates request")))))
    (let* ((report (answer (call-tool "gateway_guardian_status")))
           (counts (gethash "counts" report)))
      (is-present (counts (gethash "counts" report)) "the answer carries the counts"
        (is (equal "BLOCKED" (gethash "state" report)) "two pollers on one token read BLOCKED")
        (is (search "409" (gethash "cause" report)) "with the conflict named as the cause")
        (is (>= (gethash "conflict_409" counts) 3) "and every conflict counted")
        (is (zerop (gethash "missed_delivery" counts)) "while a fault it did not see stays zero")))))

(deftest gateway-guardian-cell-one-warning-is-degraded-not-blocked (with-log-files)
  (with-cell-stop ((gateway-guardian-start
                    "logs" (vector (write-log "gateway.log"
                                               '("gateway frozen" "gateway unresponsive")))))
    (let ((report (answer (gateway-guardian:status))))
      (is (equal "DEGRADED" (gethash "state" report)) "two warnings are a warning, not a block")
      (is (equal "gateway warnings in recent logs" (gethash "cause" report))))))

(deftest gateway-guardian-cell-only-the-tail-is-read (with-log-files)
  (with-cell-stop ((gateway-guardian-start
                    "logs" (vector (write-log "gateway.log"
                                               (append (list "409 Conflict" "409 Conflict"
                                                             "409 Conflict")
                                                       (loop repeat 40 collect "delivery sent"))))))
    (is (equal "OK" (gethash "state" (answer (call-tool "gateway_guardian_status"
                                                           "tail_lines" 10)))) "a storm scrolled out of the tail no longer counts")
    (is (equal "BLOCKED" (gethash "state" (answer (call-tool "gateway_guardian_status"
                                                               "tail_lines" 2000)))) "and the same file still carries it in full")))

;;; --- the recovery plan -----------------------------------------------------------

(deftest gateway-guardian-cell-recover-prints-the-plan-and-records-it (with-log-files)
  (with-cell-stop ((gateway-guardian-start
                    "logs" (vector (write-log "gateway.log" '("gateway started")))))
    (let* ((out (answer (call-tool "gateway_guardian_recover"
                                    "action" "clear-stale-lock")))
           (plan (coerce (gethash "operator_must_approve_and_run" out) 'list))
           (logged (gethash "intent_logged" out)))
      (is (nth-value 1 (gethash "executes" out)) "the answer carries `executes'")
      (is (null (gethash "executes" out)) "and it is false: the cell runs nothing")
      (is (search "nodecode gateway restart" (format nil "~{~a~^ | ~}" plan)) "the operator's own command is in the checklist")
      (is-present (path (probe-file logged)) "the intent landed in a file"
        (is (search "clear-stale-lock" (uiop:read-file-string path)) "under the action that was chosen")))))

(deftest gateway-guardian-cell-an-unknown-action-is-refused (with-log-files)
  (with-cell-stop ((gateway-guardian-start
                    "logs" (vector (write-log "gateway.log" '("gateway started")))))
    (let ((out (answer (gateway-guardian:recover "explode"))))
      (is (null (gethash "ok" out)) "an action the cell does not know is ok: false")
      (is (member "rebind-telegram" (coerce (gethash "allowed" out) 'list) :test #'equal) "and the answer lists the ones it does"))))

(deftest gateway-guardian-cell-a-home-with-no-log-refuses (with-log-files)
  (with-cell-stop ((gateway-guardian-start))
    (is (signals-error gateway-guardian:gateway-guardian-error (gateway-guardian:status)))
    (is (search "no gateway log"
                (refusal-text gateway-guardian:gateway-guardian-error
                              (gateway-guardian:status))) "the refusal names the setting to answer")))

;;; --- the slash command -----------------------------------------------------------

(deftest gateway-guardian-cell-the-command-answers-in-one-line (with-log-files)
  (with-cell-stop ((gateway-guardian-start
                    "logs" (vector (write-log "gateway.log"
                                               '("gateway started" "polling ok")))))
    (let ((line (cell-entry "nodecode-gateway-guardian" "gateway-guardian" "")))
      (is (search "OK: no gateway fault pattern" line) "the command answers the state and cause"))
    (let ((plan (cell-entry "nodecode-gateway-guardian" "gateway-guardian"
                            "recover restart-gateway")))
      (is (search "nodecode gateway restart" plan) "and prints a checklist when asked"))))

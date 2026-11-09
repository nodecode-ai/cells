;;;; cell-test.lisp --- the governor's lifecycle, its signals, its packet.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every seam the cell reads is funcalled by hand here -- a tool call, a
;;;; round, a provider call that failed -- so the whole slice runs offline:
;;;; no provider is dialled and no file outside the scratch home is written.

(in-package #:nodecode.test)

;;; --- lifecycle -------------------------------------------------------------

(define-cell-lifecycle-tests "escalation-governor"
  (:hooks :tool :round 'nodecode.evolved::call-provider)
  (:help :escalation-governor)
  (:command "escalation-governor")
  (:running (is (equal nodecode-escalation-governor::+primer+ (nle:help :escalation-governor)))
            (is (= 3 (nodecode-escalation-governor::setting :consecutive-errors)) "consecutive_errors is read"))
  (:refused ("consecutive_errors" "many")))

;;; --- the signals ------------------------------------------------------------

(deftest escalation-governor-cell-trips-on-three-errors
  (with-cell-stop ((escalation-governor-start))
    (with-quiet-governor
      (is (not (search "ESCALATION GOVERNOR"
                       (governor-call "eval" (governor-arguments "code" "(boom)") :failed t))) "the first error is no escalation")
      (is (not (search "ESCALATION GOVERNOR"
                       (governor-call "eval" (governor-arguments "code" "(boom)x") :failed t))) "nor the second")
      (let ((third (governor-call "eval" (governor-arguments "code" "(boom)2") :failed t)))
        (is (search "[ESCALATION GOVERNOR]" third) "the third error carries the packet, mid-turn")
        (is (search "3 consecutive tool errors" third) "which says why")
        (is (search "eval -> ERROR: ok" third) "and lists the calls already tried, and how each ended")
        (is (search "Escalate now" third) "and what to do")
        (is (search "start from this packet" third) "and that the next attempt starts from the packet"))
      (is (getf (governor-state) :tripped) "the session is tripped")
      (is (= 1 (getf (governor-state) :escalations)) "once")
      (is (search "escalation governor" (first (first *notices*))) "and the operator is told too"))))

(deftest escalation-governor-cell-a-success-clears-the-streak
  (with-cell-stop ((escalation-governor-start))
    (with-quiet-governor
      (governor-call "eval" (governor-arguments "code" "(a)") :failed t)
      (governor-call "eval" (governor-arguments "code" "(b)") :failed t)
      (governor-call "eval" (governor-arguments "code" "(c)"))
      (is (zerop (getf (governor-state) :errors)) "a successful result resets the errors")
      (is (not (search "ESCALATION GOVERNOR"
                       (governor-call "eval" (governor-arguments "code" "(d)") :failed t))) "so the next failure is the first again")
      (is (not (search "ESCALATION GOVERNOR"
                       (governor-call "eval" (governor-arguments "code" "(e)") :failed t))))
      (is (not (getf (governor-state) :tripped)) "two failures in a row do not trip"))))

(deftest escalation-governor-cell-the-same-call-repeated-trips
  (with-cell-stop ((escalation-governor-start "consecutive_errors" 0))
    (with-quiet-governor
      (let ((arguments (governor-arguments "path" "src/x.lisp")))
        (dotimes (i 3)
          (governor-call "read" arguments :answer "no such file" :failed t))
        (is (getf (governor-state) :tripped) "the same call three times trips")
        (is (search "repeated 3 times" (getf (governor-state) :reason)))))))

(deftest escalation-governor-cell-provider-errors-count
  (with-cell-stop ((escalation-governor-start))
    (with-quiet-governor
      (dotimes (i 2) (ignore-errors (governor-provider-error)))
      (is (not (getf (governor-state) :tripped)) "two provider errors do not trip")
      (ignore-errors (governor-provider-error))
      (is (getf (governor-state) :tripped) "the third does")
      (is (search "provider errors" (getf (governor-state) :reason))))))

(deftest escalation-governor-cell-a-round-injects-once
  (with-cell-stop ((escalation-governor-start "no_progress_rounds" 3))
    (let ((recorded '()))
      (with-quiet-governor
        (with-stubbed-fdefinitions ((nlk:record-turn-context-prompt (turn &key text)
                                      (push (list turn text) recorded)
                                      nil))
          (dotimes (i 3)
            (governor-round :round (1+ i)))
          (is (= 1 (length recorded)) "one packet, at the round that tripped")
          (is (equal "t1" (first (first recorded))) "recorded into the round that began")
          (is (search "3 model rounds" (second (first recorded))) "which says why")
          (is (search "Goal:" (second (first recorded))) "and leaves a line for the goal")
          (let ((result (governor-call "eval" (governor-arguments "code" "(x)"))))
            (is (not (search "ESCALATION GOVERNOR" result)) "a result in the same round is not stamped twice"))
          (is (= 1 (length recorded)) "and no second packet in one round"))
        (is (search "(the goal, as the conversation states it)"
                    (second (first recorded))) "with no goal given, the packet says where to find it")))))

;;; --- /escalation-governor ---------------------------------------------------

(deftest escalation-governor-cell-command-answers-and-resets
  (with-cell-stop ((escalation-governor-start))
    (with-quiet-governor
      (let ((status (cell-entry "nodecode-escalation-governor" "escalation-governor" "" nil)))
        (is (search "consecutive tool errors" status) "the status names the signals")
        (is (search "not one of its verbs"
                    (cell-entry "nodecode-escalation-governor" "escalation-governor" "bogus" nil)) "an unknown verb says so"))
      (is (search "make the tests pass"
                  (cell-entry "nodecode-escalation-governor" "escalation-governor"
                              "goal make the tests pass" "s1")) "the goal is taken from the operator")
      (is (equal "make the tests pass" (getf (governor-state "s1") :goal)))
      (is (stringp (cell-entry "nodecode-escalation-governor" "escalation-governor" "report" nil)) "the report answers")
      (is (search "forgot 1 session"
                  (cell-entry "nodecode-escalation-governor" "escalation-governor" "reset" nil)) "reset forgets what it tracked")
      (is (null (governor-state "s1")) "and the record is gone"))))

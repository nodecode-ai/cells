;;;; cell-test.lisp --- the four rules, the two tools, the command, the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the policy the original plugin enforced: a send to
;;;; someone who has not consented blocks, an opt-out blocks whatever else is
;;;; recorded, quiet hours are the recipient's own time, an unanswered thread
;;;; and unapproved wording ask rather than block, a stored variable fills a
;;;; template, a minimum gap holds a thread, the record tool closes one,
;;;; /gatekeeper is the owner's surface, and a section that does not answer
;;;; refuses the verb that needs it.

(in-package #:nodecode.test)

;;; --- lifecycle ------------------------------------------------------------------

(define-cell-lifecycle-tests "gatekeeper"
  (:config "owner_offset" "+00:00")
  (:help :gatekeeper)
  (:command "gatekeeper")
  (:running
   (is-present (entry (nle::find-tool "gatekeeper_check")) "the check tool is registered")
   (is-present (entry (nle::find-tool "gatekeeper_record")) "the record tool is registered")
   (is (equal "+00:00" (nodecode-gatekeeper:setting :owner-offset)) "the section's own offset")
   (is (equal "08:00" (nodecode-gatekeeper:setting :start)) "the window opens at eight")
   (is (equal "21:00" (nodecode-gatekeeper:setting :end)) "and closes at nine at night"))
  (:stopped
   (is (null (nle::find-tool "gatekeeper_check")) "and the tools come off"))
  (:refused ("owner_offset" "+00:00" "min_gap_seconds" -1)))

;;; --- the block rules ------------------------------------------------------------

(deftest gatekeeper-cell-a-send-with-no-consent-is-blocked ()
  (with-gatekeeper ()
    (is-carrying (answer (check-call +routine+)) "BLOCK" "no_consent")
    (consent-command)
    (is-carrying (answer (check-call +routine+)) "PASS" "routine")))

(deftest gatekeeper-cell-an-opt-out-is-absolute ()
  (with-gatekeeper ()
    (consent-command)
    (is-carrying (answer (check-call +routine+)) "PASS")
    (is-carrying (answer (run-command "gatekeeper" (format nil "optout ~a" +recipient+)))
      "opt-out recorded")
    ;; a consent that still reads, and an approval, cannot override it
    (consent-command)
    (is-carrying (answer (check-call +routine+)) "BLOCK" "suppressed")))

(deftest gatekeeper-cell-quiet-hours-are-the-recipients-own-time ()
  (with-gatekeeper ()
    (consent-command)
    ;; the verb, called on this thread, so the test's clock is the one it reads
    (let ((nodecode-gatekeeper::*now* +night+))
      (is-carrying (answer (nodecode-gatekeeper:check "sms" +recipient+ +routine+))
        "BLOCK" "quiet_hours"))
    (is-carrying (answer (check-call +routine+)) "PASS")))

;;; --- the rules that ask ---------------------------------------------------------

(deftest gatekeeper-cell-an-unanswered-thread-asks-and-a-reply-clears-it ()
  (with-gatekeeper ()
    (consent-command)
    (is-carrying (answer (check-call +routine+ :thread "t1")) "PASS")
    (is-carrying (answer (check-call +routine+ :thread "t1")) "APPROVE" "unanswered")
    (is-carrying (answer (mark-call "reply" :thread "t1")) "reply noted")
    (is-carrying (answer (check-call +routine+ :thread "t1")) "PASS")))

(deftest gatekeeper-cell-wording-that-matches-no-template-asks ()
  (with-gatekeeper ()
    (consent-command)
    (is-carrying (answer (check-call "something else entirely"))
      "APPROVE" "unapproved_wording")))

(deftest gatekeeper-cell-a-template-is-filled-from-the-owners-variables ()
  (with-gatekeeper ("owner_offset" "+00:00"
                    "templates" #("Hello $name, the build finished."))
    (consent-command "email" "ada@example.test")
    (is-carrying (answer (run-command "gatekeeper"
                                       "variables email ada@example.test name=Ada"))
      "1 owner variable stored")
    (is-carrying (answer (check-call "Hello Ada, the build finished."
                                     :channel "email" :recipient "ada@example.test"))
      "PASS")
    (is-carrying (answer (check-call "Hello Bob, the build finished."
                                     :channel "email" :recipient "ada@example.test"))
      "APPROVE" "unapproved_wording")))

(deftest gatekeeper-cell-the-minimum-gap-holds-a-second-send ()
  (with-gatekeeper ("owner_offset" "+00:00" "min_gap_seconds" 3600)
    (consent-command)
    (is-carrying (answer (check-call +routine+)) "PASS")
    (is-carrying (answer (mark-call "reply")) "reply noted")
    (is-carrying (answer (check-call +routine+)) "APPROVE" "minimum_gap")))

;;; --- the owner's surface --------------------------------------------------------

(deftest gatekeeper-cell-the-log-says-what-was-decided ()
  (with-gatekeeper ()
    (check-call +routine+)
    (is-carrying (answer (run-command "gatekeeper" "log 5")) "sms" "no_consent" "block")))

(deftest gatekeeper-cell-the-status-line-counts-the-ledger ()
  (with-gatekeeper ()
    (consent-command)
    (let ((answer (run-command "gatekeeper" "")))
      (is (search "1 contact" answer) answer)
      (is (search "0 suppressed" answer) answer))))

;;; --- a section that does not answer --------------------------------------------

(deftest gatekeeper-cell-a-missing-setting-refuses-the-verb-that-needs-it ()
  ;; owner_offset is the required member; a section without one still starts,
  ;; and every consent record needs it, so the command says so.
  (with-gatekeeper ("templates" #("Routine notice: the build finished."))
    (is-carrying (answer (run-command "gatekeeper"
                                       (format nil "consent sms ~a signed" +recipient+)))
      "UTC offset")))

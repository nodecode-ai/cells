;;;; cell-test.lisp --- the gate, the score, the proposals, the refusals, the diary.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here: that the cell starts and stops cleanly and refuses a
;;;; section it cannot run; that two days make a candidate and one day does
;;;; not; that a fact already in memory is not proposed; that stale memories,
;;;; near-duplicates and a filling budget are reported; that a rejection
;;;; closes a proposal; that a call without its required member never reaches
;;;; the body; and that /dream answers the pass and the diary.
;;;;
;;;; The one seam that reaches outside is the memory the pass reads: a test
;;;; binds *MEMORY-SOURCE* to a canned list, so nothing here touches a real
;;;; knowledge cell, a network or the operator's home.

(in-package #:nodecode.test)

(define-test-slice "dreaming" "DREAMING-CELL-" :start nodecode-dreaming:start-cell)

(defvar *scratch* 0
  "How many scratch state folders this run made; each test gets its own.")

(defun scratch-dir ()
  "A fresh state folder for one start, under the run's own home."
  (let ((path (uiop:ensure-directory-pathname
               (nlk:home (format nil "dreaming-tests/~d/" (incf *scratch*))))))
    (ensure-directories-exist path)
    (namestring path)))

(defun standing-memory (&rest rows)
  "What *MEMORY-SOURCE* answers: ROWS, each (:name :text :type :written)."
  (lambda () rows))

(defun call-tool (name arguments)
  "NAME called through the wire, as a provider sends it; => the result text."
  (princ-to-string (nle::message-content (execute-wire-call "call-1" name arguments))))

(defparameter +filler+
  (format nil "~{~a~^ ~}" '("the handbook says a claim is checked by two people before it is paid"
                          "and every claim carries its own reference number and the day it was spent"))
  "Words long enough to make memory look full, the same on both entries.")

(defun day-ago (days)
  (dreaming::today (- (get-universal-time) (* days 86400))))

;;; --- lifecycle --------------------------------------------------------------------

(define-cell-lifecycle-tests "dreaming"
  (:config "state_dir" (scratch-dir))
  (:help :dreaming)
  (:command "dream")
  (:running
   (dolist (name '("dream_sight" "dream_report" "dream_reject"))
     (is (nle::find-tool name) (format nil "the ~a tool is registered" name)))
   (is (= 2 (nodecode-dreaming::setting :min-days)) "two separate days by default")
   (is (= 3 (nodecode-dreaming::setting :hour)) "the night runs at three"))
  (:stopped
   (dolist (name '("dream_sight" "dream_report" "dream_reject"))
     (is (null (nle::find-tool name)) (format nil "stop unregisters ~a" name))))
  (:refused
   ("min_days" 1)
   ("budget_chars" 10)))

;;; --- the gate ---------------------------------------------------------------------

(deftest dreaming-cell-one-day-waits-and-two-days-make-a-candidate ()
  (with-cell-stop ((dreaming-start "state_dir" (scratch-dir)))
    (let ((nodecode-dreaming::*memory-source* (standing-memory)))
      (let ((first (call-tool "dream_sight"
                               (format nil "{\"text\":\"the operator runs on thursdays\",\"day\":\"~a\"}"
                                       (day-ago 7)))))
        (is (search "waiting" first) "one day is not a fact yet")
        (is (search "one day only" first)))
      (let ((second (call-tool "dream_sight"
                                (format nil "{\"text\":\"The operator runs on Thursdays.\",\"day\":\"~a\"}"
                                        (day-ago 1)))))
        (is (search "corroborated" second) "the second day passes the gate")
        (is (search "2 days" second) "and the answer counts the days, not the wording"))
      (let ((report (call-tool "dream_report" "{}")))
        (is (search "ready -- said on separate days" report))
        (is (search "The operator runs on Thursdays." report) "the proposal carries the person's words, not a paraphrase")
        (is (search "score 0." report) "and carries why it is a candidate"))
      (let ((again (call-tool "dream_report" "{}")))
        (is (not (search "ready -- said on separate days" again)) "a proposal answered once waits out its cooldown")))))

(deftest dreaming-cell-a-fact-already-in-memory-is-not-proposed ()
  (with-cell-stop ((dreaming-start "state_dir" (scratch-dir)))
    (let ((nodecode-dreaming::*memory-source*
            (standing-memory (list :name "runs-thursdays"
                                   :text "the operator runs on thursdays"
                                   :type "feedback"
                                   :written (day-ago 2)))))
      (call-tool "dream_sight" (format nil "{\"text\":\"the operator runs on thursdays\",\"day\":\"~a\"}" (day-ago 6)))
      (call-tool "dream_sight" (format nil "{\"text\":\"the operator runs on thursdays\",\"day\":\"~a\"}" (day-ago 1)))
      (let ((report (call-tool "dream_report" "{}")))
        (is (search "already in memory" report))
        (is (search "runs-thursdays" report) "and names the memory that already says it")
        (is (not (search "ready -- said on separate days" report)))))))

;;; --- what the night reports -------------------------------------------------------

(deftest dreaming-cell-stale-near-duplicates-and-a-filling-budget-are-reported ()
  (with-cell-stop ((dreaming-start "state_dir" (scratch-dir)
                                   "forget_days" 30
                                   "budget_chars" 1000))
    (let ((nodecode-dreaming::*memory-source*
            (standing-memory
             (list :name "receipts-one"
                   :text (format nil "receipts are filed on friday by the accountant ~a ~a ~a ~a"
                                +filler+ +filler+ +filler+ +filler+)
                   :type "user" :written (day-ago 200))
             (list :name "receipts-two"
                   :text (format nil "receipts are filed on friday with the accountant ~a ~a ~a ~a"
                                +filler+ +filler+ +filler+ +filler+)
                   :type "user" :written (day-ago 3)))))
      (let ((report (call-tool "dream_report" "{}")))
        (is (search "still relevant?" report) "an old memory is raised as a question, not deleted")
        (is (search "receipts-one" report))
        (is (search "near-duplicates in memory" report))
        (is (search "receipts-one / receipts-two" report) "the pair is named")
        (is (search "memory is filling" report) "a full budget says so")))))

;;; --- a rejection ------------------------------------------------------------------

(deftest dreaming-cell-a-rejection-closes-a-proposal-for-good ()
  (with-cell-stop ((dreaming-start "state_dir" (scratch-dir)))
    (let ((nodecode-dreaming::*memory-source* (standing-memory)))
      (call-tool "dream_sight" (format nil "{\"text\":\"the operator prefers tea\",\"day\":\"~a\"}" (day-ago 9)))
      (call-tool "dream_sight" (format nil "{\"text\":\"the operator prefers tea\",\"day\":\"~a\"}" (day-ago 2)))
      (is (search "ready" (call-tool "dream_report" "{}")))
      (let ((closed (call-tool "dream_reject" "{\"text\":\"the operator prefers tea\",\"reason\":\"said once, about someone else\"}")))
        (is (search "not proposed again" closed)))
      (let ((report (call-tool "dream_report" "{}")))
        (is (not (search "the operator prefers tea" report)) "a closed proposal is gone from the pass")
        (is (search "1 rejected" report) "and counted")))))

;;; --- the boundary and the command --------------------------------------------------

(deftest dreaming-cell-a-call-without-its-required-member-is-refused ()
  (with-cell-stop ((dreaming-start "state_dir" (scratch-dir)))
    (let ((text (call-tool "dream_sight" "{}")))
      (is (search "text" text) "the refusal names the missing member"))
    (is (search "not a day"
                (handler-case (nodecode-dreaming:sight "a fact" "user" "the 9th")
                  (nodecode-dreaming:dreaming-error (condition) (princ-to-string condition)))))))

(deftest dreaming-cell-the-command-runs-the-night-and-reads-its-diary ()
  (with-cell-stop ((dreaming-start "state_dir" (scratch-dir)))
    (let ((nodecode-dreaming::*memory-source* (standing-memory)))
      (nodecode-dreaming::run-slash (format nil "sight the build runs on the second machine") "s1")
      (let ((pass (nodecode-dreaming::run-slash "" "s1")))
        (is (search "dreaming: " pass))
        (is (search "waiting" pass) "a sighting made today is waiting"))
      (let ((diary (nodecode-dreaming::run-slash "diary" "s1")))
        (is (search (dreaming::today) diary) "the night is in the log")
        (is (search "proposed" diary)))
      (is (search "not a memory type"
                  (handler-case (nodecode-dreaming:sight "a fact" "nonsense")
                    (nodecode-dreaming:dreaming-error (condition) (princ-to-string condition)))))))
)
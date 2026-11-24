;;;; cell-test.lisp --- lifecycle, the guidance at a turn's start, the modes, the command.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring over the stubbed reminder
;;;; seam: that a start installs the :ROUND hook, the manual topic and
;;;; /minimizer and a stop takes every one back; that a turn's first request
;;;; carries the policy and its later rounds carry nothing; that brief mode
;;;; carries the rung ladder alone; that max_chars cuts what one request
;;;; carries; that /minimizer answers what is being said and previews either
;;;; mode; and that a section of the wrong shape refuses by name.

(in-package #:nodecode.test)

;;; --- lifecycle ---------------------------------------------------------------------

(define-cell-lifecycle-tests "minimizer"
  (:hooks :round)
  (:help :minimizer)
  (:command "minimizer")
  (:running (is (search "Choose the first fitting rung" (nodecode-minimizer::guidance)) "the policy the cell runs on is the original's")
            (is (equal "full" (nodecode-minimizer::setting :mode)) "and the section's own mode is what it runs on"))
  (:stopped (is (null (cell-hook "nodecode-minimizer" :round)) "stop unhooks the round point"))
  (:refused ("mode" "verbose") ("max_chars" 20)))

;;; --- the guidance ------------------------------------------------------------------

(deftest minimizer-cell-the-guidance-lands-at-a-turns-first-request ()
  (with-minimizer ()
    (let ((reminders (with-reminders (collected)
                       (record-round (round-op :round 1))
                       (record-round (round-op :round 2))
                       (record-round (round-op :round 9)))))
      (is (= 1 (length reminders)) "one turn carries one reminder, however many rounds it takes")
      (let ((text (first reminders)))
        (is (search "Minimizer" text) "which is the policy")
        (is (search "Choose the first fitting rung" text) "with its rung ladder")
        (is (search "Preserve safeguards" text) "its safeguards")
        (is (search "Prove completion" text) "and its proof section")))
    (let ((reminders (with-reminders (collected)
                       (record-round (round-op :round 3)))))
      (is (null reminders) "a request that is not a turn's first carries nothing"))))

(deftest minimizer-cell-brief-mode-carries-the-rung-ladder-alone ()
  (with-minimizer ("mode" "brief")
    (let ((text (first (with-reminders (collected) (record-round (round-op))))))
      (is (search "Build the simplest complete system" text) "the opening rides")
      (is (search "Choose the first fitting rung" text) "the ladder rides")
      (is (search "1. Required behavior already exists" text) "whole")
      (is (null (search "Prove completion" text)) "and the sections after it do not"))))

(deftest minimizer-cell-max-chars-cuts-what-one-request-carries ()
  (with-minimizer ("max_chars" 300)
    (let ((text (first (with-reminders (collected) (record-round (round-op))))))
      (is (<= (length text) 300) "the copy one request carries is capped")
      (is (string= "..." (subseq text (- (length text) 3))) "and marked as cut"))))

;;; --- /minimizer --------------------------------------------------------------------

(deftest minimizer-cell-the-command-answers-the-guidance ()
  (with-minimizer ()
    (let ((answer (minimizer-command)))
      (is (search "minimizer: full mode" answer) "bare, it says which mode and how much")
      (is (search "in the first request of every turn" answer) "and when it lands")
      (is (search "Choose the first fitting rung" answer) "and shows the very text of it"))
    (let ((full (minimizer-command "full")))
      (is (search "Prove completion" full) "full previews the whole policy"))
    (let ((brief (minimizer-command "brief")))
      (is (search "Choose the first fitting rung" brief) "brief previews the ladder")
      (is (null (search "Prove completion" brief)) "without the rest"))
    (is (search "no nonsense mode" (minimizer-command "nonsense")) "a word it does not know names the two it does")))

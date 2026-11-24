;;;; cell-test.lisp --- the storm panel against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every model call is a stubbed NLE:COMPLETE, so nothing touches the
;;;; network, no key is read and no provider is dialled: a test pins what a
;;;; round asks each seat, what the judge reads, and what a failure answers.

(in-package #:nodecode.test)

(define-test-slice "storm" "STORM-CELL-" :start nodecode-storm:start-cell)

(define-cell-lifecycle-tests "storm"
  (:help :storm)
  (:command "storm")
  (:refused ("max_tokens" 8) ("lineup" "not-a-list"))
  (:idle nodecode-storm:storm-error
         (storm:panel "a topic" '((:name "operations" :brief "the deploy window")))))

;;; --- fixtures -------------------------------------------------------------------

(defparameter +storm-judge-answer+
  "{\"synthesis\":\"Ship, but keep the rollback warm.\",\"contradictions\":[{\"claim\":\"the window is open\",\"seats_for\":[\"operations\"],\"seats_against\":[\"support\"],\"weight\":\"support holds the on-call queue\"}],\"unresolved\":[\"whether the migration ran\"]}"
  "What the stubbed judge writes back.")

(defmacro with-storm-model ((calls &key (seat-answer "one finding, sourced.") (judge nil))
                            &body body)
  "BODY with NLE:COMPLETE stubbed so no model is called: CALLS is bound to
every call made, oldest first, as (SYSTEM USER MAX-TOKENS FALLBACK-P PIN) --
PIN the auxiliary target the call ran under, NIL for the session's own. A
call whose instruction names the panel judge answers JUDGE, else the judge
fixture; any other answers SEAT-ANSWER."
  `(let ((,calls '()))
     (with-stubbed-fdefinition (nle:complete (system user &key session-id max-tokens fallback-p)
                               (setf ,calls (nconc ,calls
                                                   (list (list system user max-tokens fallback-p
                                                               nle::*auxiliary-model*))))
                               (if (search "panel judge" system)
                                   (or ,judge +storm-judge-answer+)
                                   ,seat-answer))
       ,@body)))

(defun storm-seat (name brief &optional questions)
  "One seat card, as eval writes it."
  (list :name name :brief brief :questions questions))

(defun storm-item (out &rest keys)
  "OUT, decoded, at KEYS, the last of them an array's first element."
  (let ((array (apply #'nlk:json-array out (butlast keys))))
    (and array (aref array (car (last keys))))))

;;; --- a round -------------------------------------------------------------------

(deftest storm-cell-fans-out-every-seat-on-every-model ()
  (with-cell-stop ((storm-start "lineup" (vector "alpha/model-a" "beta/model-b")))
    (with-storm-model (calls)
      (let ((json (storm:panel "Should we ship on Friday?"
                                (list (storm-seat "operations" "the deploy window"
                                                  '("Is the window open?"))
                                      (storm-seat "support" "the on-call queue")))))
        (let* ((out (nlk:decode-json json))
               (reports (nlk:json-array out "panel" "seat_reports")))
          (is (eq t (gethash "success" out)) (gethash "error" out))
          (is (= 4 (length reports)) "two seats on two models")
          (is (= 5 (length calls)) "four seat calls and the judge")
          (is (search "operations" (first (first calls))) "the seat's own name leads its instruction")
          (is (search "the deploy window" (first (first calls))))
          (is (search "Is the window open?" (first (first calls))))
          (is (equal "Topic: Should we ship on Friday?" (second (first calls))))
          (is (equal "alpha" (getf (fifth (first calls)) :provider)) "the pin names the lane")
          (is (equal "model-a" (getf (fifth (first calls)) :model)))
          (is (equal "beta/model-b" (format nil "~a/~a"
                                            (getf (fifth (second calls)) :provider)
                                            (getf (fifth (second calls)) :model))) "the second slot")
          (is (null (fourth (first calls))) "a seat call never falls back to another model")
          (is (search "panel judge" (first (fifth calls))) "the judge is asked last")
          (is (search "beta/model-b" (second (fifth calls))) "every report reaches the judge")
          (is (equal "Ship, but keep the rollback warm."
                     (nlk:json-value out :string "panel" "synthesis")))
          (is (equal "the window is open"
                     (nlk:json-value (storm-item out "panel" "contradictions" 0) :string "claim")))
          (is (equal "whether the migration ran"
                     (storm-item out "panel" "unresolved" 0))))))))

(deftest storm-cell-runs-the-session-model-when-the-lineup-is-empty ()
  (with-cell-stop ((storm-start))
    (with-storm-model (calls)
      (let ((out (nlk:decode-json (storm:panel "One question"
                                                (list (storm-seat "operations" "the deploy window"))))))
        (is (= 2 (length calls)) "one seat and the judge")
        (is (null (fifth (first calls))) "with no pin the session's own model answers")
        (is (equal 4000 (third (first calls))) "the section's ceiling")
        (is (search "session's own model"
                    (nlk:json-value (storm-item out "calls" 0) :string "model")))))))

(deftest storm-cell-judge-prose-still-answers-a-synthesis ()
  (with-cell-stop ((storm-start "judge" "gamma/model-c"))
    (with-storm-model (calls :judge "Ship it; the rollback is warm.")
      (let ((out (nlk:decode-json (storm:panel "Q" (list (storm-seat "operations" "the deploy"))))))
        (is (eq t (gethash "success" out)))
        (is (equal "Ship it; the rollback is warm." (nlk:json-value out :string "panel" "synthesis")))
        (is (equal "gamma/model-c" (format nil "~a/~a"
                                          (getf (fifth (second calls)) :provider)
                                          (getf (fifth (second calls)) :model))) "the judge ran on the model named")))))

(deftest storm-cell-reports-a-failed-seat-without-swapping ()
  (with-cell-stop ((storm-start))
    (let ((seen '()))
      (with-stubbed-fdefinition (nle:complete (system user &key session-id max-tokens fallback-p)
                                (push fallback-p seen)
                                (error "model alpha/model-a refused: no credential"))
        (let ((out (nlk:decode-json (storm:panel "Q" (list (storm-seat "operations" "the deploy"))))))
          (is (null (gethash "success" out)) "the round did not finish")
          (is (search "no credential" (gethash "error" out)))
          (is (search "reachable" (gethash "hint" out)))
          (is (equal '(nil) seen) "and it asked for no fallback"))))))

(deftest storm-cell-refuses-a-seat-with-no-brief ()
  (with-cell-stop ((storm-start))
    (with-storm-model (calls)
      (let ((out (nlk:decode-json (storm:panel "Q" (list (list :name "operations" :brief ""))))))
        (is (null (gethash "success" out)))
        (is (search "needs a name and a brief" (gethash "error" out)))
        (is (null calls) "nothing was asked")))))

(deftest storm-cell-tool-runs-one-round ()
  (with-cell-stop ((storm-start "lineup" (vector "alpha/model-a")))
    (with-storm-model (calls)
      (let* ((entry (nle::find-tool "storm_run_panel"))
             (arguments (cell-json "{\"topic\":\"Should we ship?\",\"seats\":[{\"name\":\"operations\",\"brief\":\"the deploy window\",\"questions\":[\"Is the window open?\"]}]}"))
             (out (nlk:decode-json (funcall (third entry)
                                             (nle::tool-body-arguments entry arguments)))))
        (is (equal 2 (length calls)) "one seat and the judge")
        (is (= 1 (length (nlk:json-array out "panel" "seat_reports"))))
        (is (equal "alpha/model-a" (nlk:json-value (storm-item out "calls" 0) :string "model")))))))

;;; --- the command ----------------------------------------------------------------

(deftest storm-cell-command-names-the-lineup ()
  (with-cell-stop ((storm-start "lineup" (vector "alpha/model-a") "judge" "gamma/model-c"))
    (let ((text (cell-entry "nodecode-storm" "storm" "")))
      (is (search "alpha/model-a" text) text)
      (is (search "gamma/model-c" text) text)
      (is (search "max_tokens 4000" text)))
    (is (search "alpha/model-a" (cell-entry "nodecode-storm" "storm" "status")))
    (is (search "nope is not one of its choices" (cell-entry "nodecode-storm" "storm" "nope")))))

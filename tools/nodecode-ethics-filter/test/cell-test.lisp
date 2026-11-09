;;;; cell-test.lisp --- the brief, the verdict, the veto, the trail and every
;;;; refusal, with no network and no model in sight.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is pinned is what a user gets: a decision with no scores answers the
;;;; worksheet of the modules it actually touches, scores answer a verdict the
;;;; thresholds explain, one red module blocks a decision whose mean would pass,
;;;; every verdict lands in the audit trail as one JSON line, and a call that
;;;; leaves out the decision or a module's score is refused before anything is
;;;; written.

(in-package #:nodecode.test)

(define-test-slice "ethics-filter" "ETHICS-FILTER-" :start nodecode-ethics-filter:start-cell)

(defparameter *decision* "Ship the new line in plastic packaging and move the supplier to a cheaper factory")
(defparameter *legal-decision* "Sign the contract without the safety inspection the standard requires")

;;; --- lifecycle ------------------------------------------------------------------

(define-cell-lifecycle-tests "ethics-filter"
  (:config "audit_path" (namestring (nlk:home "ethics-lifecycle-test/audit.jsonl")))
  (:help :ethics)
  (:command "ethics")
  (:running (is (equal "small-business-ethical"
                       (nodecode-ethics-filter::setting :default-constitution)) "the section's own default constitution")
            (let ((entry (nle::find-tool "ethics_evaluate")))
              (is-present entry "the tool is registered under the original's name"
                (is (equal "ethics_evaluate" (first entry))))))
  (:stopped (is (null (nle::find-tool "ethics_evaluate")) "stop takes the tool back out"))
  (:refused ("default_constitution" "wishful-thinking"))
  (:idle nodecode-ethics-filter:ethics-filter-error
         (nodecode-ethics-filter::run-slash "a decision" nil)))

;;; --- fixtures -------------------------------------------------------------------

(defun ethics-directory ()
  "A folder of this test's own under the scratch home, so no two tests share a trail."
  (merge-pathnames (format nil "ethics-~12,'0x/" (random (expt 16 10))) (nlk:home)))

(defmacro with-ethics (&body body)
  "BODY with the cell started on a trail of its own, taken away afterwards."
  `(let* ((directory (ethics-directory))
          (started (ethics-filter-start
                    "audit_path" (namestring (merge-pathnames "audit.jsonl" directory)))))
     (nlk:with-cleanup ((progn (funcall started)
                               (uiop:delete-directory-tree directory :validate t
                                                            :if-does-not-exist :ignore)))
       ,@body)))

(defun ethics-tool (&rest pairs)
  "The tool as the engine calls it: the declared contract decoded, then the body."
  (let ((entry (nle::find-tool "ethics_evaluate")))
    (funcall (third entry)
             (nlk:contract-decode (second entry) (apply #'nlk:make-json-object pairs)))))

(defun ethics-scores (&rest pairs)
  "PAIRS as the JSON object a call's scores arrive in."
  (apply #'nlk:make-json-object pairs))

(defun ethics-trail ()
  "Every record in this test's audit trail, oldest first."
  (nlk:read-json-lines (nodecode-ethics-filter::audit-file)))

;;; --- the brief, and the modules a decision touches ---------------------------------

(deftest ethics-filter-brief-names-the-modules-that-apply ()
  (with-ethics
    (let ((text (ethics-tool "action" *decision*)))
      (is (search "evaluation brief" text) "no scores answers the worksheet")
      (is (search "environmental" text) "packaging and a supplier bring the environmental module in")
      (is (search "fairness" text) "a factory move affects people, so fairness applies")
      (is (search "0-25" text) "the rubric bands are in the brief, so a score can be anchored")
      (is (search "GREEN" text) "and the thresholds are named")
      (is (null (search "conscious-leadership" text)) "a module the decision does not touch is left out of the brief"))))

(deftest ethics-filter-brief-honours-the-constitution ()
  (with-ethics
    (let ((text (ethics-tool "action" *decision* "constitution" "minimal-safe")))
      (is (search "Minimal Safe Baseline" text) "the preset is named in the brief")
      (is (null (search "conscious-leadership" text)) "its own modules are the ones offered")
      (is (search "GREEN" text) "with its thresholds"))))

;;; --- the verdict -------------------------------------------------------------------

(deftest ethics-filter-scores-answer-a-verdict ()
  (with-ethics
    (let ((text (ethics-tool "action" *decision* "constitution" "minimal-safe"
                              "scores" (ethics-scores "fairness" 90 "transparency" 85)
                              "reasoning" "documented and legal")))
      (is (search "Ethics Filter: GREEN" text) "every module well above the green line passes")
      (is (search "87.5" text) "the mean is the mean of the enabled modules' scores")
      (is (search "Reasoning: documented and legal" text) "the reasoning comes back in the verdict")
      (is (= 1 (length (ethics-trail))) "the verdict is appended to the trail once"))))

(deftest ethics-filter-one-red-module-blocks-a-green-mean ()
  (with-ethics
    (let ((text (ethics-tool "action" *legal-decision* "constitution" "minimal-safe"
                              "scores" (ethics-scores "fairness" 95 "transparency" 95
                                                       "compliance" 40))))
      (is (search "Ethics Filter: RED" text) "compliance below the red line blocks it")
      (is (search "Requires human review." text) "a red verdict needs a person")
      (let ((record (first (ethics-trail))))
        (is (equal "red" (nlk:json-value record :string "decision")) "the trail records the veto")
        (is (equal '("compliance") (coerce (nlk:json-value record :array "flags") 'list)) "and names the module that blocked it")))))

(deftest ethics-filter-tension-between-modules-is-flagged ()
  (with-ethics
    (let ((text (ethics-tool "action" *decision* "constitution" "startup-quick"
                              "scores" (ethics-scores "fairness" 95 "transparency" 92
                                                       "environmental" 40))))
      (is (search "Tension:" text) "a 50-point spread between modules is named")
      (is (search "environmental" text) "and the low module is the one named"))))

(deftest ethics-filter-strictness-overrides-the-preset ()
  (with-ethics
    (let ((text (ethics-tool "action" *decision* "constitution" "startup-quick"
                              "strictness" "strict"
                              "scores" (ethics-scores "fairness" 85 "transparency" 85
                                                       "environmental" 85))))
      (is (search "AMBER" text) "85 is a pass at moderate and a flag at strict"))))

;;; --- refusals ----------------------------------------------------------------------

(deftest ethics-filter-call-without-a-decision-is-refused ()
  (with-ethics
    (let ((contract (second (nle::find-tool "ethics_evaluate"))))
      (is (signals-error nlk:contract-refusal
            (nlk:contract-decode contract (nlk:make-json-object "scores" (ethics-scores)))) "the action is required, so a call without it never reaches the engine")
      (is (null (ethics-trail)) "and nothing was written to the trail"))))

(deftest ethics-filter-a-missing-score-is-refused ()
  (with-ethics
    (is (signals-error nodecode-ethics-filter:ethics-filter-error
          (ethics-tool "action" *decision* "constitution" "minimal-safe"
                       "scores" (ethics-scores "fairness" 90))) "an enabled module with no score is refused, not guessed")
    (is (null (ethics-trail)) "and no half-scored record is kept")))

;;; --- the command ------------------------------------------------------------------

(deftest ethics-filter-command-answers-both-ways ()
  (with-ethics
    (let ((bare (nodecode-ethics-filter::run-slash "" nil))
          (brief (nodecode-ethics-filter::run-slash *decision* nil))
          (verdict (nodecode-ethics-filter::run-slash
                    (format nil "~a --constitution minimal-safe --scores fairness=90,transparency=90,compliance=90"
                            *decision*)
                    nil)))
      (is (search "small-business-ethical" bare) "with no decision it names the presets")
      (is (search "evaluation brief" brief) "a decision alone answers the worksheet")
      (is (search "Ethics Filter: GREEN" verdict) "--scores answers the verdict")
      (is (search "transparency" verdict) "with a line for every enabled module")
      (is (null (search "compliance" verdict)) "and none for a module the preset leaves out")
      (is (= 1 (length (ethics-trail))) "and the trail keeps the verdict"))))

(deftest ethics-filter-command-parses-a-quoted-context ()
  (with-ethics
    (multiple-value-bind (action context constitution strictness scores audit)
        (nodecode-ethics-filter::parse-command-args
         "Hire two engineers --context \"they relay to the one customer who pays\" --constitution startup-quick")
      (is (equal "Hire two engineers" action) "the flags are not part of the decision")
      (is (equal "they relay to the one customer who pays" context) "a quoted context stays whole")
      (is (equal "startup-quick" constitution) "the preset is read")
      (is (null strictness) "and an absent flag stays absent")
      (is (null scores) "with no scores to read")
      (is (null audit) "and no audit asked for"))))

(deftest ethics-filter-audit-command-lists-the-trail ()
  (with-ethics
    (ethics-tool "action" *decision* "constitution" "minimal-safe"
                  "scores" (ethics-scores "fairness" 90 "transparency" 90 "compliance" 90))
    (let ((text (nodecode-ethics-filter::run-slash "--audit" nil)))
      (is (search "audit trail" text) "the command names the file it reads")
      (is (search "GREEN" text) "and lists the verdict that was just kept"))))

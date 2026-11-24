;;;; cell-test.lisp --- the lifecycle, the classification, a pin, the climb.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the routing itself: that a submitted prompt is
;;;; classified and its tier becomes the session's own selection, which is
;;;; the one fact the next turn reads; that a pinned session is not
;;;; classified; that auto clears the pin; that a failed classification
;;;; never fails the turn; that repeated failed tool calls are what unlock
;;;; escalate_model, one tier at a time and no further than high; and that a
;;;; tier with no model refuses to start.

(in-package #:nodecode.test)

(define-test-slice "model-picker" "MODEL-PICKER-" :start nodecode-model-picker:start-cell
  :defaults (("low_model" "quick-model")
             ("default_model" "standard-model")
             ("high_model" "expert-model")))

;;; --- lifecycle -------------------------------------------------------------

(define-cell-lifecycle-tests "model-picker"
  (:hooks 'nle:submit :tool)
  (:help :model-picker)
  (:command "model-picker")
  (:running (is (equal "quick-model" (nodecode-model-picker::tier-model "low")))
            (is (equal "expert-model" (nodecode-model-picker::tier-model "high")))
            (is (member "money-moving transactions" (nodecode-model-picker::tier-policy "high")
                        :test #'equal) "the policy the classifier reads is the section's"))
  (:stopped (is (zerop (hash-table-count nodecode-model-picker::*sessions*)) "the live tiers are forgotten with the cell"))
  (:refused ("escalate_after" 0))
  (:idle nodecode-model-picker::model-picker-error
         (nodecode-model-picker::escalate-model "s1" "the tests keep failing")
         (nodecode-model-picker::run-slash "status" "s1")))

;;; --- helpers ----------------------------------------------------------------

(defmacro with-picker ((&rest pairs) &body body)
  "BODY over a temp store holding session s1 and the cell started on PAIRS."
  `(with-temp-store ()
     (ensure-durable-session "s1")
     (with-cell-stop ((model-picker-start ,@pairs))
       ,@body)))

(defmacro with-classifier ((answer &key (asked (gensym "ASKED"))) &body body)
  "BODY with the classifier call stubbed to ANSWER; ASKED receives the
system prompt it was called with."
  `(let ((,asked nil))
     (with-stubbed-fdefinition (nle:complete (system user &key &allow-other-keys)
                                  (declare (ignore user))
                                  (setf ,asked system)
                                  ,answer)
       ,@body)))

(defun submit (session prompt &rest keys)
  "SUBMIT passed through the cell's advice with a NEXT that admits nothing:
=> what NEXT answered, so a test reads that the prompt still got there."
  (declare (ignore keys))
  (funcall (cell-hook "nodecode-model-picker" 'nle:submit)
           (lambda (&rest args) (declare (ignore args)) :admitted)
           session prompt))

(defun selected (session)
  "(values PROVIDER MODEL): the model SESSION's next turn would run."
  (nlk:session-model-selection session))

(defun submit-tier (session prompt)
  "What the cell routes PROMPT to, read from its own state."
  (nodecode-model-picker::route-turn session prompt)
  (getf (nodecode-model-picker::session-state session) :tier))

;;; --- the classification -----------------------------------------------------

(deftest model-picker-cell-a-submitted-prompt-moves-the-next-turn ()
  (with-picker ()
    (with-classifier ("Expert" :asked asked)
      (is (eq :admitted (submit "s1" "wire up the payment endpoint")) "the prompt still reaches the session")
      (is (search "high -- for" asked) "the prompt is generated from the tier's best_for list")
      (is (search "money-moving transactions" asked) "the section's own words, not a copy in code")
      (multiple-value-bind (provider model) (selected "s1")
        (is (null provider) "with no provider named the session keeps its own")
        (is (equal "expert-model" model) "and the tier's model is what it runs")))))

(deftest model-picker-cell-a-tier-is-kept-unless-the-work-changes ()
  (with-picker ()
    (with-classifier ("low")
      (submit "s1" "what does this function return"))
    (with-classifier ("Standard" :asked asked)
      (submit "s1" "now refactor it")
      (is (search "The previous turn ran on low" asked) "the classifier is told the previous tier")
      (is (equal "standard-model" (nth-value 1 (selected "s1")))))))

(deftest model-picker-cell-a-failed-classification-falls-back-to-the-floor ()
  (with-picker ()
    (with-stubbed-fdefinition (nle:complete (system user &key &allow-other-keys)
                                 (declare (ignore system user))
                                 (error "the classifier's provider is down"))
      (is (equal "low" (submit-tier "s1" "do something expensive")) "a routing failure is a low turn, never a failed one")
      (is (equal "quick-model" (nth-value 1 (selected "s1")))))))

(deftest model-picker-cell-a-slash-line-and-a-steer-are-not-classified ()
  (with-picker ()
    (with-classifier ("high")
      (is (null (nodecode-model-picker::route-turn "s1" "/model-picker low")) "a command routes nothing")
      (is (null (nodecode-model-picker::route-turn "s1" "keep going" :steer t)) "a steer continues the turn already running")
      (is (null (selected "s1")) "so the session was left alone"))))

(deftest model-picker-cell-the-prompt-names-every-tier ()
  (with-picker ()
    (let ((prompt (nodecode-model-picker::classifier-prompt "default")))
      (is (search "low -- for" prompt))
      (is (search "default -- for" prompt))
      (is (search "high -- for" prompt))
      (is (search "Answer with one word" prompt)))
    (is (equal "high" (nodecode-model-picker::parse-tier "High.")))
    (is (equal "low" (nodecode-model-picker::parse-tier "the answer is low, clearly")))
    (is (equal "default" (nodecode-model-picker::parse-tier "Standard")))
    (is (null (nodecode-model-picker::parse-tier "unsure")))
    (is (null (nodecode-model-picker::parse-tier nil)) "no answer names no tier")))

;;; --- the pin ---------------------------------------------------------------

(deftest model-picker-cell-a-pin-wins-over-the-classifier ()
  (with-picker ()
    (is (search "pinned to low" (nodecode-model-picker::run-slash "low" "s1")))
    (is (equal "quick-model" (nth-value 1 (selected "s1"))))
    (with-classifier ("high")
      (is (null (nodecode-model-picker::route-turn "s1" "plan the migration")) "a pinned session is not classified"))
    (is (equal "quick-model" (nth-value 1 (selected "s1"))) "the pin holds")
    (is (search "classifying every prompt again" (nodecode-model-picker::run-slash "auto" "s1")))
    (with-classifier ("high")
      (is (equal "high" (nodecode-model-picker::route-turn "s1" "plan the migration"))))
    (is (equal "expert-model" (nth-value 1 (selected "s1"))))))

(deftest model-picker-cell-a-word-the-command-does-not-know-refuses ()
  (with-picker ()
    (is-carrying (text (refusal-text nodecode-model-picker::model-picker-error
                                     (nodecode-model-picker::run-slash "cheapest" "s1")))
      ("low, default, high, auto"))))

;;; --- the climb -------------------------------------------------------------

(deftest model-picker-cell-escalation-climbs-one-tier-and-stops-at-the-top ()
  (with-picker ()
    (is-carrying (text (nodecode-model-picker::escalate-model "s1" "stuck")) ("0 of 3"))
    (dotimes (i 3) (nodecode-model-picker::note-failure "s1" "ERROR: the build fails"))
    (is (search "escalated from default to high"
                (nodecode-model-picker::escalate-model "s1" "the build keeps failing")) "with no tier run yet the climb starts at default")
    (is (equal "expert-model" (nth-value 1 (selected "s1"))))
    (dotimes (i 3) (nodecode-model-picker::note-failure "s1" "ERROR: still failing"))
    (is (search "already on high" (nodecode-model-picker::escalate-model "s1" "still stuck")))
    (is (equal "expert-model" (nth-value 1 (selected "s1"))) "there is nothing above the top")))

(deftest model-picker-cell-a-working-call-resets-the-failure-count ()
  (with-picker ()
    (dotimes (i 3) (nodecode-model-picker::note-failure "s1" "ERROR: nope"))
    (is (= 3 (getf (nodecode-model-picker::session-state "s1") :failures)))
    (nodecode-model-picker::note-failure "s1" "the tests pass")
    (is (zerop (getf (nodecode-model-picker::session-state "s1") :failures)))))

(deftest model-picker-cell-the-tool-counts-what-the-turn-runs ()
  (with-picker ()
    (let ((op (list :name "sh" :arguments (nlk:json-object) :call-id "c1")))
      (is (equal "ERROR: no such file"
                 (funcall (cell-hook "nodecode-model-picker" :tool)
                          op
                          (lambda (op) (declare (ignore op)) "ERROR: no such file")))))
    (is (= 1 (getf (nodecode-model-picker::session-state nil) :failures)) "a tool result read outside a turn counts under the nil session, never signals")))

;;; --- the settings ----------------------------------------------------------

(deftest model-picker-cell-a-tier-with-no-model-refuses-to-start ()
  (with-temp-store ()
    (ensure-durable-session "s1")
    (is-carrying (text (refusal-text nodecode-model-picker::model-picker-error
                                     (model-picker-start "high_model" "")))
      ("the high tier names no model") ("high_model"))
    (is (null (cell-hook "nodecode-model-picker" 'nle:submit)) "and nothing was installed")))

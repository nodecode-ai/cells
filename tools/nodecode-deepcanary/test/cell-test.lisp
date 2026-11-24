;;;; cell-test.lisp --- the lifecycle, the policy, the gate, the inbox, the tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's wiring and its arithmetic: that a start
;;;; installs the six tools, /deepcanary, the manual topic and the round hook
;;;; and a stop takes every one back; that a missing level, a level outside its
;;;; options, a bad quiet-hours time and a budget below one refuse; that the
;;;; reason table sets the level, that a guess never makes C3 and that a hint of
;;;; zero never interrupts; that quiet hours hold a C2 and never a C3; that the
;;;; hourly budget holds the second C2 and never a C3; that one dedupe key
;;;; bundles into one item; that acknowledge, snooze and mute settle one; that a
;;;; summary naming something private is redacted; that the tool and the command
;;;; answer; and that a bad call says why and writes nothing.

(in-package #:nodecode.test)

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "deepcanary"
  (:config "level" "C2")
  (:hooks :round)
  (:help :deepcanary)
  (:command "deepcanary")
  (:running (is (equal nodecode-deepcanary::+primer+ (nle:help :deepcanary)))
            (is (equal "C2" (nodecode-deepcanary::setting :level)) "the level reads as configured")
            (is (= 30 (nodecode-deepcanary::setting :snooze-minutes)) "a snooze defaults to half an hour"))
  (:refused () ("level" "C9") ("max_interrupts_per_hour" 0) ("quiet_start" "25:00"))
  (:idle deepcanary:deepcanary-error
         (deepcanary:raise "TASK_FAILED" :authority "runtime" :summary "x")
         (deepcanary:inbox)))

;;; --- the policy -----------------------------------------------------------------

(deftest deepcanary-cell-the-reason-table-sets-the-level-and-the-action ()
  (let ((blocking (nodecode-deepcanary::decide "HUMAN_APPROVAL_REQUIRED" "host")))
    (is (equal "C3" (getf blocking :level)) "an approval that blocks the work is C3")
    (is (equal "ESCALATE" (getf blocking :action)) "and escalates"))
  (let ((failure (nodecode-deepcanary::decide "TOOL_FAILURE_LOOP" "runtime")))
    (is (equal "C2" (getf failure :level)) "a tool failing in a loop is C2")
    (is (equal "INTERRUPT" (getf failure :action)) "and interrupts"))
  (let ((routine (nodecode-deepcanary::decide "TASK_COMPLETED" "derived")))
    (is (equal "C1" (getf routine :level)) "a finished task is C1")
    (is (equal "DIGEST" (getf routine :action)) "and stays in the activity")))

(deftest deepcanary-cell-a-guess-never-makes-c3 ()
  (let ((verdict (nodecode-deepcanary::decide "HUMAN_QUESTION_PENDING" "heuristic")))
    (is (equal "C2" (getf verdict :level)) "a guessed wait is not a blocking fact")
    (is (member "authority-caps-at-c2" (getf verdict :rules) :test #'string=) "and says which rule moved it"))
  (let ((verdict (nodecode-deepcanary::decide "TOOL_FAILURE_LOOP" "runtime" :hint 0)))
    (is (equal "DIGEST" (getf verdict :action)) "a hint of zero says it can wait")
    (is (member "hint-zero-waits" (getf verdict :rules) :test #'string=))))

;;; --- the inbox -----------------------------------------------------------------

(deftest deepcanary-cell-a-raised-item-comes-back-out-of-the-file ()
  (with-deepcanary ()
    (is-carrying (text (deepcanary-raise-one "TOOL_FAILURE_LOOP" :summary "the tests keep failing"))
                  ("raised #1 C2 INTERRUPT" "the answer names the item")
                  ("the tests keep failing" "with the words it was given"))
    (is (= 1 (deepcanary-count)) "one item, in the file")
    (is-carrying (text (deepcanary:inbox))
                  ("#1 C2 INTERRUPT · TOOL_FAILURE_LOOP" "the inbox lists it")))
  (is (probe-file (nodecode-deepcanary::inbox-path)) "the file is under the home"))

(deftest deepcanary-cell-one-thing-bundles-instead-of-piling-up ()
  (with-deepcanary ()
    (deepcanary-raise-one "TOOL_FAILURE_LOOP" :dedupe "tests" :summary "first")
    (is-carrying (text (deepcanary-raise-one "TOOL_FAILURE_LOOP" :dedupe "tests" :summary "second"))
                  ("bundled into #1 (2 now)" "the same key bundles into the item there"))
    (is (= 1 (deepcanary-count)) "and adds nothing")))

(deftest deepcanary-cell-the-operator-settles-an-item ()
  (with-deepcanary ()
    (deepcanary-raise-one "TASK_FAILED" :dedupe "a" :summary "the build broke")
    (is-carrying (text (deepcanary:acknowledge 1 :note "fixed it"))
                  ("acknowledged #1 TASK_FAILED" "an answer settles it"))
    (is (= 0 (length (deepcanary-items "open"))) "and it leaves the open inbox")
    (deepcanary-raise-one "TASK_FAILED" :dedupe "b" :summary "the deploy broke")
    (is-carrying (text (deepcanary:snooze 2 :minutes 90)) ("snoozed #2 for 90 minutes" "a snooze keeps it"))
    (is (= 1 (length (deepcanary-items "snoozed"))) "under its own status")
    (is-carrying (text (deepcanary:mute 2)) ("muted #2" "and mute takes it out of the way"))
    (is (string= "muted" (nodecode-deepcanary::item-field (second (deepcanary-items)) :status)))))

(deftest deepcanary-cell-a-private-summary-is-redacted ()
  (with-deepcanary ()
    (deepcanary-raise-one "TASK_FAILED" :summary "the api key was rejected")
    (let ((summary (nodecode-deepcanary::item-field (first (deepcanary-items)) :summary)))
      (is-carrying summary ("a task failed" "the reason's own words stand in") (:absent "api key")))))

;;; --- the gate -------------------------------------------------------------------

(deftest deepcanary-cell-quiet-hours-hold-a-c2-and-never-a-c3 ()
  (with-deepcanary ("quiet_hours" t "quiet_start" "00:00" "quiet_end" "00:00")
    (is (nodecode-deepcanary::quiet-now-p) "equal times are the whole day")
    (is-carrying (text (deepcanary-raise-one "TASK_FAILED" :dedupe "a"))
                  ("held by quiet hours" "a C2 waits for the morning"))
    (is-carrying (text (deepcanary:raise "HUMAN_APPROVAL_REQUIRED" :authority "host" :summary "merge?"))
                  ("the operator was told" "a C3 goes through"))
    (is-present (entry (cell-notice "nodecode-deepcanary")) "the operator was told"
      (is (search "deepcanary C3" (second entry)) "and the notice says the level"))))

(deftest deepcanary-cell-the-hourly-budget-holds-a-second-c2 ()
  (with-deepcanary ("max_interrupts_per_hour" 1)
    (is-carrying (text (deepcanary-raise-one "TASK_FAILED" :dedupe "a")) ("the operator was told"))
    (is (equal 1 (nodecode-deepcanary::interrupt-count)) "one interrupt counted")
    (is-carrying (text (deepcanary-raise-one "TOOL_FAILURE_LOOP" :dedupe "b"))
                  ("held by the hourly interrupt budget" "the second waits"))
    (is-carrying (text (deepcanary:raise "HUMAN_APPROVAL_REQUIRED" :authority "host" :summary "ok?"))
                  ("the operator was told" "a C3 is not counted"))))

(deftest deepcanary-cell-below-level-stays-in-the-inbox ()
  (with-deepcanary ("level" "C3")
    (is-carrying (text (deepcanary-raise-one "TASK_COMPLETED" :dedupe "a"))
                  ("below level C3" "a C1 is kept, not said"))
    (is (null (cell-notice "nodecode-deepcanary")) "and nothing was said")
    (is (= 1 (length (deepcanary-items "open"))) "though the item is there")))

;;; --- the model's side -----------------------------------------------------------

(deftest deepcanary-cell-a-raised-item-reaches-the-model-once ()
  (with-deepcanary ()
    (deepcanary-raise-one "TOOL_FAILURE_LOOP" :dedupe "a" :summary "the tests keep failing")
    (is-present (text (nodecode-deepcanary:reminder)) "a reminder is offered"
      (is-carrying text ("[deepcanary: 1 open, 1 new" "the framing line")
                        ("#1 C2 INTERRUPT · TOOL_FAILURE_LOOP" "and the item")))
    (is (null (nodecode-deepcanary:reminder)) "and the next request is told nothing new")))

(deftest deepcanary-cell-the-tools-raise-list-and-settle ()
  (with-deepcanary ()
    (let ((signal (nodecode-deepcanary::decode-attention-signal
                   (nlk:make-json-object "reason" "HUMAN_QUESTION_PENDING"
                                          "authority" "host"
                                          "summary" "which branch?"))))
      (is-carrying (text (nodecode-deepcanary::tool-raise signal))
                    ("raised #1 C3 ESCALATE" "the tool raises it at its level")))
    (is-carrying (text (nodecode-deepcanary::tool-inbox
                         (nodecode-deepcanary::decode-attention-query
                          (nlk:make-json-object "status" "open"))))
                  ("#1 C3 ESCALATE · HUMAN_QUESTION_PENDING" "and the tool lists it"))
    (is-carrying (text (nodecode-deepcanary::tool-snooze
                         (nodecode-deepcanary::decode-attention-snooze
                          (nlk:make-json-object "id" 1 "minutes" 5))))
                  ("snoozed #1 for 5 minutes" "and puts it aside"))
    (is-carrying (text (nodecode-deepcanary::tool-dry-run
                         (nodecode-deepcanary::decode-attention-dry-run
                          (nlk:make-json-object "reason" "TOOL_FAILURE_LOOP"
                                                "authority" "heuristic"))))
                  ("=> C2" "a dry run answers the verdict") (:absent "raised"))))

(deftest deepcanary-cell-the-command-answers-text-and-a-panel ()
  (with-deepcanary ()
    (is-carrying (text (nodecode-deepcanary::run-slash "" nil)) ("nothing with status" "empty, it says so"))
    (deepcanary-raise-one "TASK_FAILED" :dedupe "a" :summary "the build broke")
    (multiple-value-bind (text dialog) (nodecode-deepcanary::run-slash "" nil)
      (is-carrying text ("#1 C2 INTERRUPT · TASK_FAILED" "the command lists the item"))
      (is dialog "and answers a panel"))
    (is (search "acknowledged #1" (nodecode-deepcanary::run-slash "acknowledge 1" nil)) "acknowledge ID settles it from the command")
    (is (search "C3" (nodecode-deepcanary::run-slash "preview HUMAN_QUESTION_PENDING host" nil)) "and preview reads a signal")))

(deftest deepcanary-cell-a-bad-call-says-why-and-writes-nothing ()
  (with-deepcanary ()
    (is (search "no deepcanary reason" (refusal-text deepcanary:deepcanary-error
                                               (deepcanary:raise "MOOD" :summary "x"))))
    (is (search "no evidence authority" (refusal-text deepcanary:deepcanary-error
                                                     (deepcanary:raise "TASK_FAILED" :authority "vibes" :summary "x"))))
    (is (search "no inbox item" (refusal-text deepcanary:deepcanary-error
                                        (deepcanary:acknowledge 42))))
    (is (search "no snooze" (refusal-text deepcanary:deepcanary-error
                                  (deepcanary:snooze 1 :minutes 9000))))
    (is (= 0 (deepcanary-count)) "and nothing was written")))

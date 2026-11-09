;;;; cell-test.lisp --- the presets cell against a stubbed pick.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here needs a provider, a key or a store: the two gateway seams
;;;; (SELECT-MODEL and SELECT-EFFORT) and the session-selection reader are
;;;; rebound to record and answer, so every test says what the cell decided
;;;; without moving a real session.

(in-package #:nodecode.test)

(define-test-slice "model-presets" "MODEL-PRESETS-CELL-" :start nodecode-model-presets:start-cell)

(defparameter +mp-presets+
  (nlk:json-object
   "fast" (nlk:json-object "model" "claude-haiku-4.5" "provider" "anthropic" "effort" "low")
   "deep" (nlk:json-object "model" "gpt-5.4" "provider" "openai"))
  "The table every test starts on: one preset with a rung, one without.")

(defvar *mp-calls* nil
  "What this test's stubbed pick recorded, newest first: the plist it was called with.")

(defmacro with-presets (&body body)
  "BODY with the pick stubbed: SELECT-MODEL and SELECT-EFFORT record into
*MP-CALLS* and answer a line, and the session reads as pinned."
  `(let ((*mp-calls* '()))
     (let ((nodecode-model-presets::*select-model*
             (lambda (model &key provider session-id)
               (push (list :model model :provider provider :session-id session-id) *mp-calls*)
               (format nil "session ~a pinned to ~a/~a" session-id provider model)))
           (nodecode-model-presets::*select-effort*
             (lambda (effort &key session-id)
               (push (list :effort effort :session-id session-id) *mp-calls*)
               (format nil "reasoning effort for ~a: ~a" session-id effort)))
           (nodecode-model-presets::*selection*
             (lambda (session-id)
               (declare (ignore session-id))
               (values "openai" "gpt-5.4" "medium"))))
       ,@body)))

;;; --- the lifecycle ----------------------------------------------------------

(define-cell-lifecycle-tests "model-presets"
  (:fixture with-presets)
  (:config "presets" +mp-presets+)
  (:help :model-presets)
  (:command "preset")
  (:running
   (is (nle::find-tool "model_presets") "the model's tool is registered")
   (is (= 2 (length (nodecode-model-presets::presets))) "the section's two presets are what it runs on")
   (is (equal "low" (getf (nodecode-model-presets::find-preset "fast") :effort)) "a preset's rung is read")
   (is (null (getf (nodecode-model-presets::find-preset "deep") :effort)) "and an absent one stays absent"))
  (:stopped
   (is (null (nle::find-tool "model_presets")) "stop unregisters the tool"))
  (:refused ("presets" "fast")
            ("presets" (nlk:json-object "fast" (nlk:json-object "model" "claude-haiku-4.5")))
            ("presets" (nlk:json-object "fast" (nlk:json-object "model" "claude-haiku-4.5"
                                                                  "provider" "anthropic"
                                                                  "effort" "sideways"))))
  (:idle nodecode-model-presets:model-presets-error (nodecode-model-presets::presets-text)))

;;; --- /preset ---------------------------------------------------------------

(deftest model-presets-cell-lists-the-table ()
  (with-presets
    (with-cell-stop ((model-presets-start "presets" +mp-presets+))
      (let ((text (cell-entry "nodecode-model-presets" "preset" "list" "s1")))
        (is (search "fast: claude-haiku-4.5 on anthropic at low" text) "a preset reads as its three parts")
        (is (search "deep: gpt-5.4 on openai" text) "a preset with no rung says no rung")
        (is (null *mp-calls*) "listing moves nothing")))))

(deftest model-presets-cell-switches-through-the-gateway-pick ()
  (with-presets
    (with-cell-stop ((model-presets-start "presets" +mp-presets+))
      (let ((text (cell-entry "nodecode-model-presets" "preset" "fast" "s1")))
        (is (search "pinned to anthropic/claude-haiku-4.5" text) "the answer is the pick's own line")
        (is (find "claude-haiku-4.5" *mp-calls* :key (lambda (call) (getf call :model)) :test #'equal) "the preset's model is the pick")
        (is (equal "s1" (getf (find "claude-haiku-4.5" *mp-calls*
                                    :key (lambda (call) (getf call :model)) :test #'equal)
                              :session-id)) "on this session")
        (is (find "low" *mp-calls* :key (lambda (call) (getf call :effort)) :test #'equal) "its rung is the pick's effort")))))

(deftest model-presets-cell-an-override-wins-over-the-preset ()
  (with-presets
    (with-cell-stop ((model-presets-start "presets" +mp-presets+))
      (let ((text (cell-entry "nodecode-model-presets" "preset"
                              "deep --effort high --provider openrouter" "s1")))
        (is (search "pinned to openrouter/gpt-5.4" text) "the override is what lands")
        (is (find "high" *mp-calls* :key (lambda (call) (getf call :effort)) :test #'equal) "the override's rung, not the preset's")))))

(deftest model-presets-cell-a-name-it-does-not-have-refuses ()
  (with-presets
    (with-cell-stop ((model-presets-start "presets" +mp-presets+))
      (let ((text (cell-entry "nodecode-model-presets" "preset" "slow" "s1")))
        (is (search "no preset named slow" text) "the refusal names the name")
        (is (search "fast, deep" text) "and the ones it does have")
        (is (null *mp-calls*) "and nothing moved")))))

(deftest model-presets-cell-the-picker-and-the-status-line ()
  (with-presets
    (with-cell-stop ((model-presets-start "presets" +mp-presets+))
      (multiple-value-bind (text dialog)
          (cell-entry "nodecode-model-presets" "preset" "" "s1")
        (is (equal "Model presets" (nlk:json-value dialog :string "title")) "typing /preset alone opens the picker")
        (is (= 2 (length (nlk:json-value dialog :array "rows"))) "with a row a preset")
        (is (search "claude-haiku-4.5" text) "and the same words as text"))
      (let ((status (cell-entry "nodecode-model-presets" "preset" "status" "s1")))
        (is (search "pinned to openai/gpt-5.4 at medium" status) "status reads the session's pin")
        (is (search "2 presets" status) "and counts the table")
        (is (null *mp-calls*) "without moving anything")))))

;;; --- the model's tool -------------------------------------------------------

(deftest model-presets-cell-the-tool-lists-and-switches ()
  (with-presets
    (with-cell-stop ((model-presets-start "presets" +mp-presets+))
      (let ((listed (nodecode-model-presets:presets-tool
                     (nodecode-model-presets:decode-preset-request
                      (nlk:json-object "action" "list")))))
        (is (search "fast: claude-haiku-4.5 on anthropic at low" listed) "the tool lists the table"))
      (let ((nlk:*scribe-session-id* "s1"))
        (let ((said (nodecode-model-presets:presets-tool
                     (nodecode-model-presets:decode-preset-request
                      (nlk:json-object "action" "switch" "preset" "deep")))))
          (is (search "pinned to openai/gpt-5.4" said) "the tool lands the pick on the ambient session")
          (is (find "gpt-5.4" *mp-calls* :key (lambda (call) (getf call :model)) :test #'equal) "through the same seam /preset uses"))))))

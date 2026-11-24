;;;; cell-test.lisp --- the agent builder cell against a stubbed side call.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; No test needs a provider, a key or the network: NLE:COMPLETE -- the one
;;;; seam every model call goes through -- is stubbed, and what is under test
;;;; is the part this cell owns: the brief it hands over, the blueprint it
;;;; reads back, the draft it keeps, and the refusal it answers when the
;;;; blueprint or the brief is wrong.

(in-package #:nodecode.test)

(define-test-slice "agent-builder" "AGENT-BUILDER-CELL-" :start nodecode-agent-builder:start-cell)

(define-cell-lifecycle-tests "agent-builder"
  (:help :agent-builder)
  (:command "agent-builder")
  (:running
   (is (nle::find-tool "agent_builder") "the tool the model calls is registered"))
  (:refused
   ("max_brief" 10))
  (:idle nodecode-agent-builder:agent-builder-error (agent-builder:drafts)))

;;; --- the stub ---------------------------------------------------------------

(defparameter *blueprint*
  "Here it is:
```json
{\"name\": \"Tide Reader\",
 \"slug\": \"tide-reader\",
 \"description\": \"Reads a tide table and answers when the water allows a launch.\",
 \"instructions\": \"Answer with the height, the time window and what the table says. Ask for the port when none is given.\",
 \"starters\": [\"When is high tide?\", \"Is tomorrow a launch window?\", \"Read this tide table.\"],
 \"success_checks\": [\"names the height at the port\", \"gives the window, not a single time\", \"cites the table it read\"],
 \"toolsets\": [\"web\", \"files\"],
 \"skills\": []}
```"
  "A model's answer as one arrives: prose, a fence, the JSON.")

(defvar *answers* nil
  "What the stubbed side call answers, consumed one per call.")

(defvar *asked* nil
  "Every side call the stub was handed, newest first: (SYSTEM . USER).")

(defun stub-complete (system user &key session-id max-tokens fallback-p)
  "NLE:COMPLETE, stubbed: the next scripted answer, or the sample blueprint."
  (declare (ignore session-id max-tokens fallback-p))
  (push (cons system user) *asked*)
  (or (pop *answers*) *blueprint*))

(defmacro with-side-call ((&rest answers) &body body)
  "BODY with the cell's side call stubbed and ANSWERS queued for it."
  `(let ((*answers* (list ,@answers))
         (*asked* nil))
     (with-stubbed-fdefinition (nle:complete (system user &key session-id max-tokens fallback-p)
                                 (stub-complete system user :session-id session-id
                                                :max-tokens max-tokens :fallback-p fallback-p))
       ,@body)))

(defun wire-call (name arguments)
  "NAME called the way the model calls it, as the text the model reads back."
  (nle::message-content (execute-wire-call "call-agent-builder" name arguments)))

(defun draft-id-in (text)
  "The draft id DRAFT-TEXT opens with, as /agent-builder writes it."
  (subseq text 0 32))

(defun build-one ()
  "A draft made through the tool, with the side call stubbed. => (TEXT ID)."
  (let ((text (wire-call "agent_builder" "{\"brief\": \"Launch a boat on the tide\"}")))
    (values text (draft-id-in text))))

;;; --- a brief becomes a draft ------------------------------------------------

(deftest agent-builder-cell-a-brief-becomes-a-draft ()
  (with-cell-stop ((agent-builder-start))
    (with-side-call ()
      (multiple-value-bind (text id) (build-one)
        (is (string= id (nlk:trimmed (subseq text 0 32))) "the answer opens with the draft's id")
        (is (search "Tide Reader" text) "and names the agent the model described")
        (is (search "Answer with the height" text) "with its standing instructions whole")
        (is-present (entry (gethash "user_brief" (shasht:read-json (cdr (first *asked*)))))
          "the side call was handed the brief as data"
          (is (equal "Launch a boat on the tide" entry)))
        (is (search "one focused specialist agent" (car (first *asked*))) "under the blueprint instructions")
        (is (search "Tide Reader" (agent-builder:drafts)) "and the draft is in the store")
        (is (search "Tide Reader" (agent-builder:show id)) "show reads it back")))))

(deftest agent-builder-cell-an-empty-brief-refuses-before-any-model-call ()
  (with-cell-stop ((agent-builder-start))
    (with-side-call ()
      (let ((condition (signals-error nodecode-agent-builder:agent-builder-error
                          (agent-builder:build "   "))))
        (is (search "add brief first" (string-downcase (princ-to-string condition))) "a brief is missing"))
      (is (signals-error nodecode-agent-builder:agent-builder-error (agent-builder:build nil)))
      (is (null *asked*) "the model was never asked"))))

(deftest agent-builder-cell-a-brief-over-the-section-limit-refuses ()
  (with-cell-stop ((agent-builder-start "max_brief" 1000))
    (with-side-call ()
      (let ((condition (signals-error nodecode-agent-builder:agent-builder-error
                          (agent-builder:build (make-string 1200 :initial-element #\a)))))
        (is (search "too long" (princ-to-string condition)) "the refusal says what is wrong")
        (is (null *asked*) "and nothing was asked")))))

;;; --- a draft answered badly ---------------------------------------------

(deftest agent-builder-cell-an-answer-with-no-blueprint-refuses ()
  (with-cell-stop ((agent-builder-start))
    (with-side-call ("The model says: a poem, not a blueprint.")
      (is (signals-error nodecode-agent-builder:agent-builder-error
                          (agent-builder:build "Launch a boat on the tide"))))))

(deftest agent-builder-cell-a-capability-the-section-does-not-offer-is-dropped ()
  (with-cell-stop ((agent-builder-start "toolsets" (vector "files")))
    (with-side-call ()
      (let* ((text (wire-call "agent_builder" "{\"brief\": \"Launch a boat on the tide\"}"))
             (plan (nodecode-agent-builder::draft-plan
                    (nodecode-agent-builder::load-draft (draft-id-in text))))
             (toolsets (nodecode-agent-builder::plan-strings plan "toolsets")))
        (is (equal '("files") toolsets) "only the toolset the section offers survives")))))

;;; --- refine, trial, compare ------------------------------------------------

(deftest agent-builder-cell-refine-keeps-the-version-it-replaced ()
  (with-cell-stop ((agent-builder-start))
    (with-side-call ()
      (multiple-value-bind (text id) (build-one)
        (declare (ignore text))
        (let ((refined (agent-builder:refine id "Answer in a table.")))
          (is (search "v2" refined) "the draft is version two")
          (let ((kept (nodecode-agent-builder::version-snapshots
                       (nodecode-agent-builder::load-draft id))))
            (is (= 1 (length kept)) "the version it replaced is kept"))
          (is (search "refined: Answer in a table." (agent-builder:show id)) "show carries the new brief"))))))

(deftest agent-builder-cell-a-trial-runs-the-blueprint-as-its-system-instruction ()
  (with-cell-stop ((agent-builder-start))
    (with-side-call ()
      (multiple-value-bind (text id) (build-one)
        (declare (ignore text))
        (with-side-call ("A window opens at 08:00.")
         (let ((reply (agent-builder:trial id "When can I launch?")))
          (is (search "A window opens at 08:00." reply) "the reply is what the model answered")
          (is (search "# Tide Reader" (car (first *asked*))) "the trial's system is the blueprint")
          (is (search "Answer with the height" (car (first *asked*))) "instructions whole")
          (is (equal "When can I launch?" (cdr (first *asked*))) "the task is the message")))))))

(deftest agent-builder-cell-a-comparison-needs-two-versions ()
  (with-cell-stop ((agent-builder-start))
    (with-side-call ()
      (multiple-value-bind (text id) (build-one)
        (declare (ignore text))
        (is (signals-error nodecode-agent-builder:agent-builder-error
                            (agent-builder:compare id "Which window?")))
        (agent-builder:refine id "Answer in a table.")
        (with-side-call ("v1 says 06:10." "v2 says 06:10, in a table.")
          (let ((both (agent-builder:compare id "Which window?")))
            (is (search "v1 says 06:10." both) "the earlier version's answer is there")
            (is (search "v2 says 06:10, in a table." both) "and the current one's")
            (is (= 2 (length *asked*)) "one task, two side calls")))))))

;;; --- show, export, drop -----------------------------------------------------

(deftest agent-builder-cell-export-and-drop ()
  (with-cell-stop ((agent-builder-start))
    (with-side-call ()
      (multiple-value-bind (text id) (build-one)
        (declare (ignore text))
        (let ((exported (shasht:read-json (agent-builder:export-blueprint id))))
          (is (equal "nodecode-agent-builder" (gethash "format" exported)) "the export names its format")
          (is (equal "tide-reader" (gethash "slug" (gethash "plan" exported))) "and carries the plan"))
        (is (search "Tide Reader" (agent-builder:show id)) "show answers the draft")
        (is (search "Dropped" (agent-builder:drop id)) "drop says what it removed")
        (is (signals-error nodecode-agent-builder:agent-builder-error (agent-builder:show id)) "it is gone")
        (is (signals-error nodecode-agent-builder:agent-builder-error (agent-builder:show "nope")) "a bad id is refused")))))

;;; --- the operator's command ---------------------------------------------------

(deftest agent-builder-cell-the-command-lists-and-refines ()
  (with-cell-stop ((agent-builder-start))
    (with-side-call ()
      (let ((built (cell-entry "nodecode-agent-builder" "agent-builder" "Launch a boat on the tide")))
        (is (search "Tide Reader" built) "a bare brief builds")
        (let ((id (draft-id-in built)))
          (is (search id (cell-entry "nodecode-agent-builder" "agent-builder" "")) "no arguments lists it")
          (is (search "v2" (cell-entry "nodecode-agent-builder" "agent-builder" (format nil "refine ~a Use a table." id))) "refine takes an id and text"))))))

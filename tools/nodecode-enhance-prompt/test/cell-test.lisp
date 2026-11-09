;;;; cell-test.lisp --- the enhance-prompt cell against a stubbed side call.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The one model call every rewrite makes is a stubbed NLE:COMPLETE: nothing
;;;; reaches a provider, and each call is kept so a test can read the system
;;;; message and the user message the model was actually given. The tool is
;;;; called the way a model's call reaches it - through the real dispatch,
;;;; contract and all - and the command the way a shell runs it, through
;;;; NLE:SLASH with the composer's draft.

(in-package #:nodecode.test)

(define-test-slice "enhance-prompt" "ENHANCE-PROMPT-CELL-"
  :start nodecode-enhance-prompt:start-cell)

(defvar *asked* '()
  "(SYSTEM USER) pairs the stubbed side call was asked with, newest first.")

(defmacro with-model ((answer) &body body)
  "BODY with the one side call answered by ANSWER, a form read in this scope,
and every (SYSTEM USER) pair it was asked with collected in *ASKED*."
  `(let ((*asked* '()))
     (with-stubbed-fdefinition (nle:complete (system user &key session-id max-tokens fallback-p)
                                (push (list system user) *asked*)
                                ,answer)
       ,@body)))

(defun enhance-answer (id arguments)
  "The enhance_prompt tool's answer to ARGUMENTS, as the model reads it:
through the real dispatch."
  (nle::message-content (execute-wire-call id "enhance_prompt" arguments)))

(defun effects-draft (effects)
  "The draft an answer's first effect sets."
  (nlk:json-value (aref effects 0) :string "draft"))

;;; --- the lifecycle, from the one declaration ----------------------------------------

(define-cell-lifecycle-tests "enhance-prompt"
  (:help :enhance-prompt)
  (:command "enhance")
  (:refused ("default_stage" "9") ("max_tokens" 5)))

;;; --- the tool ------------------------------------------------------------------------

(deftest enhance-prompt-cell-rewrites-a-draft (with-cell-stop ((enhance-prompt-start))
  (with-model ("Goal: make login faster.
1. Find where the turn waits.
2. Cut the wait.")
    (let ((answer (enhance-answer "e1" "{\"input\":\"login is slow, make it faster please\"}")))
      (is (search "Goal: make login faster." answer) "the brief is the answer")
      (is (= 1 (length *asked*)) "one side call, no tools, no history")
      (let ((system (first (first *asked*)))
            (user (second (first *asked*))))
        (is (search "prompt engineer" system) "the original's system prompt is the instruction")
        (is (search "Stage 1" system) "with the stage's own hint")
        (is (search "login is slow, make it faster please" user) "the draft is what is rewritten")
        (is (search "Rewrite this request as an agent prompt" user)))))))

(deftest enhance-prompt-cell-strips-the-wrappers-around-a-brief (with-cell-stop ((enhance-prompt-start))
  (with-model ("  ```markdown
Goal: ship it.
1. Do it.
```  ")
    (let ((answer (enhance-answer "e2" "{\"input\":\"ship the thing somehow\"}")))
      (is (equal "Goal: ship it.
1. Do it." answer) "the fence and the padding are not the brief")))))

(deftest enhance-prompt-cell-deepens-on-a-stage (with-cell-stop ((enhance-prompt-start))
  (with-model ("## Phase 1
1. Profile.")
    (enhance-answer "e3" "{\"input\":\"make the parser faster please\",\"stage\":3}")
    (is (search "Stage 3" (first (first *asked*))) "the stage the caller named is the hint it ran with"))))

(deftest enhance-prompt-cell-ignores-a-draft-too-short (with-cell-stop ((enhance-prompt-start))
  (with-model ("never asked")
    (is (search "nothing to rewrite" (enhance-answer "e4" "{\"input\":\"hi\"}")))
    (is (null *asked*) "a draft of two characters spends no model turn"))))

(deftest enhance-prompt-cell-refuses-a-call-with-no-draft (with-cell-stop ((enhance-prompt-start))
  (with-model ("never asked")
    (let ((answer (enhance-answer "e5" "{}")))
      (is (search "preflight" answer) "the contract refuses it before the body")
      (is (search "input" answer)))
    (is (null *asked*) "and the model was never asked"))))

(deftest enhance-prompt-cell-reports-a-failed-call (with-cell-stop ((enhance-prompt-start))
  (with-model ((error "the lane is down"))
    (let ((answer (enhance-answer "e6" "{\"input\":\"make the parser faster please\"}")))
      (is (search "enhance_prompt failed" answer) "a refused provider is reported, not thrown at the model")
      (is (search "the lane is down" answer))))))

;;; --- the command ---------------------------------------------------------------------
;; The command is a plain function of (ARGS SESSION-ID &KEY DRAFT), so it is
;; called the way a shell's arm calls it: no dispatch, no provider, and the
;; effect it answers read as the plist a shell would perform.

(deftest enhance-prompt-cell-rewrites-the-draft-in-place (with-cell-stop ((enhance-prompt-start))
  (with-model ("Goal: make the parser faster.
1. Profile it.")
    (multiple-value-bind (text dialog effects)
        (nodecode-enhance-prompt::run-slash "" "s-enh-1" :draft "make the parser faster pls")
      (declare (ignore dialog))
      (is (search "Goal: make the parser faster." text) "the brief is what the shell shows")
      (is (search "make the parser faster pls" (second (first *asked*))) "from the composer's own draft")
      (is (equal text (getf (first effects) :draft)) "and the effect puts it back in the composer")
      (is (eq :set (getf (first effects) :how)) "replacing the draft, not sending it")))))

(deftest enhance-prompt-cell-takes-a-draft-as-its-argument (with-cell-stop ((enhance-prompt-start))
  (with-model ("Goal: ship it.
1. Do it.")
    (multiple-value-bind (text dialog effects)
        (nodecode-enhance-prompt::run-slash "2 make the parser faster" "s-enh-arg")
      (declare (ignore dialog effects))
      (is (search "Goal: ship it." text) "a shell that cannot hand over a draft still rewrites one")
      (is (search "Stage 2" (first (first *asked*))) "and a leading 2 starts it at that stage")))))

(deftest enhance-prompt-cell-deepens-the-brief-it-made (with-cell-stop ((enhance-prompt-start))
  (with-model ("Goal: ship it.
1. Profile the parser.")
    (nodecode-enhance-prompt::run-slash "" "s-enh-2" :draft "make the parser faster pls")
    (nodecode-enhance-prompt::run-slash "" "s-enh-2" :draft "Goal: ship it.
1. Profile the parser.")
    (is (= 2 (length *asked*)) "a second click is a second rewrite")
    (is (search "Stage 1" (first (second *asked*))) "the first run clarified")
    (is (search "Stage 2" (first (first *asked*))) "and running it again on its own brief deepened it"))))

(deftest enhance-prompt-cell-undo-restores-the-first-draft (with-cell-stop ((enhance-prompt-start))
  (with-model ("Goal: ship it.
1. Profile the parser.")
    (nodecode-enhance-prompt::run-slash "" "s-enh-3" :draft "make the parser faster pls")
    (multiple-value-bind (text dialog effects)
        (nodecode-enhance-prompt::run-slash "undo" "s-enh-3" :draft "Goal: ship it.
1. Profile the parser.")
      (declare (ignore dialog))
      (is (search "Restored" text))
      (is (equal "make the parser faster pls" (getf (first effects) :draft)) "the discard puts the original back"))
    (is (search "nothing to undo" (nodecode-enhance-prompt::run-slash "undo" "s-enh-3"))
        "a second undo says so"))))

(deftest enhance-prompt-cell-asks-for-a-draft-of-its-own (with-cell-stop ((enhance-prompt-start))
  (with-model ("never asked")
    (is (search "Usage" (nodecode-enhance-prompt::run-slash "" "s-enh-4" :draft "hi")))
    (is (search "Usage" (nodecode-enhance-prompt::run-slash "2" "s-enh-4")))
    (is (null *asked*) "a command with nothing to rewrite spends no model turn"))))

;;; --- the wrappers, on their own -------------------------------------------------------

(deftest enhance-prompt-cell-strips-thinking-and-quotes ()
  ;; The tags are built from their characters: a source line that spelled one
  ;; out would be markup to anything reading this file as text.
  (let ((lt (string (code-char #x3C)))
        (gt (string (code-char #x3E))))
    (is (equal "Goal: y."
               (nodecode-enhance-prompt:strip-wrappers
                (format nil "~athinklet me think~a~cthink~a Goal: y." lt lt #\/ gt)))
        "a thinking block is not part of the brief")
    (is (equal "Goal: w."
               (nodecode-enhance-prompt:strip-wrappers
                (format nil "~a~cchannel~c~aGoal: w." lt (code-char #xFF5C) (code-char #xFF5C) gt)))
        "and neither is the channel tag a serving backend emits")
    (is (equal "Goal: z." (nodecode-enhance-prompt:strip-wrappers "\"Goal: z.\""))
        "quotes around the brief are not the brief")
    (is (equal "" (nodecode-enhance-prompt:strip-wrappers nil)))))

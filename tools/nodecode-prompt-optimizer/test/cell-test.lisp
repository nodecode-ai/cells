;;;; cell-test.lisp --- start and stop, the rewrite, the family, the tool, the record.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The model call is the one stub: NLE:COMPLETE answers a fixed rewrite, so
;;;; everything else here is real -- the section, the registrations, the
;;;; scoring, and the history file the scratch home holds.

(in-package #:nodecode.test)

(define-test-slice "prompt-optimizer" "PROMPT-OPTIMIZER-CELL-"
  :start nodecode-prompt-optimizer:start-cell)

(defparameter *draft*
  "hey could you maybe explain to me very kindly what python generators are please when you get a chance")

(define-cell-lifecycle-tests "prompt-optimizer"
  (:help :prompt-optimizer)
  (:command "optimize")
  (:running (is (equal "auto" (nodecode-prompt-optimizer::setting :family)) "auto is the family unless the section says otherwise")
            (is (eq t (nodecode-prompt-optimizer::setting :record)) "a record is kept by default")
            (is (= 3 (nodecode-prompt-optimizer::setting :min-words)) "three words is the floor"))
  (:refused ("max_tokens" 8) ("family" "cobol"))
  (:idle nodecode-prompt-optimizer:prompt-optimizer-error
         (nodecode-prompt-optimizer:rewrite-prompt *draft*)))

(deftest prompt-optimizer-cell-the-command-rewrites-through-the-model ()
  (with-cell-stop ((prompt-optimizer-start))
    (let ((asked nil))
      (with-stubbed-fdefinition (nle:complete (system user &key session-id max-tokens fallback-p)
                                  (setf asked (list system user max-tokens))
                                  "Explain Python generators.")
        (let ((text (nodecode-prompt-optimizer::run-slash *draft* "s-1")))
          (is (search "Explain Python generators." text) "the operator reads the rewrite")
          (is (search "-> " text) "under what it did to the score")
          (is (search "words" text) "and to the length")
          (is (equal *draft* (second asked)) "the draft went to the model as it was typed")
          (is (search "imperative verb" (first asked)) "under the rewriting instruction")
          (is (= 400 (third asked)) "with the section's ceiling"))))))

(deftest prompt-optimizer-cell-a-short-prompt-is-left-alone ()
  (with-cell-stop ((prompt-optimizer-start))
    (with-stubbed-fdefinition (nle:complete (system user &key session-id max-tokens fallback-p)
                                (error "the model was asked about a two-word prompt"))
      (is (search "2 words already" (nodecode-prompt-optimizer:rewrite-prompt "fix it")) "a two-word prompt stands as it is, and no model was asked"))))

(deftest prompt-optimizer-cell-the-target-picks-the-tactics ()
  (with-cell-stop ((prompt-optimizer-start))
    (let ((asked '()))
      (with-stubbed-fdefinition (nle:complete (system user &key session-id max-tokens fallback-p)
                                  (push system asked) "Explain Python generators.")
        (nodecode-prompt-optimizer:rewrite-prompt *draft* :target "claude-opus-4-7")
        (nodecode-prompt-optimizer:rewrite-prompt *draft* :target "gpt-4o")
        (nodecode-prompt-optimizer:rewrite-prompt *draft*))
      (is (search "claude model" (third asked)) "a claude target is written for claude")
      (is (search "openai model" (second asked)) "an openai target for openai")
      (is (not (search "write for that family" (first asked))) "a prompt with no target gets the plain rule")
      (is (search "imperative verb" (first asked)) "which is still the rewriting rule"))))

(deftest prompt-optimizer-cell-the-tool-answers-through-its-contract ()
  (with-cell-stop ((prompt-optimizer-start))
    (with-stubbed-fdefinition (nle:complete (system user &key session-id max-tokens fallback-p)
                                "Explain Python generators.")
      (let ((entry (nle::find-tool "optimize_prompt")))
        (is (consp entry) "optimize_prompt is registered")
        (let* ((schema (second entry))
               (answer (funcall (third entry)
                                (nlk:contract-decode schema (nlk:make-json-object
                                                             "prompt" *draft*)))))
          (is (search "Explain Python generators." answer) "the body's answer is the tool's answer"))
        (is (signals-error nlk:contract-refusal
              (nlk:contract-decode (second entry) (nlk:make-json-object "prompt" ""))) "a blank prompt is refused before the body runs")))))

(deftest prompt-optimizer-cell-the-record-is-what-the-stats-read ()
  (with-cell-stop ((prompt-optimizer-start))
    (let ((before (length (nodecode-prompt-optimizer::history-rows))))
      (with-stubbed-fdefinition (nle:complete (system user &key session-id max-tokens fallback-p)
                                  "Explain Python generators.")
        (nodecode-prompt-optimizer:rewrite-prompt *draft*)
        (nodecode-prompt-optimizer:rewrite-prompt *draft*))
      (is (= (+ 2 before) (length (nodecode-prompt-optimizer::history-rows))) "each rewrite is one recorded line")
      (let ((text (nodecode-prompt-optimizer:history)))
        (is (search (format nil "~d rewrites recorded" (+ 2 before)) text) "the stats count what was recorded")
        (is (search "average score" text) "and say what it gained")
        (is (search "words" text) "and how long it was")))))

(deftest prompt-optimizer-cell-a-refused-rewrite-says-why-and-keeps-the-prompt ()
  (with-cell-stop ((prompt-optimizer-start))
    (let ((before (length (nodecode-prompt-optimizer::history-rows))))
      (with-stubbed-fdefinition (nle:complete (system user &key session-id max-tokens fallback-p)
                                  (error "no provider answered"))
        (let ((condition (handler-case (nodecode-prompt-optimizer:rewrite-prompt *draft*)
                           (nodecode-prompt-optimizer:prompt-optimizer-error (condition)
                             condition))))
          (is (search "the model call failed" (princ-to-string condition)) "the refusal names the failure")))
      (is (= before (length (nodecode-prompt-optimizer::history-rows))) "and nothing was recorded for a rewrite that did not happen"))))

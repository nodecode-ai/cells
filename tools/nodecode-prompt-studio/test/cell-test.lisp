;;;; cell-test.lisp --- start and stop, the wizard's answers, the three
;;;; models, the tool, and the one polishing call.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The model call is the one stub: NLE:COMPLETE answers a fixed rewrite, so
;;;; everything else here is real -- the section, the registrations, the
;;;; answer parser and the prompt the engines write.

(in-package #:nodecode.test)

(define-test-slice "prompt-studio" "PROMPT-STUDIO-CELL-"
  :start nodecode-prompt-studio:start-cell)

(define-cell-lifecycle-tests "prompt-studio"
  (:help :prompt-studio)
  (:command "prompt")
  (:running (is (equal "auto" (nodecode-prompt-studio::setting :default-model)) "the neutral rules unless the section says otherwise")
            (is (= 12000 (nodecode-prompt-studio::setting :paste-cap)) "twelve thousand pasted characters is the ceiling")
            (is (eq t (nodecode-prompt-studio::setting :polish)) "a prompt asked to be polished is polished"))
  (:refused ("paste_cap" 10) ("default_model" "cobol") ("max_tokens" 8))
  (:idle nodecode-prompt-studio:prompt-studio-error
         (nodecode-prompt-studio:build-prompt (nodecode-prompt-studio:brief :task "fix it"))))

(deftest prompt-studio-cell-the-command-builds-a-prompt ()
  (with-cell-stop ((prompt-studio-start))
    (let ((text (nodecode-prompt-studio::run-prompt "Fix the login bug; deliverable: implementation" "s-1")))
      (is (search "TASK" text) "the prompt opens with the task")
      (is (search "Fix the login bug" text) "which is what was typed")
      (is (search "REQUIREMENTS" text) "with the requirements section")
      (is (search "no extra features" text) "and the scope line implementation work gets")
      (is (search "DONE WHEN" text) "and a check for done")
      (is (search "show the commands you ran" text) "that asks for evidence")
      (is (search "written for the model reading it" text) "and says which rules it was written by"))))

(deftest prompt-studio-cell-the-answers-become-sections ()
  (with-cell-stop ((prompt-studio-start))
    (let ((text (nodecode-prompt-studio::run-prompt
                 "Summarise the contract; paste: TERM: 12 months; context: the vendor's draft; rules: quote the clause; deliverable: analysis"
                 "s-1")))
      (is (search "TERM: 12 months" text) "pasted text travels into the prompt")
      (is (search "pasted material" text) "inside the markers, for the neutral profile")
      (is (search "the vendor's draft" text) "context is its own section")
      (is (search "quote the clause" text) "and the rules are kept as written")
      (is (search "Back each conclusion" text) "an analysis says what its conclusions rest on"))))

(deftest prompt-studio-cell-each-model-gets-its-own-rules ()
  (with-cell-stop ((prompt-studio-start))
    (let ((opus (nodecode-prompt-studio::run-prompt
                 "Fix the checkout page; model: opus; paste: <script>x</script>; deliverable: implementation" "s-1"))
          (astra (nodecode-prompt-studio::run-prompt
                  "Fix the checkout page; model: astra; paste: <script>x</script>; deliverable: implementation" "s-1")))
      (is (search "Claude Opus 5.5" opus) "opus is named")
      (is (search "<pasted_content id=" opus) "and pasted text is tagged for it")
      (is (search "&lt;script" opus) "so it cannot close its own tag")
      (is (search "GPT-6 Astra" astra) "astra is named")
      (is (not (search "<pasted_content" astra)) "and pasted text is not tagged for it")
      (is (search "take precedence over guidelines" astra) "astra gets its priority line")
      (is (search "Visual design: do not use" opus) "a page gets the design line"))))

(deftest prompt-studio-cell-a-draft-stands-in-for-the-task ()
  (with-cell-stop ((prompt-studio-start))
    (let ((text (nodecode-prompt-studio::run-prompt "" "s-1" :draft "Port the export script")))
      (is (search "Port the export script" text) "the composer's text is the task"))
    (multiple-value-bind (text dialog)
        (nodecode-prompt-studio::run-prompt "" "s-1" :draft "")
      (is (search "type the task after /prompt" text) "an empty call asks for one")
      (is (consp dialog) "and offers the steps in a dialog"))))

(deftest prompt-studio-cell-a-build-without-a-task-refuses ()
  (with-cell-stop ((prompt-studio-start))
    (is (search "no task to write a prompt for"
                (princ-to-string
                 (handler-case (nodecode-prompt-studio:build-prompt
                                (nodecode-prompt-studio:brief :task "  "))
                   (nodecode-prompt-studio:prompt-studio-error (condition) condition)))) "a blank task is refused in words the model can act on")))

(deftest prompt-studio-cell-the-tool-answers-through-its-contract ()
  (with-cell-stop ((prompt-studio-start))
    (let ((entry (nle::find-tool "prompt_studio")))
      (is (consp entry) "prompt_studio is registered")
      (let* ((schema (second entry))
             (answer (funcall (third entry)
                              (nlk:contract-decode schema (nlk:make-json-object
                                                           "task" "Review the migration plan"
                                                           "deliverable" "review"
                                                           "paste" "the diff"
                                                           "model" "opus")))))
        (is (search "Review the migration plan" answer) "the task is in the prompt the tool answers")
        (is (search "<pasted_content id=" answer) "written by the rules of the model it was told about")
        (is (search "next step:" answer) "and it names the next step the wizard would ask"))
      (is (signals-error nlk:contract-refusal
            (nlk:contract-decode (second entry) (nlk:make-json-object "task" ""))) "a blank task is refused before the body runs")
      (is (signals-error nlk:contract-refusal
            (nlk:contract-decode (second entry) (nlk:make-json-object "task" "x" "deliverable" "vibes"))) "a deliverable outside the set is refused"))))

(deftest prompt-studio-cell-polish-is-one-side-call ()
  (with-cell-stop ((prompt-studio-start))
    (let ((asked nil))
      (with-stubbed-fdefinition (nle:complete (system user &key session-id max-tokens fallback-p)
                                  (setf asked (list system user))
                                  (format nil "~a~%POLISHED~%" user))
        (let ((text (nodecode-prompt-studio::run-prompt "Fix the bug; polish: t" "s-1")))
          (is (search "POLISHED" text) "the polished prompt is the answer")
          (is (search "same sections" (first asked)) "under the polishing instruction")
          (is (search "TASK" (second asked)) "with the built prompt as the one message"))))))

(deftest prompt-studio-cell-a-lost-section-keeps-the-built-prompt ()
  (with-cell-stop ((prompt-studio-start))
    (with-stubbed-fdefinition (nle:complete (system user &key session-id max-tokens fallback-p)
                                "TASK
only the task came back")
      (let ((text (nodecode-prompt-studio::run-prompt "Fix the bug; polish: t" "s-1")))
        (is (search "the polished prompt lost" text) "the note names the sections that went missing")
        (is (search "show the commands you ran" text) "and the built prompt is the one kept")
        (is (not (search "only the task came back" text)) "the half answer is not the one shown")))))

(deftest prompt-studio-cell-the-steps-report-recommends ()
  (with-cell-stop ((prompt-studio-start))
    (is (search "deliverable: What should you get back?" (nodecode-prompt-studio:steps-report nil)) "every step carries its question")
    (is (search "recommended: auto reads it off the task" (nodecode-prompt-studio:steps-report nil)) "and its recommendation")
    (is (search "answer: implementation" (nodecode-prompt-studio:steps-report
                                          (nodecode-prompt-studio::parse-answers
                                           "Fix it; deliverable: implementation"))) "and the answer typed replaces it")))

;;;; cell.lisp --- the witness cell: the evidence gate, its verbs and its one declaration.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Two seams and a tool. NLK:RECORD-DEFINITIONS is advised, so the moment
;;;; the agent keeps a prose define-skill the skill is graded against what
;;;; this session's tools actually did; a grade D moves the file into the
;;;; quarantine folder beside its cell (mode gate) and says so in the model's
;;;; prompt. The :TOOL point keeps the debug trace (tool names and argument
;;;; keys, never values). The `witness' tool and /witness read the grades
;;;; back; trust and release are the operator's, in /witness only.

(in-package #:nodecode-witness)

(defparameter +usage+
  "/witness [report | show NAME | trust NAME | release NAME | mode [gate|observe] | verify | trace | grade NAME]")

(defparameter +manual+
  "The witness cell grades what the agent keeps. When a prose define-skill is written the definitions it kept are read and the skill's own words are set against this session's tool trail: the last work call failing, the same error twice, a verify script written and never run, a 'verified' claim with no passing command behind it, a push/deploy/delete that drops a rule the operator's own memories state, placeholders, unsupported confidence. A grade is A (a passing command in the trail), B (no negative evidence), C (weak) or D (contradicted). In mode gate a grade-D skill's file is moved to <dir>/witness-quarantine/ beside its cell and taken out of the index; /witness release NAME puts it back. Every grade and every human action is a line of a sha256-chained ledger; /witness verify reads the chain. Usage: /witness report (every grade, worst first), show NAME, grade NAME (grade it again now), trust NAME, release NAME, mode [gate|observe], verify, trace."
  "What (help :witness) answers while the cell runs.")

(defparameter +tool-manual+
  "Grade a skill against the tool trail of this session, and read the grades the witness cell kept. action=grade grades skill (a name the index lists) or text (skill prose given here) and answers a row with its findings; action=report lists every grade, worst first; action=show answers one skill's findings; action=verify checks the chained ledger; action=trace shows the last tool names the cell was told about. Grades: A verified by a passing command, B no negative evidence, C weak, D contradicted by the trail - and in gate mode a D skill's file is moved out of the index until the operator releases it."
  "The tool's description.")

;;; --- the tool's one shape ---------------------------------------------------------------

(nlk:define-contract witness-call
  (:doc +tool-manual+)
  ("action" :text :required t :options '("grade" "report" "show" "verify" "trace")
            :doc "what to do: grade a skill, report every grade, show one, verify the ledger, trace the hooks")
  ("skill" :string :default "" :doc "the skill as the index lists it, for grade and show")
  ("text" :string :default "" :doc "skill prose to grade instead of a kept skill"))

(defun witness-verb (call)
  "The `witness' tool: a record materialized from WITNESS-CALL's declaration."
  (let ((action (witness-call-action call))
        (skill (string-downcase (string-trim " " (witness-call-skill call))))
        (body (witness-call-text call)))
    (nlk:with-handlers ((witness-error (condition) (format nil "witness: ~a" condition)))
      (cond ((string= action "grade")
             (if (and (zerop (length skill)) (zerop (length body)))
                 "witness: grade needs a skill name the index lists, or text to grade"
                 (let ((record (grade-named (if (plusp (length skill)) skill "text")
                                            :session-id nlk:*scribe-session-id*
                                            :text (and (plusp (length body)) body))))
                   (if record
                       (format nil "~a~%~a" (record-row (if (plusp (length skill)) skill "text") record)
                               (show-text (if (plusp (length skill)) skill "text")))
                       "witness: no such skill is kept"))))
            ((string= action "report") (report-text))
            ((string= action "show") (show-text skill))
            ((string= action "verify") (verify-text))
            ((string= action "trace") (trace-text))))))

;;; --- /witness, the operator's verbs ------------------------------------------------------

(defun run-slash (args session-id)
  "One /witness invocation: the shared report, then the verbs only a person types."
  (running-settings)
  (let* ((words (nlk:split-words args))
         (head (first words))
         (rest (string-trim " " (subseq (or args "") (min (length (or args ""))
                                                          (+ (length (or head ""))
                                                             (if head 1 0)))))))
    (nlk:with-handlers ((witness-error (condition) (format nil "witness: ~a" condition)))
      (cond ((null head) (report-text))
            ((string-equal head "report") (report-text))
            ((string-equal head "show") (show-text (string-downcase rest)))
            ((string-equal head "grade") (or (grade-named (string-downcase rest) :session-id session-id)
                                            (format nil "witness: no skill named ~a is kept" rest)))
            ((string-equal head "trust") (trust-named (string-downcase rest)))
            ((string-equal head "release") (release-named (string-downcase rest)))
            ((string-equal head "mode") (set-mode rest))
            ((string-equal head "verify") (verify-text))
            ((string-equal head "trace") (trace-text))
            (t (format nil "witness: unknown verb ~a; usage ~a" head +usage+))))))

(defun set-mode (word)
  "Read the mode, or write it into the section and say what makes it live."
  (let ((word (string-downcase (string-trim " " (or word "")))))
    (cond ((zerop (length word))
           (format nil "mode: ~a  (gate | observe)" (setting :mode)))
          ((member word '("gate" "observe") :test #'string=)
           (handler-case
               (progn (nle:config-set '("witness") (list :mode word))
                      (format nil "mode: ~a; (restart-cells \"nodecode-witness\") makes it live" word))
             (error (condition) (format nil "mode: ~a (writing the config failed: ~a)" (setting :mode) condition))))
          (t (format nil "mode must be gate or observe, not ~a" word)))))

;;; --- the seams ----------------------------------------------------------------------------

(defun trace-call (op next)
  "The :TOOL point: keep the call's name and argument keys -- never a value."
  (handler-case
      (let ((arguments (getf op :arguments)))
        (push (list :tool (getf op :name)
                    :keys (and (hash-table-p arguments)
                               (sort (loop for key being the hash-keys of arguments collect key)
                                     #'string<)))
              *trace*)
        (when (> (length *trace*) 200) (setf *trace* (subseq *trace* 0 200))))
    (error () nil))
  (funcall next op))

(defun watch-keeps (next &rest args)
  "Advice on NLK:RECORD-DEFINITIONS: after the keep is written, grade a prose
skill it wrote. A judging error never costs the definition its write."
  (prog1 (apply next args)
    (handler-case
        (let ((form (first args)))
          (when (and (prose-skill-p form) *witness*)
            (grade-named (skill-name-of form) :session-id (getf (cdddr args) :session-id))))
      (error () nil))))

;;; --- the one declaration ----------------------------------------------------------------

(nle:define-cell witness
  (:section ("witness")
    (:guide "nothing here is required; mode gate moves a grade-D skill's file into witness-quarantine beside its cell, observe only labels it; dir is where the grades and the ledger live")
    ("dir" :string :default ""
           :doc "where the grades, the ledger and the quarantine live; empty is witness/ under this home")
    ("mode" :choice :options '("gate" "observe") :default "gate"
            :doc "gate quarantines a grade-D skill; observe only labels it")
    ("min_uses" :integer :default 3 :min 1
                :doc "uses a skill needs before it can be called degraded")
    ("degraded_percent" :integer :default 60 :min 0
                        :doc "the failed share of those uses, in percent"))
  (:tool "witness" (nlk:contract 'witness-call) 'witness-verb)
  (:hook :tool #'trace-call)
  (:hook 'nlk:record-definitions #'watch-keeps)
  (:command "witness" 'run-slash
            :description "Every skill the agent kept, graded, with the evidence"
            :argument-hint "report | show NAME | trust NAME | release NAME | mode | verify | trace")
  (:help :witness "witness: grades the skills the agent kept against the tool trail - /witness report" +manual+)
  (:start (lambda ()
            (ensure-directories-exist (witness-directory))
            (setf *records* (load-records)
                  *trace* '())
            (nle:notice nil :key "nodecode-witness")
            (nle:on-stop (lambda () (setf *records* nil *trace* nil))))))

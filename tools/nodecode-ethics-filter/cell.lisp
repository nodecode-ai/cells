;;;; cell.lisp --- the ethics_evaluate tool, /ethics, and the section they run on.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original registers one tool and hands /ethics to its bundled skill, so
;;;; the reasoning happens in the conversation instead of in a hidden nested
;;;; call. Here that is the tool's own shape: called with only a decision it
;;;; answers the brief (which modules apply, what each scores against, the
;;;; thresholds), and the model scores inline and calls again with scores for
;;;; the deterministic, audited verdict.

(in-package #:nodecode-ethics-filter)

(defparameter +tool-name+ "ethics_evaluate"
  "The tool's name, the original plugin's, so a prompt written for it still reads right.")

(defparameter +primer+
  "The Ethics Filter scores a decision across six modules and answers GREEN (proceed),
AMBER (flag for a person) or RED (block). The tool is ethics_evaluate.

Two calls, one decision. First call it with the action (and any context: who is
affected, the constraints, the facts) and no scores: it answers which modules apply
to THIS decision, the rubric of each, and the thresholds in force. Score every
enabled module 0-100 against its rubric, band by band -- 0-25 severe, 26-50 weak,
51-75 acceptable, 76-100 strong -- then call again with scores to get the verdict,
which is appended to the audit trail. Give the numbers' reasons in your reply: the
reasoning is the value, not the number.

The modules: environmental (carbon, waste, water, lifecycle), fairness (who is
affected and how the benefits and burdens land), transparency (the publicity
test, traceability, accountability, conflicts), conscious-leadership (above or
below the line, values, long term), ethical-framework (utilitarian, rights,
fairness, common good, virtue) and compliance (law, certification, audit
readiness, reporting). A module the decision does not touch is not scored: pick a
constitution preset that leaves it out, or give it a score only if it applies.

The overall score is the mean of the enabled modules, and one module scoring
below the red threshold blocks the whole decision whatever the mean says. A
spread of 40 points or more between modules is a tension: name it and say what
would resolve it. Nothing here is a rubber stamp -- when in doubt take the
stricter call, and say plainly when a human has to decide."
  "What (help :ethics) answers while the cell runs.")

;;; --- the shape of one call --------------------------------------------------

(nlk:define-contract ethics-evaluation
  (:doc "Evaluate a decision through the Ethics Filter: six ethical modules (environmental, fairness, transparency, conscious leadership, ethical framework, compliance), strictness thresholds, tension detection and an audit trail. Call it with just the action for the evaluation brief -- which modules apply, their rubrics, the thresholds, the JSON shape to answer in -- then score every enabled module 0-100 and call again with scores for the deterministic verdict GREEN/AMBER/RED, which is appended to the audit trail. Any module below the red threshold blocks the whole decision.")
  ("action" :text :required t
            :doc "the proposed action or decision, one or two sentences")
  ("context" :text
             :doc "who is affected, the constraints, and the facts a scorer needs")
  ("constitution" :text
                   :options '("personal-reflection" "small-business-ethical"
                             "corporate-governance" "startup-quick" "maximalist"
                             "minimal-safe")
                   :doc "which preset decides the modules in play and how strict the verdict is; omit for the section's default_constitution")
  ("strictness" :text :options '("constitution" "relaxed" "moderate" "strict")
                 :doc "override the preset's thresholds; constitution keeps the preset's own")
  ("scores" :object
            :doc "a 0-100 score for every enabled module, e.g. {\"fairness\": 90, \"compliance\": 60}; with it the answer is the audited verdict")
  ("reasoning" :text
               :doc "one or two sentences on what decided the scores, kept in the audit record"))

;;; --- the tool ----------------------------------------------------------------

(defun evaluate-decision (call)
  "The ethics_evaluate tool: a brief without scores, the audited verdict with them."
  (let ((action (ethics-evaluation-action call))
        (context (or (ethics-evaluation-context call) ""))
        (constitution (or (ethics-evaluation-constitution call) (setting :default-constitution)))
        (strictness (or (ethics-evaluation-strictness call) "constitution"))
        (scores (ethics-evaluation-scores call))
        (reasoning (or (ethics-evaluation-reasoning call) "")))
    (if scores
        (format-verdict-text (verdict action context scores constitution strictness
                                      :reasoning reasoning))
        (format-brief-text (brief action context constitution)))))

;;; --- /ethics -----------------------------------------------------------------

(defun split-args (text)
  "TEXT as words, a double-quoted run kept whole: what a person types after
/ethics, one token at a time."
  (let ((words '()) (current (make-string-output-stream)) (in-string nil) (started nil))
    (flet ((finish ()
             (when started
               (push (get-output-stream-string current) words)
               (setf started nil))))
      (loop for char across text do
        (cond ((char= char #\")
               (setf in-string (not in-string) started t))
              ((and (not in-string) (member char '(#\Space #\Tab)))
               (finish))
              (t (write-char char current) (setf started t))))
      (finish))
    (nreverse words)))

(defun parse-score-list (text)
  "'fairness=90, compliance=60' as the score alist the verdict reads, or a
refusal naming the pair that is not a number."
  (loop for pair in (uiop:split-string (or text "") :separator ",")
        for at = (position #\= pair)
        do (unless at
             (fail "a score is written module=number, as in fairness=90: '~a' is not." pair))
        collect (let ((name (nlk:trimmed (subseq pair 0 at)))
                       (value (ignore-errors (read-from-string (nlk:trimmed (subseq pair (1+ at)))))))
                  (unless (realp value)
                    (fail "the score for '~a' is not a number: '~a'." name
                          (nlk:trimmed (subseq pair (1+ at)))))
                  (cons name value))))

(defun parse-command-args (text)
  "The /ethics arguments as (values ACTION CONTEXT CONSTITUTION STRICTNESS SCORES AUDIT-P)."
  (let ((words (split-args text)) (action '()) (context "") (constitution nil)
        (strictness nil) (scores nil) (audit nil))
    (loop while words do
      (let ((word (pop words)))
        (cond ((string-equal word "--context") (setf context (or (pop words) "")))
              ((string-equal word "--constitution") (setf constitution (pop words)))
              ((string-equal word "--strictness") (setf strictness (pop words)))
              ((string-equal word "--scores") (setf scores (parse-score-list (pop words))))
              ((string-equal word "--audit") (setf audit t))
              (t (push word action)))))
    (values (nlk:trimmed (format nil "~{~a~^ ~}" (nreverse action)))
            context constitution strictness scores audit)))

(defun usage-text ()
  "What /ethics answers with no decision: the presets and how to type a call."
  (with-output-to-string (out)
    (format out "/ethics <decision> [--context \"...\"] [--constitution ID] [--strictness LEVEL] [--scores module=score,...] [--audit]~%~%
The decision goes in plain words after the command. Without --scores it answers the~%
evaluation brief, which is what the agent scores in its reply; with --scores it~%
answers the audited verdict. --audit lists the newest records of the trail.~%~%Constitutions:~%")
    (dolist (preset +presets+)
      (format out "  ~20a ~a (~a)~%" (getf preset :id) (getf preset :name)
              (getf preset :strictness)))
    (format out "~%Strictness: ~{~a~^, ~} -- a level of its own, or `constitution' to keep the preset's.~%"
            +strictness-ids+)))

(defun run-slash (args session-id)
  "One /ethics invocation."
  (declare (ignore session-id))
  (multiple-value-bind (action context constitution strictness scores audit)
      (parse-command-args (or args ""))
    (cond ((plusp (length action))
           (let ((preset (or constitution (setting :default-constitution))))
             (if scores
                 (format-verdict-text (verdict action context scores preset
                                               (or strictness "constitution")))
                 (format-brief-text (brief action context preset)))))
          (audit (format-audit-text (recent-audit 10)))
          (t (usage-text)))))

;;; --- the cell ------------------------------------------------------------------

(nle:define-cell ethics-filter
  (:section ("ethics-filter")
    (:guide "nothing here is required: small-business-ethical scores every module, and company-governance presets live in the tool's constitution argument; audit_path is where verdicts are kept, defaulting to a file under this home")
    ("default_constitution" :choice
                             :options '("personal-reflection" "small-business-ethical"
                                       "corporate-governance" "startup-quick"
                                       "maximalist" "minimal-safe")
                             :default "small-business-ethical"
                             :doc "the preset a call uses when it names none")
    ("strictness" :choice
                   :options '("constitution" "relaxed" "moderate" "strict")
                   :default "constitution"
                   :doc "a level of its own, or constitution to keep the preset's")
    ("audit_path" :path :default "ethics-filter/audit.jsonl"
                   :doc "where scored verdicts are appended, one JSON line each; under this home unless absolute"))
  (:tool +tool-name+ (nlk:contract 'ethics-evaluation) 'evaluate-decision)
  (:help :ethics "ethics: ethics_evaluate scores a decision across six modules and answers GREEN, AMBER or RED" +primer+)
  (:command "ethics" 'run-slash
            :description "Score a decision: the evaluation brief, or the audited verdict with scores"
            :argument-hint "<decision> [--context \"...\"] [--scores fairness=90,...]")
  (:start (lambda ()
            ;; The folder the trail lives in exists from the first start, so the
            ;; operator can find it before the first verdict.
            (ensure-directories-exist (audit-file)))))

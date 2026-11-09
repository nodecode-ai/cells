;;;; engine.lisp --- relevance, the verdict, the brief and the audit trail.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original's engine.py, in the same three jobs. Relevance decides which
;;;; modules a decision touches; the evaluation turns one 0-100 score per
;;;; relevant module into a verdict by the strictness thresholds, with one red
;;;; module blocking the whole decision whatever the mean says; and every
;;;; scored verdict is appended to a JSONL audit file so a decision stays
;;;; traceable. The engine never invents a score: it makes scoring consistent
;;;; and auditable, and the model or the person supplies the numbers.

(in-package #:nodecode-ethics-filter)

;;; --- small helpers ----------------------------------------------------------

(defun contains-p (needle text)
  "T when NEEDLE occurs in TEXT, case-sensitively: both are already lowercased."
  (and (<= (length needle) (length text)) (not (null (search needle text)))))

(defun utc-stamp (&optional (universal (get-universal-time)))
  "UNIVERSAL as an ISO-8601 stamp in UTC, the shape the audit trail keeps."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time universal 0)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
            year month day hour minute second)))

(defun audit-file ()
  "The audit trail a verdict is appended to: the section's audit_path, which is
under the home unless it is absolute."
  (nlk:home (nlk:trimmed (or (setting :audit-path) "ethics-filter/audit.jsonl"))))

;;; --- relevance --------------------------------------------------------------

(defun check-relevance (action context module-name)
  "T when MODULE-NAME applies to this decision. A module with no inclusion
keywords is relevant unless one of its phrases says otherwise; one with them is
relevant only when one occurs in the decision text."
  (let ((module (module-info module-name)))
    (when module
      (let ((text (string-downcase (format nil "~a ~a" action (or context "")))))
        (cond ((some (lambda (phrase) (contains-p (string-downcase phrase) text))
                     (getf module :exclude-when))
               nil)
              ((null (getf module :relevant-when)) t)
              (t (some (lambda (keyword) (contains-p keyword text))
                       (getf module :relevant-when))))))))

(defun enabled-modules (action context preset)
  "The modules PRESET enables that also apply to this decision, in the
original's module order."
  (loop for (name . active) in (getf preset :modules)
        when (and active (check-relevance action context name)) collect name))

;;; --- thresholds -------------------------------------------------------------

(defun thresholds (strictness)
  "The thresholds level STRICTNESS means, as (:red-below N :green-at-or-above N)."
  (let ((level (strictness-info strictness)))
    (unless level
      (fail "unknown strictness '~a': it is one of ~{~a~^, ~}." strictness +strictness-ids+))
    (list :red-below (getf level :red-below)
          :green-at-or-above (getf level :green-at-or-above))))

(defun classify (score thresholds)
  "SCORE as :red, :amber or :green, by the thresholds in force."
  (cond ((< score (getf thresholds :red-below)) :red)
        ((>= score (getf thresholds :green-at-or-above)) :green)
        (t :amber)))

(defun module-flags (name score thresholds)
  "What one module's score says about itself, in the audit record."
  (case (classify score thresholds)
    (:red (list (format nil "~a: score ~a is below the ~a red threshold"
                         name (round score) (getf thresholds :red-below))))
    (:amber (list (format nil "~a: score ~a warrants review before proceeding"
                           name (round score))))
    (t nil)))

;;; --- scores -----------------------------------------------------------------

(defun canonical-module-name (key)
  "KEY as the module name it means: a score map written with underscores
(conscious_leadership) reads as the canonical hyphenated name."
  (let* ((text (string-downcase (if (keywordp key) (symbol-name key) (princ-to-string key))))
         (dashed (substitute #\- #\_ text)))
    (cond ((member text (module-names) :test #'string=) text)
          ((member dashed (module-names) :test #'string=) dashed)
          (t text))))

(defun score-alist (scores)
  "SCORES, a JSON object (hash table) or an alist, as an alist of
(name . number), the names canonical."
  (cond ((null scores) nil)
        ((hash-table-p scores)
         (loop for key being the hash-keys of scores using (hash-value value)
               collect (cons (canonical-module-name key) value)))
        ((listp scores)
         (loop for (key . value) in scores
               collect (cons (canonical-module-name key) value)))
        (t nil)))

(defun validate-scores (scores enabled)
  "Refuse a score map that leaves an enabled module unscored, names a module
that does not exist, or carries a score outside 0-100."
  (let ((missing (remove-if (lambda (name) (assoc name scores :test #'string=)) enabled)))
    (when missing
      (fail "missing scores for enabled modules: ~{~a~^, ~}. Provided: ~{~a~^, ~}. Score every enabled module 0-100, or choose a constitution that leaves out the modules that do not apply."
            missing (or (mapcar #'car scores) (list "none")))))
  (dolist (pair scores)
    (let ((name (car pair)) (value (cdr pair)))
      (unless (member name (module-names) :test #'string=)
        (fail "unknown module '~a' in scores: the modules are ~{~a~^, ~}."
              name (module-names)))
      (unless (realp value)
        (fail "the score for '~a' is not a number: ~s." name value))
      (unless (<= 0 value 100)
        (fail "the score for '~a' is ~a; a score is 0-100." name value)))))

;;; --- the verdict ------------------------------------------------------------

(defun verdict (action context scores preset-id strictness &key (reasoning "") (audit t))
  "Evaluate one decision end to end and answer the result as a plist.

SCORES is a JSON object or an alist of module -> 0-100. Every module the
preset enables and the decision touches must carry one. The overall score is
the mean of the relevant scores; the verdict is the mean against the
strictness thresholds, and any single module in the red zone blocks it
whatever the mean says. With AUDIT the record is appended to the trail."
  (let* ((preset (or (preset-info preset-id) (preset-info "small-business-ethical")))
         (level (if (and strictness (string/= strictness "constitution"))
                    strictness
                    (getf preset :strictness)))
         (limits (thresholds level))
         (enabled (enabled-modules action context preset))
         (given (score-alist scores)))
    (unless given
      (fail "no scores: pass a 0-100 score for every enabled module (~{~a~^, ~}) to get a verdict." enabled))
    (validate-scores given enabled)
    (let* ((results (loop for name in enabled
                          for score = (cdr (assoc name given :test #'string=))
                          collect (list :name name
                                        :score (coerce score 'double-float)
                                        :rationale ""
                                        :flags (module-flags name (float score) limits))))
           (overall (/ (reduce #'+ results :key (lambda (result) (getf result :score)))
                       (length results)))
           (decision (classify overall limits))
           (red (remove-if-not (lambda (result) (eq (classify (getf result :score) limits) :red))
                                results)))
      ;; One red module blocks the whole decision: a mean must never dilute a
      ;; genuine red flag into a pass.
      (when red (setf decision :red))
      (let ((result (list :timestamp (utc-stamp)
                          :action action
                          :context (or context "")
                          :constitution (getf preset :id)
                          :constitution-name (getf preset :name)
                          :strictness level
                          :thresholds limits
                          :enabled-modules enabled
                          :module-results results
                          :overall-score overall
                          :decision decision
                          :flags (mapcar (lambda (r) (getf r :name)) red)
                          :tensions (tensions results)
                          :reasoning (or reasoning "")
                          :requires-human (not (null red)))))
        (when audit (append-audit result))
        result))))

(defun tensions (results)
  "Material disagreement between the modules: a spread of 40 points or more
means they are not describing the same decision."
  (when (>= (length results) 2)
    (let* ((scores (mapcar (lambda (result) (getf result :score)) results))
           (spread (- (reduce #'max scores) (reduce #'min scores))))
      (when (>= spread 40)
        (let ((high (reduce (lambda (a b) (if (> (getf a :score) (getf b :score)) a b)) results))
              (low (reduce (lambda (a b) (if (< (getf a :score) (getf b :score)) a b)) results)))
          (list (format nil "Module disagreement: ~a (~a) vs ~a (~a) -- resolve before acting."
                         (getf high :name) (round (getf high :score))
                         (getf low :name) (round (getf low :score)))))))))

;;; --- the brief --------------------------------------------------------------

(defun rubric-text (module)
  "MODULE's rubric as the model reads it: its question, then each criterion
with the four bands a score has to sit in."
  (with-output-to-string (out)
    (format out "### ~a (~a)~%~a~%Question: ~a~%"
            (getf module :name) (getf module :title)
            (getf module :description) (getf module :question))
    (dolist (criterion (getf module :criteria))
      (format out "- ~a~%" (first criterion))
      (if (cdr criterion)
          (loop for band in +bands+ for text in (rest criterion)
                do (format out "  ~a: ~a~%" band text))
          (format out "  ~a~%" (second criterion))))
    (terpri out)))

(defun evaluation-prompt (action context preset limits enabled)
  "The prompt the original's build_evaluation_prompt composes: the decision, the
constitution, the thresholds, the rubrics of the enabled modules, and the rules
that keep a score honest."
  (with-output-to-string (out)
    (format out "You are the Ethics Filter, a structured ethical evaluation engine.~%~%
Evaluate the proposed decision against each enabled module's rubric. For every~%
module, assign a score from 0 to 100 (higher = more ethical on that dimension)~%
and give a concise rationale. Scores are structured thinking aids, not~%
measurements -- the reasoning matters more than the number.~%~%
## Decision to evaluate~%Action: ~a~%~@[Context: ~a~%~]~%
## Constitution~%Preset: ~a (~a)~%Strictness: ~a~%
- GREEN (proceed): overall score >= ~a~%
- AMBER (flag for human judgment): ~a <= overall < ~a~%
- RED (block, escalate to human): overall < ~a~%~%
## Enabled modules (~d)~%~{~a~^, ~}~%~%
## Module rubrics~%"
            action context (getf preset :name) (getf preset :id)
            (getf preset :strictness)
            (getf limits :green-at-or-above) (getf limits :red-below)
            (getf limits :green-at-or-above) (getf limits :red-below)
            (length enabled) enabled)
    (dolist (name enabled) (princ (rubric-text (module-info name)) out))
    (format out "~%
## Output format~%
Score every enabled module and answer, in your reply, as JSON:~%
{\"module_results\": [{\"name\": \"<module>\", \"score\": <0-100>, \"rationale\": \"<why>\"}],~%
 \"reasoning\": \"<what decides it>\"}~%~%
Rules:~%
- Score every enabled module. Do not skip modules.~%
- Anchor each score to a specific rubric band: say which band the score falls in~%
  (0-25, 26-50, 51-75, 76-100) and the evidence for it. Do not default to~%
  70-90 for everything.~%
- Be hard on red flags: if a criterion for a module clearly fails, that module~%
  scores below ~a, and one red module blocks the decision.~%
- If modules disagree materially, say so in the reasoning; a tension of 40~%
  points or more is flagged for a person.~%
- When in doubt prefer the stricter call: a pass should be earned.~%"
            (getf limits :red-below))))

(defun brief (action context preset-id)
  "The evaluation worksheet for a decision that has no scores yet: which modules
apply, what each one scores against, and the shape the scores come back in."
  (let* ((preset (or (preset-info preset-id) (preset-info "small-business-ethical")))
         (limits (thresholds (getf preset :strictness)))
         (enabled (enabled-modules action context preset)))
    (list :action action
          :context (or context "")
          :constitution (getf preset :id)
          :constitution-name (getf preset :name)
          :strictness (getf preset :strictness)
          :thresholds limits
          :enabled-modules enabled
          :prompt (evaluation-prompt action context preset limits enabled))))

;;; --- reading back ------------------------------------------------------------

(defun format-brief-text (worksheet)
  "WORKSHEET as the lines the model and the operator read: what applies, then
its rubrics and the shape the scores come back in."
  (let ((limits (getf worksheet :thresholds)))
    (format nil "Ethics Filter: evaluation brief -- score each enabled module 0-100 against its~%
rubric and deliver the verdict in your reply.~%
Action: ~a~%
Constitution: ~a (~a)~%
Enabled modules: ~{~a~^, ~}~%
Thresholds: RED < ~a, GREEN >= ~a~%~%~a~%
To get the audited verdict, call ethics_evaluate again with scores for every enabled~%
module; that answer is the verdict, and it is appended to the audit trail.~%"
            (getf worksheet :action)
            (getf worksheet :constitution-name) (getf worksheet :strictness)
            (getf worksheet :enabled-modules)
            (getf limits :red-below) (getf limits :green-at-or-above)
            (getf worksheet :prompt))))

(defun format-verdict-text (result)
  "RESULT as the verdict a person reads: the decision, the mean, every module's
score, the tensions and what needs a person."
  (with-output-to-string (out)
    (format out "Ethics Filter: ~a (score ~,1f/100)~%Constitution: ~a (~a)~%Enabled modules: ~{~a~^, ~}~%"
            (string-upcase (symbol-name (getf result :decision)))
            (getf result :overall-score)
            (getf result :constitution-name) (getf result :strictness)
            (getf result :enabled-modules))
    (dolist (module (getf result :module-results))
      (format out "  ~22a ~5,1f~%" (getf module :name) (getf module :score)))
    (dolist (tension (getf result :tensions)) (format out "Tension: ~a~%" tension))
    (when (getf result :requires-human) (format out "Requires human review.~%"))
    (unless (zerop (length (getf result :reasoning)))
      (format out "Reasoning: ~a~%" (getf result :reasoning)))
    (format out "Audited: ~a~%" (audit-file))))

;;; --- the audit trail ---------------------------------------------------------

(defun audit-record (result)
  "RESULT as the JSON object the trail keeps: the whole evaluation, so a
decision can be defended long after the conversation is gone."
  (nlk:plist-json
   (list :timestamp (getf result :timestamp)
         :action (getf result :action)
         :context (getf result :context)
         :constitution (getf result :constitution)
         :strictness (getf result :strictness)
         :thresholds (list :red-below (getf (getf result :thresholds) :red-below)
                           :green-at-or-above (getf (getf result :thresholds) :green-at-or-above))
         :enabled-modules (coerce (getf result :enabled-modules) 'vector)
         :module-results (map 'vector
                              (lambda (module)
                                (list :name (getf module :name)
                                      :score (getf module :score)
                                      :rationale (getf module :rationale)
                                      :flags (coerce (getf module :flags) 'vector)))
                              (getf result :module-results))
         :overall-score (getf result :overall-score)
         :decision (string-downcase (symbol-name (getf result :decision)))
         :flags (coerce (getf result :flags) 'vector)
         :tensions (coerce (getf result :tensions) 'vector)
         :reasoning (getf result :reasoning)
         :requires-human (getf result :requires-human))))

(defun append-audit (result)
  "Append RESULT to the audit trail; answer the file it went to."
  (let ((path (audit-file)))
    (nlk:append-json-line path (audit-record result))
    path))

(defun recent-audit (&optional (count 5))
  "The newest COUNT records of the trail, oldest first."
  (let ((records (nlk:read-json-lines (audit-file))))
    (last records (min count (length records)))))

(defun format-audit-text (records)
  "RECORDS as one line each: when, the decision, the mean, and what it was."
  (if (null records)
      (format nil "The ethics audit trail at ~a holds nothing yet." (audit-file))
      (with-output-to-string (out)
        (format out "The ethics audit trail at ~a, newest last:~%" (audit-file))
        (dolist (record records)
          (format out "  ~a  ~a  ~,1f  ~a~%"
                  (or (nlk:json-value record :string "timestamp") "?")
                  (string-upcase (or (nlk:json-value record :string "decision") "?"))
                  (or (nlk:json-value record :number "overall_score") 0.0)
                  (nlk:json-value record :string "action"))))))

;;;; rules.lisp --- the drift rules: pure functions of one finished run.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original's rules.py, one function per rule, each answering a finding
;;;; or nothing: a run that said it worked without proving it, one that said
;;;; done over a failed step, one stuck in a loop, one that cost far more than
;;;; its job usually does, one that did far less work, and one that answered
;;;; without touching a tool. Nothing here reads a file, a clock or a config:
;;;; a run and, for the baseline rules, the last healthy runs of the same
;;;; job, are the whole input, so every rule is tested by handing it one.

(in-package #:nodecode-flightlog)

;;; --- what a person reads -----------------------------------------------------

(defparameter +rule-order+
  '("unverified_claim" "swallowed_errors" "loop" "cost_spike" "effort_collapse" "empty_work")
  "The rules in the order a report lists them, the one that matters most
first. The last three are the baseline rules: they speak only for a job.")

(defparameter +headlines+
  '(("unverified_claim" . "Claimed it works, but never proved it")
    ("swallowed_errors" . "Said done, but a step failed")
    ("loop" . "Stuck in a loop")
    ("cost_spike" . "Cost far above normal")
    ("effort_collapse" . "Did far less work than usual")
    ("empty_work" . "Answered without doing any work"))
  "What the operator reads as the verdict, in the original's plain words.")

(defparameter +impact+
  '(("unverified_claim" . "The change may be broken; check it before you merge or ship it.")
    ("swallowed_errors" . "The answer may be built on data the agent never got.")
    ("loop" . "It repeated itself instead of making progress, burning time and tokens.")
    ("cost_spike" . "Something made this run much more expensive than this job usually is.")
    ("effort_collapse" . "It may have skipped steps or answered from memory.")
    ("empty_work" . "It probably answered from memory instead of checking."))
  "Why each verdict matters, in one sentence, as the original writes it.")

(defun headline (rule) (or (cdr (assoc rule +headlines+ :test #'equal)) rule))
(defun impact (rule) (or (cdr (assoc rule +impact+ :test #'equal)) ""))

(defun rule-rank (rule)
  "Where RULE sits in a report's order, or past every known rule."
  (or (position rule +rule-order+ :test #'equal) (length +rule-order+)))

;;; --- the shapes the rules read ------------------------------------------------

;; A run is the recorder's struct, read here through these accessors alone, so
;; a rule never has to know how the recorder keeps it.

(defun tool-events (run)
  (remove-if-not (lambda (event) (equal "tool" (getf event :kind))) (run-events run)))

(defun model-events (run)
  (remove-if-not (lambda (event) (equal "model" (getf event :kind))) (run-events run)))

(defun error-events (run)
  (remove-if-not (lambda (event) (equal "error" (getf event :status))) (tool-events run)))

(defun times-text (count) (if (= count 1) "once" (format nil "~d times" count)))

(defun finding (rule summary evidence)
  "One finding: the rule, the sentence a person reads, and the steps that prove it."
  (list :rule rule :summary summary :evidence evidence))

(defun threshold (key default)
  "KEY's setting, or DEFAULT while the cell is not the one running."
  (let ((value (ignore-errors (setting key))))
    (if (numberp value) value default)))

(defun number-text (value)
  "VALUE with no decimals when it is whole, so a median reads as 11, not 11.0."
  (if (= value (fround value)) (format nil "~d" (round value)) (format nil "~,1f" value)))

;;; --- the patterns the original reached for ------------------------------------

(defparameter +admits+
  "(?i)\\b(errors?|fail(ed|s|ure)?|couldn'?t|could not|cannot|can'?t|unable|not found|missing|no such|denied|timed? ?out|rate.?limit\\w*|unavailable)\\b"
  "An answer that says what failed is honest, not swallowed.")

(defparameter +check+
  "(?i)\\b(pytest|unittest|tox|nox|jest|vitest|mocha|playwright|cargo (test|build|check|clippy)|go (test|build|vet)|(npm|pnpm|yarn|bun)( run)? (test|build|lint|typecheck|check)|make( \\w+)?|just (test|check|lint)|tsc|mypy|pyright|ruff|eslint|gradle|mvn|swift (test|build)|xcodebuild|rspec|phpunit|dotnet (test|build)|node --test|sbcl --script)\\b"
  "What a check command looks like: the original's list, plus the recipes a
Nodecode workspace usually has and a lisp file run on its own.")

(defparameter +claims+
  "(?i)\\b(tests? (now )?pass(es|ed|ing)?|all (tests )?(pass|green)|(build|it) (now )?(succeeds|passes|works)|works now|verified|fixed|everything (is )?green)\\b"
  "What an answer claims when it says the work is proved.")

(defparameter +edit-tools+
  '("edit" "write-file" "write_file" "patch" "apply_patch" "str_replace" "edit_file" "write")
  "The tools that change a file, as the original names them and as a cell may.")

(defparameter +check-tools+
  '("eval" "sh" "shell" "bash" "run" "exec" "terminal")
  "The tools a check command runs through.")

(defun eval-text (event)
  "The text a call carries: the form of an eval, the command of a shell tool,
which the recorder keeps as the call's target."
  (or (getf event :target) ""))

(defun edit-event-p (event)
  "T when EVENT changed a file: a tool named for editing, or an eval whose
form edits or writes one."
  (let ((name (or (getf event :name) "")))
    (or (member name +edit-tools+ :test #'string-equal)
        (and (string-equal name "eval")
             (ppcre:scan "(?i)\\((edit|write-file)\\s" (eval-text event))))))

(defun check-event-p (event)
  "T when EVENT ran something that proves work: a test, a build, a lint."
  (and (member (or (getf event :name) "") +check-tools+ :test #'string-equal)
       (ppcre:scan +check+ (eval-text event))))

(defun admits-p (answer)
  (and (stringp answer) (plusp (length answer)) (ppcre:scan +admits+ answer)))

(defun claim-of (answer)
  "The claim ANSWER makes, as it wrote it, or NIL."
  (when (and (stringp answer) (plusp (length answer)))
    (let ((match (ppcre:scan-to-strings +claims+ answer)))
      (and match (aref match 0)))))

;;; --- one rule each -----------------------------------------------------------

(defun rule-swallowed-errors (run baseline)
  "A tool errored in a run that still reported success, and the answer never
mentions it."
  (declare (ignore baseline))
  (when (run-ok-p run)
    (let ((errors (error-events run)))
      (when (and (>= (length errors) (threshold :swallowed-errors 1))
                 (not (admits-p (run-answer run))))
        (let* ((name (getf (first errors) :name))
               (same (remove-if-not (lambda (event) (equal name (getf event :name))) errors))
               (count (length same))
               (others (- (length errors) count))
               (detail (getf (first same) :detail)))
          (finding "swallowed_errors"
                   (format nil "~a failed ~a~@[, ~a~]~a, and the answer never mentions it."
                           name (times-text count) (and detail (plusp (length detail)) detail)
                           (if (plusp others)
                               (format nil ", plus ~d other error~:p" others)
                               ""))
                   (mapcar (lambda (event) (getf event :seq)) errors)))))))

(defun rule-unverified-claim (run baseline)
  "The answer claims the work is proved, and no check ran after the last
edit, or the last one failed."
  (declare (ignore baseline))
  (let ((claim (and (run-ok-p run) (claim-of (run-answer run)))))
    (when claim
      (let* ((tools (tool-events run))
             (edits (remove-if-not #'edit-event-p tools)))
        (when edits
          (let* ((last-edit (car (last edits)))
                 (after (remove-if-not (lambda (event) (and (> (getf event :seq) (getf last-edit :seq))
                                                            (check-event-p event)))
                                       tools))
                 (said (format nil "The answer says \"~a\"" claim)))
            (cond
              ((null after)
               (finding "unverified_claim"
                        (format nil "~a, but no test or build ran after the last edit (~a)."
                                said (or (getf last-edit :target) (getf last-edit :name)))
                        (list (getf last-edit :seq))))
              ((not (equal "error" (getf (car (last after)) :status)))
               nil)
              (t
               (let ((last (car (last after))))
                 (finding "unverified_claim"
                          (format nil "~a, but the last check failed: ~a~@[ (~a)~]."
                                  said (eval-text last)
                                  (let ((detail (getf last :detail)))
                                    (and detail (plusp (length detail)) detail)))
                          (list (getf last :seq))))))))))))

(defun repeated-call (events)
  "The (NAME . ARGKEY) group EVENTS repeats most, with its events and its
count; three values, or NIL when there are none."
  (let ((groups (make-hash-table :test #'equal)))
    (dolist (event events)
      (push event (gethash (cons (or (getf event :name) "") (or (getf event :argkey) ""))
                           groups)))
    (let ((best nil) (best-events nil) (best-count 0))
      (maphash (lambda (key group)
                 (when (> (length group) best-count)
                   (setf best key best-events group best-count (length group))))
               groups)
      (when best (values best (nreverse best-events) best-count)))))

(defun rule-loop (run baseline)
  "The same tool, called with near-identical arguments, over and over."
  (declare (ignore baseline))
  (multiple-value-bind (key events count) (repeated-call (tool-events run))
    (when (and key (>= count (threshold :loop-repeats 5)))
      (let ((target (or (getf (first events) :target) "")))
        (finding "loop"
                 (format nil "Called ~a~@[ (~a)~] ~a with the same arguments."
                         (car key) (and (plusp (length target)) target) (times-text count))
                 (mapcar (lambda (event) (getf event :seq)) events))))))

(defun baseline-ready-p (baseline)
  (and baseline (>= (or (getf baseline :n) 0) (threshold :min-baseline-runs 3))))

(defun rule-cost-spike (run baseline)
  "Tokens far above what the job's healthy runs usually spend."
  (when (and (baseline-ready-p baseline)
             (plusp (or (getf baseline :median-tokens) 0))
             (run-job run))
    (let* ((median (float (getf baseline :median-tokens)))
           (ratio (/ (run-tokens run) median)))
      (when (>= (* 100 ratio) (threshold :cost-spike-pct 300))
        (finding "cost_spike"
                 (format nil "Used ~:d tokens, ~,1fx this job's usual ~:d."
                         (run-tokens run) ratio (round median))
                 (mapcar (lambda (event) (getf event :seq)) (model-events run)))))))

(defun rule-effort-collapse (run baseline)
  "Far fewer tool calls than the job usually makes, short of none at all."
  (when (and (baseline-ready-p baseline)
             (plusp (or (getf baseline :median-tools) 0))
             (plusp (run-tool-calls run))
             (run-job run))
    (let ((median (float (getf baseline :median-tools))))
      (when (< (* 100 (/ (run-tool-calls run) median)) (threshold :effort-collapse-pct 25))
        (finding "effort_collapse"
                 (format nil "Made ~d tool call~:p; this job usually makes ~a."
                         (run-tool-calls run) (number-text median))
                 (mapcar (lambda (event) (getf event :seq)) (tool-events run)))))))

(defun rule-empty-work (run baseline)
  "No tool call at all, on a job that normally uses tools."
  (when (and (baseline-ready-p baseline)
             (plusp (or (getf baseline :median-tools) 0))
             (zerop (run-tool-calls run))
             (run-job run))
    (finding "empty_work"
             (format nil "Made no tool calls; this job usually makes ~a."
                     (number-text (float (getf baseline :median-tools))))
             '())))

(defparameter +rules+
  '(("unverified_claim" . rule-unverified-claim)
    ("swallowed_errors" . rule-swallowed-errors)
    ("loop" . rule-loop)
    ("cost_spike" . rule-cost-spike)
    ("effort_collapse" . rule-effort-collapse)
    ("empty_work" . rule-empty-work))
  "Each rule's name and the function that decides it.")

(defparameter +baseline-free+
  '("unverified_claim" "swallowed_errors" "loop")
  "The rules that need no history: a chat gets these and nothing else.")

(defun evaluate (run baseline silenced)
  "Every finding RUN earns, worst first, minus the rules SILENCED for its job."
  (let ((names (if (run-job run) +rule-order+ +baseline-free+)))
    (sort (remove nil
                  (loop for name in names
                        for rule = (cdr (assoc name +rules+ :test #'equal))
                        for found = (and rule (funcall rule run baseline))
                        when (and found (not (member name silenced :test #'equal)))
                          collect found))
          #'< :key (lambda (item) (rule-rank (getf item :rule))))))

(defun median (numbers)
  (let* ((sorted (sort (copy-list numbers) #'<))
         (count (length sorted)))
    (cond ((zerop count) nil)
          ((oddp count) (nth (floor count 2) sorted))
          (t (/ (+ (nth (1- (/ count 2)) sorted) (nth (/ count 2) sorted)) 2.0)))))

(defun baseline-from (rows)
  "The baseline of ROWS, the job's recent healthy runs: how many, and each
median. NIL with no rows."
  (when rows
    (list :n (length rows)
          :median-tools (median (mapcar (lambda (row) (getf row :tool-calls)) rows))
          :median-tokens (median (mapcar (lambda (row) (getf row :tokens)) rows))
          :median-duration-ms (median (mapcar (lambda (row) (getf row :duration-ms)) rows)))))

;;; --- what a finding reads like ------------------------------------------------

(defun template-report (findings)
  "The explanation a finding carries: its sentence and why it matters, in
plain words, always present."
  (format nil "~{~a~^ ~}" (mapcar (lambda (item)
                                     (format nil "~a ~a" (getf item :summary) (impact (getf item :rule))))
                                   findings)))

(defun note-text (findings run-id)
  "The paragraph a flagged run raises: the verdict, why it matters, the rest,
and where the timeline is."
  (with-output-to-string (out)
    (let ((lead (first findings)))
      (format out "flightlog: ~a. ~a ~a"
              (string-downcase (headline (getf lead :rule)))
              (getf lead :summary) (impact (getf lead :rule))))
    (dolist (item (rest findings))
      (format out "~% - ~a. ~a" (headline (getf item :rule)) (getf item :summary)))
    (format out "~%Details: /flightlog why ~a" run-id)))

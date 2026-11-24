;;;; cell.lisp --- one fixed question set, one exact test, one message when
;;;; the model you use gets quietly worse.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The shape is the original plugin's: a run asks the same exactly graded
;;;; questions, the answers are kept as one JSON line, and a run is compared
;;;; with the same model's own baseline by an exact test. The model call goes
;;;; through NLE:COMPLETE, the organism's own side call, so this cell never
;;;; sees a key and never names an endpoint.
;;;;
;;;; Read-only verbs answer strings; the tool and /model-drift-watch answer
;;;; the same report.

(in-package #:nodecode-model-drift-watch)

(defparameter +usage+
  "/model-drift-watch: is the model you use still as good as it was?

  /model-drift-watch           the last check, the baseline, and the verdict
  /model-drift-watch run       ask the questions now (spends tokens)
  /model-drift-watch estimate  what one check would cost, before it spends anything
  /model-drift-watch questions the fixed question set, and how each one is graded
  /model-drift-watch reset     forget the runs and start a new baseline"
  "What /model-drift-watch answers when it is typed alone or with a word it does not know.")

(defparameter +manual+
  "Model Drift Watch asks the same fourteen exactly graded questions (a number, a
spelling, a shape, a word count) on demand, keeps every run in one JSON line, and
compares a run with the same model's own baseline by an exact one-sided test. It
reports a drop only when the drop is at least min_drop_points, the test passes at
the section's alpha, more questions fell than rose, and -- with confirm on -- a
second run right after the first shows it too. The first baseline_runs clean runs
are the baseline; a run that is mostly provider errors is a failed measurement and
never drift. The model asked is the auxiliary target unless provider and model name
one. Answers are cut out of the reply, so the questions are asked under a fixed
instruction and graded by rule, never by a judge.
  /model-drift-watch run        ask the questions now
  /model-drift-watch estimate   tokens a check would spend, before it spends them
  /model-drift-watch questions  the question set, by category
  /model-drift-watch reset      a new baseline, after a model change you meant
Pair it with the cron cell for a scheduled check: a prompt that says
\"/model-drift-watch run\" repeats on whatever schedule you set."
  "What (help :model-drift-watch) answers while the cell runs.")

(defparameter +max-error-share+ 1/5
  "Above this share of unusable answers, a run is a failed measurement, not a score.")

;;; --- the model call ----------------------------------------------------------------

(defvar *sample* nil
  "The one model call this cell makes: (lambda (system user max-tokens) ...), or NIL
for the organism's own side call. A test binds it; nothing else does.")

(defun text-setting (key)
  "KEY's value when it is a non-empty string, else NIL."
  (let ((value (setting key)))
    (and (stringp value) (plusp (length (nv value))) (nv value))))

(defun target-name ()
  "The provider:model the next check asks, as text."
  (let ((provider (text-setting :provider))
        (model (text-setting :model)))
    (if (or provider model)
        (format nil "~a:~a" (or provider "default") (or model "default"))
        (handler-case
            (multiple-value-bind (default-provider default-model) (nle:auxiliary-model)
              (format nil "~a:~a" (or default-provider "default") (or default-model "default")))
          (error () "the configured model")))))

(defun ask (system user max-tokens)
  "SYSTEM and USER put to the watched model; its answer's text."
  (let ((provider (text-setting :provider))
        (model (text-setting :model)))
    (cond
      (*sample* (funcall *sample* system user max-tokens))
      ((or provider model)
       (multiple-value-bind (default-provider default-model) (nle:auxiliary-model)
         (let ((nle::*auxiliary-model*
                 (list :provider (or provider default-provider)
                       :model (or model default-model))))
           (nle:complete system user :max-tokens max-tokens))))
      (t (nle:complete system user :max-tokens max-tokens)))))

;;; --- the store: one JSON line a run, under the home -------------------------------

(defun state-path ()
  "The run file: the section's state_dir when it names one, else under the home."
  (let ((directory (text-setting :state-dir)))
    (merge-pathnames "runs.jsonl"
                     (if directory
                         (uiop:ensure-directory-pathname directory)
                         (nlk:home "model-drift-watch/")))))

(defun runs ()
  "Every recorded run, oldest first, as decoded JSON objects."
  (or (nlk:read-json-lines (state-path)) '()))

(defun append-run (row)
  "Record ROW as the newest run."
  (nlk:append-json-line (state-path) row))

(defun trim-runs ()
  "Keep the newest KEEP-RUNS rows, so the file stays a file."
  (let* ((all (runs))
         (keep (setting :keep-runs)))
    (when (> (length all) keep)
      (let ((path (state-path)))
        (with-open-file (out path :direction :output :if-exists :supersede
                                  :if-does-not-exist :create :external-format :utf-8)
          (dolist (row (last all keep))
            (write-line (nlk:encode-json-object row) out)))))))

(defun row-ts (row) (or (nlk:json-value row :integer "ts") 0))
(defun row-samples (row) (or (nlk:json-value row :integer "samples") 0))
(defun row-answered (row) (or (nlk:json-value row :integer "answered") 0))
(defun row-correct (row) (or (nlk:json-value row :integer "correct") 0))
(defun row-target (row) (nlk:json-value row :string "target"))
(defun row-items (row) (nlk:json-value row :array "items"))

(defun row-unusable (row)
  "The answers of ROW that could not be used at all."
  (- (row-samples row) (row-answered row)))

(defun clean-p (row)
  "Whether ROW is a measurement: no more than a fifth of its answers unusable."
  (let ((samples (row-samples row)))
    (and (plusp samples) (<= (row-unusable row) (* +max-error-share+ samples)))))

(defun ago-text (ts)
  "How long ago TS was, in a word or two."
  (let ((seconds (max 0 (- (get-universal-time) ts))))
    (cond ((< seconds 90) "just now")
          ((< seconds 5400) (format nil "~dm ago" (max 1 (round seconds 60))))
          ((< seconds 172800) (format nil "~dh ago" (round seconds 3600)))
          (t (format nil "~dd ago" (round seconds 86400))))))

(defun date-of (universal)
  "UNIVERSAL as a date."
  (multiple-value-bind (second minute hour day month year) (decode-universal-time universal)
    (declare (ignore second minute hour))
    (format nil "~4,'0d-~2,'0d-~2,'0d" year month day)))

;;; --- one run ----------------------------------------------------------------------

(defun selected-questions ()
  "The items this run asks: the section's categories, or all of them."
  (let ((categories (setting :categories)))
    (if (null categories)
        *suite*
        (remove-if-not (lambda (question)
                         (member (getf question :category) categories :test #'string-equal))
                       *suite*))))

(defun ask-quietly (question)
  "QUESTION's reply, or (values NIL TEXT) when the call itself failed."
  (multiple-value-bind (system user) (ask-for question)
    (handler-case (values (nv (ask system user (setting :max-tokens))) nil)
      (error (condition)
        (values nil (nlk:one-line (princ-to-string condition) :cap 100))))))

(defun run-question (question)
  "QUESTION asked once. => a plist of what came back."
  (multiple-value-bind (reply failure) (ask-quietly question)
    (let ((id (getf question :id)))
      (cond
        (failure (list :id id :ok nil :chars 0 :answered nil :error failure))
        ((zerop (length reply))
         (list :id id :ok nil :chars 0 :answered nil :error "an empty answer"))
        (t (let ((answer (extracted question reply)))
             (multiple-value-bind (passed detail) (grade question answer)
               (list :id id :ok (and passed t) :chars (length reply) :answered t
                     :answer (nlk:one-line answer :cap 100) :note (or detail "")))))))))

(defun outcome-json (outcome)
  "OUTCOME as the row's own JSON object."
  (nlk:json-object "id" (getf outcome :id)
                    "ok" (if (getf outcome :ok) 1 0)
                    "answered" (if (getf outcome :answered) 1 0)
                    "chars" (getf outcome :chars)
                    "answer" (or (getf outcome :answer) "")
                    "error" (or (getf outcome :error) "")
                    "note" (or (getf outcome :note) "")))

(defun one-run (questions)
  "QUESTIONS asked once, in order. => (values the ROW, the outcomes)."
  (let* ((outcomes (loop for question in questions collect (run-question question)))
         (answered (count-if (lambda (outcome) (getf outcome :answered)) outcomes))
         (correct (count-if (lambda (outcome) (getf outcome :ok)) outcomes))
         (row (nlk:json-object "ts" (get-universal-time)
                                "target" (target-name)
                                "samples" (length outcomes)
                                "answered" answered
                                "correct" correct
                                "items" (coerce (mapcar #'outcome-json outcomes) 'vector))))
    (values row outcomes)))

;;; --- measuring a run against the baseline -----------------------------------------

(defun item-counts (rows)
  "ID => (CORRECT . SAMPLES), pooled over ROWS."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (row rows)
      (let ((items (row-items row)))
        (when items
          (loop for item across items
                for id = (nlk:json-value item :string "id")
                when id
                  do (let ((counts (gethash id table)))
                       (setf (gethash id table)
                             (cons (+ (if counts (car counts) 0)
                                      (or (nlk:json-value item :integer "ok") 0))
                                   (1+ (if counts (cdr counts) 0)))))))))
    table))

(defun compare-counts (baseline current)
  "BASELINE and CURRENT item tables compared by the exact one-sided test.
=> a plist: the pooled counts, both rates, the drop in points, the p, and the
questions that fell and those that rose."
  (let ((base-correct 0) (base-samples 0) (current-correct 0) (current-samples 0)
        (regressed '()) (improved '()))
    (maphash (lambda (id counts)
               (let ((before (gethash id baseline)))
                 (when before
                   (incf base-correct (car before))
                   (incf base-samples (cdr before))
                   (incf current-correct (car counts))
                   (incf current-samples (cdr counts))
                   (let ((was (/ (car before) (float (cdr before))))
                         (now (/ (car counts) (float (cdr counts)))))
                     (cond ((and (>= was 0.6) (<= now 0.4)) (push id regressed))
                           ((and (<= was 0.4) (>= now 0.6)) (push id improved)))))))
             current)
    (let ((base-rate (rate base-correct (- base-samples base-correct)))
          (current-rate (rate current-correct (- current-samples current-correct))))
      (list :base-correct base-correct :base-samples base-samples
            :current-correct current-correct :current-samples current-samples
            :base-rate base-rate :current-rate current-rate
            :points (* 100d0 (- current-rate base-rate))
            :p (fisher-lower base-correct (- base-samples base-correct)
                             current-correct (- current-samples current-correct))
            :regressions (nreverse regressed)
            :improvements (nreverse improved)))))

(defun compare-runs (baseline current-rows)
  "The BASELINE rows against CURRENT-ROWS, with the answer-length shift too."
  (let ((comparison (compare-counts (item-counts baseline) (item-counts current-rows))))
    (setf (getf comparison :chars)
          (let ((before (mean-chars baseline))
                (now (mean-chars current-rows)))
            (and before now (plusp before) (/ now (float before)))))
    comparison))

(defun mean-chars (rows)
  "The mean answer length over ROWS, or NIL when nothing was answered."
  (let ((total 0) (answered 0))
    (dolist (row rows)
      (when (row-items row)
        (loop for item across (row-items row)
              when (eql 1 (nlk:json-value item :integer "answered"))
                do (incf total (or (nlk:json-value item :integer "chars") 0))
                   (incf answered))))
    (and (plusp answered) (/ total answered))))

(defun same-target (rows row)
  "The clean rows of ROWS measured on ROW's target."
  (remove-if-not (lambda (previous)
                    (and (clean-p previous)
                         (equal (row-target previous) (row-target row))))
                  rows))

(defun baseline-of (rows)
  "ROWS as a baseline: the newest BASELINE-RUNS of them."
  (last rows (setting :baseline-runs)))

(defun dropped-p (comparison bar)
  "Whether COMPARISON is a drop that passes the exact test at BAR."
  (and (minusp (getf comparison :points))
       (<= (getf comparison :points) (- (setting :min-drop-points)))
       (< (getf comparison :p) bar)
       (> (length (getf comparison :regressions))
          (length (getf comparison :improvements)))))

(defun alpha-value ()
  "The section's significance level, as a double."
  (or (ignore-errors (read-from-string (nv (setting :alpha)))) 0.05d0))

;;; --- the words a report is made of -------------------------------------------------

(defun score-text (comparison)
  "COMPARISON's current score in words."
  (format nil "~d of ~d correct (~d%)"
          (getf comparison :current-correct) (getf comparison :current-samples)
          (round (* 100 (getf comparison :current-rate)))))

(defun baseline-text (comparison)
  "COMPARISON's baseline in words."
  (format nil "a baseline of ~d of ~d (~d%)"
          (getf comparison :base-correct) (getf comparison :base-samples)
          (round (* 100 (getf comparison :base-rate)))))

(defun points-text (comparison)
  "COMPARISON's change in accuracy points, signed."
  (format nil "~a~d points"
          (if (plusp (getf comparison :points)) "+" "")
          (round (getf comparison :points))))

(defun p-text (p)
  "The exact one-sided p in words."
  (cond ((< p 0.0001) "p<0.0001")
        ((< p 0.01) (format nil "p=~,4f" p))
        (t (format nil "p=~,2f" p))))

(defun fell-text (comparison)
  "How many questions fell and how many rose."
  (format nil "~d question(s) fell against ~d that rose"
          (length (getf comparison :regressions))
          (length (getf comparison :improvements))))

(defun regressed-text (comparison)
  "The questions that fell, named, or nothing."
  (let ((regressed (getf comparison :regressions)))
    (if regressed (format nil " Regressed: ~{~a~^, ~}." regressed) "")))

(defun chars-text (comparison)
  "The answer-length shift, when it is worth saying."
  (let ((ratio (getf comparison :chars)))
    (if (and ratio (or (> ratio 1.15) (< ratio 0.87)))
        (format nil "~%  Answers are ~d% ~:[longer~;shorter~] than in the baseline."
                (round (abs (* 100 (- ratio 1)))) (< ratio 1))
        "")))

(defun verdict-line (comparison)
  "The one line that says whether COMPARISON is drift."
  (if (dropped-p comparison (alpha-value))
      (format nil "DRIFT: ~a against ~a, ~a lower, ~a: ~a.~a~a"
              (score-text comparison) (baseline-text comparison)
              (points-text comparison) (p-text (getf comparison :p))
              (fell-text comparison) (regressed-text comparison)
              (chars-text comparison))
      (format nil "no drift: ~a against ~a, ~a, ~a.~a"
              (score-text comparison) (baseline-text comparison)
              (points-text comparison) (p-text (getf comparison :p))
              (regressed-text comparison))))

(defun next-text (comparison)
  "What to do after a report."
  (if (dropped-p comparison (alpha-value))
      "Next: if you did not change models on purpose, this is a real drop -- questions lists what is asked. After a change you meant, reset starts a new baseline."
      "Next: the next check asks the same questions again; a real drop shows up in more than one."))

(defun result-text (row comparison &optional lead)
  "The report for one check: what it scored and what it means."
  (format nil "~@[~a~%~]~a, ~a~%  ~a~%  ~a"
          lead
          (or (row-target row) "the watched model")
          (ago-text (row-ts row))
          (verdict-line comparison)
          (next-text comparison)))

(defun collecting-text (row baseline others)
  "The report while the baseline is still being built."
  (format nil "baseline run ~d of ~d for ~a: ~d of ~d correct.~%~@[  The watched model changed (before: ~{~a~^, ~}): the baseline starts again.~%~]  The comparison starts once ~d clean runs are recorded; nothing is called drift before that."
          (1+ (length baseline)) (setting :baseline-runs)
          (or (row-target row) "the watched model")
          (row-correct row) (row-samples row)
          (and others (list others))
          (setting :baseline-runs)))

(defun failed-text (row)
  "The report for a run most of whose answers could not be used."
  (format nil "the check could not measure: ~d of ~d answers unusable~@[ (first: ~a)~]."
          (row-unusable row) (row-samples row)
          (loop for item across (or (row-items row) #())
                for error = (nlk:json-value item :string "error")
                when (and error (plusp (length error))) do (return error))))

;;; --- the verbs ---------------------------------------------------------------------

(defun check ()
  "Ask the questions now and say what it means. => the report as a string."
  (let ((questions (selected-questions)))
    (when (null questions)
      (fail "no question is in the categories ~s; clear the categories setting to ask all of them"
            (setting :categories)))
    (let ((before (runs)))
      (multiple-value-bind (row outcomes) (one-run questions)
        (declare (ignore outcomes))
        (append-run row)
        (trim-runs)
        (cond
          ((not (clean-p row))
           (format nil "~a~%  Nothing was decided: a failed measurement never counts as drift, and it does not join the baseline. Next: run again."
                   (failed-text row)))
          (t
           (let* ((baseline (baseline-of (same-target before row))))
             (if (< (length baseline) (setting :baseline-runs))
                 (collecting-text row baseline
                                  (remove-duplicates
                                   (loop for previous in before
                                         for target = (row-target previous)
                                         unless (or (null target) (equal target (row-target row)))
                                           collect target)
                                   :test #'equal))
                 (let ((comparison (compare-runs baseline (list row))))
                   (cond
                     ((not (dropped-p comparison 0.05))
                      (result-text row comparison))
                     ((not (setting :confirm))
                      (result-text row comparison
                                   (format nil "It looks worse, but confirm is off, so this is a possible drop, not drift.")))
                     (t
                      (multiple-value-bind (second second-outcomes) (one-run questions)
                        (declare (ignore second-outcomes))
                        (append-run second)
                        (trim-runs)
                        (let ((together (compare-runs baseline (list row second)))
                              (again (compare-runs baseline (list second))))
                          (if (and (dropped-p together (alpha-value))
                                   (<= (getf again :points) (- (/ (setting :min-drop-points) 2))))
                              (result-text second together "A second run confirmed it.")
                              (format nil "possible drift, not confirmed: the first run scored ~a, the second ~a, and together ~a.~%  ~a~%  Next: one more check settles it."
                                      (score-text comparison) (score-text again)
                                      (verdict-line together)
                                      (regressed-text together))))))))))))))))

(defun status ()
  "The last check, the baseline and the verdict, from the recorded runs. Free."
  (let ((history (runs)))
    (if (null history)
        (format nil "model-drift-watch: nothing measured yet~@[ against ~a~].~%  run asks the ~d fixed questions and starts the baseline of ~d runs; estimate says what that spends."
                (target-name) (length *suite*) (setting :baseline-runs))
        (let* ((row (car (last history)))
               (earlier (same-target (butlast history) row))
               (baseline (baseline-of earlier))
               (recorded (+ (if (clean-p row) 1 0) (length earlier)))
               (unusable (row-unusable row)))
          (with-output-to-string (out)
            (format out "model-drift-watch: ~a~%" (or (row-target row) "the watched model"))
            (format out "  last check: ~a, ~d of ~d correct~@[ (~d unusable)~]~%"
                    (ago-text (row-ts row)) (row-correct row) (row-samples row)
                    (and (plusp unusable) unusable))
            (if (< recorded (setting :baseline-runs))
                (format out "  baseline: ~d of ~d runs recorded, since ~a; the comparison starts at ~d.~%"
                        recorded (setting :baseline-runs)
                        (date-of (row-ts (or (first (or baseline earlier)) row)))
                        (setting :baseline-runs))
                (let ((comparison (compare-runs baseline (list row))))
                  (format out "  baseline: ~d runs, ~d of ~d correct, since ~a~%"
                          (length baseline) (getf comparison :base-correct)
                          (getf comparison :base-samples)
                          (date-of (row-ts (first baseline))))
                  (format out "  verdict: ~a~%" (verdict-line comparison))))
            (format out "  next: run checks again; questions lists the set; reset starts a new baseline."))))))

(defun estimate ()
  "What one check would spend, before it spends anything. Free."
  (let* ((questions (selected-questions))
         (ceiling (setting :max-tokens))
         (input (loop for question in questions
                       sum (multiple-value-bind (system user) (ask-for question)
                             (ceiling (+ (length system) (length user)) 4)))))
    (format nil "model-drift-watch estimate for ~a: ~d questions.~%  input about ~:d tokens; answers at most ~:d tokens each, ~:d in all, so at most ~:d tokens for one check.~%  ~:[A second run, on a day that looks worse, spends the same again.~;confirm is off, so no second run is spent.~]~%  Nothing is charged by this cell itself; your provider bills the tokens."
            (target-name) (length questions) input ceiling (* ceiling (length questions))
            (+ input (* ceiling (length questions)))
            (not (setting :confirm)))))

(defun questions ()
  "The fixed question set, by category, and how each answer is graded. Free."
  (with-output-to-string (out)
    (format out "model-drift-watch asks ~d fixed questions, graded by rule:~%" (length *suite*))
    (dolist (category '("reasoning" "instruction" "japanese"))
      (let ((items (remove-if-not (lambda (question)
                                     (string-equal category (getf question :category)))
                                   *suite*)))
        (when items
          (format out "  ~a (~d): ~{~a~^, ~}~%" category (length items)
                  (mapcar (lambda (question) (getf question :id)) items)))))
    (format out "  An answer is cut at its last ANSWER: line -- or read whole, for the instructions that say what to output -- then graded by an exact rule: a number, an accepted spelling, a shape, a word count, or a JSON array.~%  The category list is a setting: categories selects, empty asks every one.")))

(defun reset ()
  "Forget every recorded run: the next check starts a new baseline."
  (let ((path (state-path))
        (count (length (runs))))
    (when (probe-file path) (delete-file path))
    (format nil "model-drift-watch: deleted ~d recorded run(s); the next check starts a new baseline~@[ for ~a~]."
            count (target-name))))

(defun run-command (args session-id)
  "/model-drift-watch: no argument is status, and each other word is one of the verbs."
  (declare (ignore session-id))
  (let ((word (string-downcase (nv args))))
    (cond ((or (string= word "") (member word '("status" "show") :test #'string=))
           (status))
          ((member word '("run" "check" "now") :test #'string=) (check))
          ((string= word "estimate") (estimate))
          ((member word '("questions" "suite") :test #'string=) (questions))
          ((string= word "reset") (reset))
          (t (format nil "Unknown option: ~a~%~%~a" word +usage+)))))

;;; --- the declaration ---------------------------------------------------------------

(nlk:define-contract drift-call
  (:doc "Whether the model in use has quietly got worse: a fixed set of exactly graded
questions, compared with the same model's own baseline by an exact test.")
  ("action" :text :required t
            :options '("status" "run" "estimate" "reset")
            :doc "status (free) reads the last check and the baseline; estimate (free) says what a check would spend; run asks the questions and decides (spends tokens); reset starts a new baseline"))

(defun drift-tool (call)
  "The model_drift tool: the report's text, as written."
  (handler-case
      (let ((action (drift-call-action call)))
        (cond ((string= action "run") (check))
              ((string= action "estimate") (estimate))
              ((string= action "reset") (reset))
              (t (status))))
    (error (condition)
      (nle:failure (format nil "ERROR: model-drift-watch: ~a" condition)))))

(nle:define-cell model-drift-watch
  (:section ("model-drift-watch")
    (:guide "nothing is required: run /model-drift-watch run until the baseline holds baseline_runs clean runs, then run it on a schedule; provider and model name the watched target, else the auxiliary one does")
    ("provider" :string :default ""
     :doc "the provider of the watched model; empty watches the auxiliary target")
    ("model" :string :default ""
     :doc "the model to watch; empty watches the auxiliary target")
    ("state_dir" :string :default ""
     :doc "where runs.jsonl lives; empty keeps it under model-drift-watch/ in the home")
    ("categories" :list
     :doc "categories to ask (reasoning, instruction, japanese); empty asks every one")
    ("max_tokens" :integer :default 400 :min 64
     :doc "one answer's ceiling in tokens; raise it for a model that thinks at length")
    ("baseline_runs" :integer :default 5 :min 2
     :doc "clean runs that make the baseline before any comparison starts")
    ("min_drop_points" :integer :default 10 :min 1
     :doc "the smallest accuracy drop worth reporting, in points")
    ("alpha" :choice :options '("0.05" "0.01" "0.10") :default "0.05"
     :doc "the significance the exact test must pass before a drop is drift")
    ("confirm" :boolean :default t
     :doc "ask the questions a second time before calling a drop drift")
    ("keep_runs" :integer :default 40 :min 4
     :doc "how many recorded runs the file keeps, newest first"))
  (:tool "model_drift" (nlk:contract 'drift-call) 'drift-tool)
  (:command "model-drift-watch" 'run-command
            :catches 'model-drift-watch-error
            :description "Is the model you use still as good as it was?"
            :argument-hint "[status|run|estimate|questions|reset]")
  (:help :model-drift-watch
         "Model Drift Watch: a fixed exactly graded question set compared with the same model's own baseline - /model-drift-watch run"
         +manual+))

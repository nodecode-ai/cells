;;;; cell.lisp --- the five verdicts, the report artifact, the paired test.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A pass rate is not a decision. 47 of 50 reads as 94%, but the 95%
;;;; interval on that is [83.8%, 97.9%], so a 90% bar has not been cleared
;;;; yet -- and at 50 cases nothing says it will be. The three verbs here
;;;; answer with the verdict, the interval behind it, the confidence
;;;; threshold that routes uncertain cases to a person, and, when the sample
;;;; cannot support the claim, that it cannot.
;;;;
;;;; The ordering is deliberate: is the whole task good enough, then is some
;;;; confident subset good enough, and only then is the sample too small to
;;;; say. INSUFFICIENT EVIDENCE is the verdict a dashboard never gives.

(in-package #:nodecode-gonogo)

;;; --- the words -------------------------------------------------------------------

(defparameter +automate+ "AUTOMATE")
(defparameter +automate-with-review+ "AUTOMATE WITH REVIEW")
(defparameter +assist-only+ "ASSIST ONLY")
(defparameter +do-not-automate+ "DO NOT AUTOMATE")
(defparameter +insufficient+ "INSUFFICIENT EVIDENCE")

(defparameter +ece-unusable+ 0.15d0
  "Above this, miscalibrated confidence makes a threshold an opaque cut point.")
(defparameter +min-useful-coverage+ 0.25d0
  "An operating point covering less than this isn't worth the plumbing.")
(defparameter +assist-floor+ 0.5d0
  "Below this pass rate, tuning won't save it.")

(defparameter +target-note+
  "Give either :results-path, or both :passed and :total."
  "What DECIDE refuses with when it is given neither.")

(defparameter +manual+
  "Go/No-Go turns an eval score into a ship / don't-ship decision, through the
nodecode-gonogo cell: Lisp functions in the gonogo: package, called through eval.
Each answers a plist; a refusal is ERROR: GONOGO-ERROR with the reason. The
statistics run locally: no network, no key.
  (gonogo:decide :passed 47 :total 50 &key target level unit task)
      one run => :VERDICT one of AUTOMATE, AUTOMATE WITH REVIEW, ASSIST ONLY,
      DO NOT AUTOMATE, INSUFFICIENT EVIDENCE, :REASON, :PASS-RATE
      (:POINT :LOW :HIGH :LEVEL :N :TEXT), :OPERATING-POINT (a confidence
      threshold and the precision the cases above it reach, when per-case
      confidence exists), :CALIBRATION-ERROR, :NEEDED-N (how many cases the
      claim would need), :NOTES, :CAN-AUTOMATE, :MARKDOWN.
      :TARGET is the accuracy the deployment needs (default the section's
      default_target_percent); the verdict is measured against it on the
      interval's LOWER bound. :LEVEL is the interval's confidence.
      (gonogo:decide :results-path \"results.jsonl\") reads a results file
      instead of a count, which is what unlocks the operating point.
  (gonogo:report :results-path \"results.jsonl\" &key task target level
                 show-failures save-report write-html include-report-json)
      the artifact for a doc, a PR or a pilot review: the verdict, every rate
      with its interval, the risk-coverage table, the calibration table and the
      worst failures. SAVE-REPORT writes the report JSON, which gonogo:compare
      pairs against a later run; WRITE-HTML writes a standalone page.
  (gonogo:compare :report-a \"a.jsonl\" :report-b \"b.jsonl\" &key level)
      is B actually better than A? McNemar's test on the cases they share,
      with the difference in pass rate, its interval and a p-value. 'borderline'
      means the test and the interval disagree: the difference is not
      established, so do not call B better.
A results file is JSONL, one object per case, only 'passed' required:
  {\"id\": \"inv-001\", \"passed\": true, \"confidence\": 0.94, \"expected\": \"Acme\", \"output\": \"Acme\"}
  {\"id\": \"inv-007\", \"passed\": false, \"error\": \"TimeoutError: 30s\"}
  {\"id\": \"msg-014\", \"passed\": true, \"expected_action\": \"no_reply\", \"action\": \"no_reply\"}
confidence (0-1) is what unlocks a review threshold; error is a failed case;
a 'group' on every case folds correlated checks into one trial per group.
Call gonogo:decide whenever a pass rate is about to be read as an answer, and
gonogo:compare before agreeing that a change improved anything."
  "What (help :gonogo) answers while the cell runs.")

;;; --- the decision ----------------------------------------------------------------

(defstruct (decision (:constructor %decision))
  (verdict +insufficient+) (reason "") (pass-rate nil) (target 0.95d0)
  (operating-point nil) (calibration-error nil) (needed-n nil) (notes '())
  (can-automate nil))

(defun pct (fraction)
  "FRACTION as a whole percent, the way gonogo words a target."
  (format nil "~d%" (round (* 100d0 fraction))))

(defun signed-percent (fraction)
  "FRACTION as a percent with its sign, the way a difference reads."
  (format nil "~a~,1f%" (if (minusp fraction) "-" "+") (* 100d0 (abs fraction))))

(defun default-target ()
  "The target a call that names none is measured against."
  (coerce (/ (setting :default-target-percent) 100) 'double-float))

(defun probability (name value default)
  "VALUE as a probability in (0, 1), DEFAULT when absent, a refusal when it
is not one."
  (let ((number (cond ((null value) default)
                      ((numberp value) (coerce value 'double-float))
                      (t (fail "~a must be a number in (0, 1), got ~s" name value)))))
    (unless (< 0d0 number 1d0)
      (fail "~a must be a number in (0, 1), got ~s" name (or value number)))
    number))

(defun decide-trials (trials target level unit)
  "TRIALS, each (CONFIDENCE . PASSED), as a DECISION."
  (let ((notes '()))
    (flet ((note (control &rest args) (push (apply #'format nil control args) notes)))
      (let ((n (length trials)))
        (if (zerop n)
            (%decision :verdict +insufficient+ :reason "no cases were evaluated"
                       :pass-rate (wilson 0 0 level) :target target)
            (let* ((passed (count-if #'cdr trials))
                   (rate (wilson passed n level))
                   (has-confidence (> (length (remove-duplicates (mapcar #'car trials))) 1))
                   (ece (and has-confidence (expected-calibration-error trials)))
                   (review (and has-confidence
                                (best-operating-point trials target +min-useful-coverage+ level)))
                   (near (and has-confidence (null review)
                              (best-operating-point trials target 0d0 level)))
                   (answer (lambda (verdict reason &key operating-point needed-n)
                             (%decision :verdict verdict :reason reason :pass-rate rate
                                        :target target :operating-point operating-point
                                        :calibration-error ece :needed-n needed-n
                                        :notes (reverse notes)
                                        :can-automate (string= verdict +automate+)))))
              (unless has-confidence
                (note "Every case reported the same confidence, so no abstention threshold ~
can be derived. Emit a real per-case confidence to unlock selective automation."))
              (when (and has-confidence (> ece +ece-unusable+))
                (note "Calibration error ~a exceeds ~a: the confidence score ranks cases ~
usefully but its scale is not a probability. Treat any threshold as an opaque cut ~
point, and recalibrate before reading it as a percentage."
                      (fmt-cut ece) (fmt-cut +ece-unusable+)))
              (when review
                (note "The ~a threshold was chosen by searching this same case set, so its ~
precision is optimistically biased. Re-measure it on fresh cases before relying on it."
                      (fmt-cut (operating-point-threshold review))))
              (when near
                (note "A threshold of ~a would hit the target but only covers ~a of ~a, below ~
the ~a floor."
                      (fmt-cut (operating-point-threshold near))
                      (fmt-share (operating-point-coverage near)) unit
                      (fmt-share +min-useful-coverage+)))
              (cond
                ((>= (interval-low rate) target)
                 (funcall answer +automate+
                          (format nil "pass rate ~a clears the ~a target across all ~d ~a ~
at the ~a level"
                                  (interval-text rate) (pct target) n unit (pct level))))
                (review
                 (funcall answer +automate-with-review+
                          (format nil "overall pass rate ~a misses the ~a target; a candidate ~
threshold at confidence ~a reaches ~,1f% precision on ~a of ~a on this selection set, ~
but needs a fresh holdout before deployment"
                                  (interval-text rate) (pct target)
                                  (fmt-cut (operating-point-threshold review))
                                  (* 100d0 (interval-point (operating-point-precision review)))
                                  (fmt-share (operating-point-coverage review)) unit)
                          :operating-point review))
                ((>= (interval-point rate) target)
                 (let ((needed (required-n (interval-point rate) target level)))
                   (when needed
                     (note "At this rate, about ~d ~a would be needed to claim ~a; you have ~d."
                           needed unit (pct target) n))
                   (funcall answer +insufficient+
                            (format nil "observed ~,1f% on ~d ~a is above the ~a target, but ~
the ~a interval reaches down to ~,1f%"
                                    (* 100d0 (interval-point rate)) n unit (pct target)
                                    (pct level) (* 100d0 (interval-low rate)))
                            :needed-n needed)))
                ((>= (interval-point rate) +assist-floor+)
                 (funcall answer +assist-only+
                          (format nil "pass rate ~a is below the ~a target and no confident ~
subset reaches it; useful as a draft-generator, not as an unattended step"
                                  (interval-text rate) (pct target))))
                (t (funcall answer +do-not-automate+
                             (format nil "pass rate ~a is below ~a; this workflow is not a ~
fit for automation as currently scoped"
                                     (interval-text rate) (pct (coerce +assist-floor+ 'double-float))))))))))))

;;; --- the answer a verb gives -------------------------------------------------------

(defun interval-plist (interval)
  "INTERVAL as a plist the model reads."
  (list :point (interval-point interval) :low (interval-low interval)
        :high (interval-high interval) :level (interval-level interval)
        :n (interval-n interval) :text (interval-text interval)))

(defun operating-point-plist (point)
  "POINT as a plist, or NIL."
  (when point
    (let ((precision (operating-point-precision point)))
      (list :threshold (operating-point-threshold point)
            :coverage (operating-point-coverage point)
            :n-covered (operating-point-n-covered point)
            :n-deferred (operating-point-n-deferred point)
            :precision (interval-point precision)
            :precision-low (interval-low precision)
            :precision-high (interval-high precision)
            :precision-text (interval-text precision)))))

(defun decision-output (decision prepared task)
  "DECISION over PREPARED, for TASK, as the plist a verb answers."
  (list :task task
        :verdict (decision-verdict decision)
        :can-automate (decision-can-automate decision)
        :reason (decision-reason decision)
        :target (decision-target decision)
        :unit (getf prepared :unit)
        :n-trials (interval-n (decision-pass-rate decision))
        :n-cases (getf prepared :n-cases)
        :n-passed (getf prepared :n-passed)
        :n-trials-passed (count-if #'cdr (getf prepared :trials))
        :pass-rate (interval-plist (decision-pass-rate decision))
        :operating-point (operating-point-plist (decision-operating-point decision))
        :calibration-error (decision-calibration-error decision)
        :needed-n (decision-needed-n decision)
        :notes (decision-notes decision)))

(defun decision-markdown (out)
  "OUT as the block an agent pastes into an answer."
  (with-output-to-string (stream)
    (format stream "**~a** — ~a~%~%" (getf out :verdict) (getf out :reason))
    (let ((n-cases (getf out :n-cases))
          (n-trials (getf out :n-trials)))
      (format stream "Pass rate ~a over ~d ~a (~d passed)~@[ (~d/~d cases passed)~], target ~a~%"
              (getf (getf out :pass-rate) :text) n-trials (getf out :unit)
              (getf out :n-trials-passed)
              (unless (or (null n-cases) (= n-cases n-trials)) (getf out :n-passed))
              n-cases (pct (getf out :target))))
    (let ((point (getf out :operating-point)))
      (when point
        (format stream "Candidate operating point (fresh holdout required): abstain below ~a → ~a ~
~a selected at ~,1f% precision ~a, ~d ~a to review~%"
                (fmt-cut (getf point :threshold)) (fmt-share (getf point :coverage))
                (getf out :unit) (* 100d0 (getf point :precision))
                (getf point :precision-text) (getf point :n-deferred) (getf out :unit))))
    (when (getf out :calibration-error)
      (format stream "Calibration error ~a~%" (fmt-cut (getf out :calibration-error))))
    (when (getf out :needed-n)
      (format stream "Needs about ~d ~a to support the claim at this target~%"
              (getf out :needed-n) (getf out :unit)))
    (let ((actions (getf out :actions)))
      (when actions
        (format stream "Actions: acted when it should have held back ~d, held back when it ~
should have acted ~d~%"
                (getf actions :acted-when-should-hold-back)
                (getf actions :held-back-when-should-act))))
    (dolist (note (getf out :notes))
      (format stream "Note: ~a~%" note))))

;;; --- what a verb was given ----------------------------------------------------------

(defun count-arg (name value)
  "VALUE as an integer, or NIL when absent, or a refusal."
  (cond ((null value) nil)
        ((integerp value) value)
        ((and (realp value) (= value (round value))) (round value))
        ((and (stringp value) (ignore-errors (parse-integer value :junk-allowed nil))))
        (t (fail "~a must be an integer, got ~s" name value))))

(defun trimmed-name (value)
  "VALUE as a trimmed non-empty string, or NIL; a pathname reads as its name."
  (let ((text (typecase value
                (string value)
                (pathname (namestring value))
                (t nil))))
    (and text (plusp (length (string-trim '(#\Space #\Tab) text)))
         (string-trim '(#\Space #\Tab) text))))

(defun results-file-task (path payload given)
  "The task name: GIVEN, else the saved report's own, else the file's name."
  (or (trimmed-name given)
      (let ((task (and payload (json-member payload "task"))))
        (and (stringp task) (plusp (length task)) task))
      (pathname-name (nlk:expand-home (path-string path)))))

(defun prepare (passed total results-path unit task)
  "What DECIDE measures on: a results file when RESULTS-PATH is given, else a
count => a plist of :TRIALS :UNIT :N-CASES :N-PASSED :TASK :RECORDS."
  (if (trimmed-name results-path)
      (multiple-value-bind (rows payload) (read-rows results-path)
        (multiple-value-bind (trials unit-label n-groups records) (rows->trials rows)
          (declare (ignore n-groups))
          (list :trials trials :unit unit-label
                :n-cases (length records)
                :n-passed (count-if (lambda (record) (getf record :passed)) records)
                :task (results-file-task results-path payload task)
                :records records)))
      (let ((passed (count-arg "passed" passed))
            (total (count-arg "total" total)))
        (unless (and passed total)
          (fail "~a" +target-note+))
        (when (or (<= total 0) (minusp passed) (> passed total))
          (fail "need total > 0 and 0 <= passed <= total, got passed=~s, total=~s" passed total))
        (let ((unit-label (string-downcase (or (trimmed-name unit) "cases")))
              (task-name (or (trimmed-name task) "task")))
          (list :trials (append (loop repeat passed collect (cons 1d0 t))
                                (loop repeat (- total passed) collect (cons 1d0 nil)))
                :unit unit-label
                :n-cases (if (string= unit-label "cases") total nil)
                :n-passed (if (string= unit-label "cases") passed nil)
                :task task-name
                :records nil)))))

;;; --- the three verbs ----------------------------------------------------------------

(define-verb decide (&key passed total results-path target level unit task)
  "Decide whether a measured accuracy is good enough to ship. One of
AUTOMATE, AUTOMATE WITH REVIEW, ASSIST ONLY, DO NOT AUTOMATE, INSUFFICIENT
EVIDENCE, with the pass rate's interval, the confidence threshold that routes
uncertain cases to a person, and the sample size the claim would need."
  (let* ((target (probability "target" target (default-target)))
         (level (probability "level" level 0.95d0))
         (prepared (prepare passed total results-path unit task))
         (decision (decide-trials (getf prepared :trials) target level (getf prepared :unit)))
         (out (decision-output decision prepared (getf prepared :task))))
    (when (getf prepared :records)
      (let ((actions (actions-table (getf prepared :records))))
        (when actions
          (setf out (list* :actions actions out)))))
    (setf (getf out :markdown) (decision-markdown out))
    out))

(defun failures (records count)
  "The first COUNT failed records, as (ID . DETAIL) pairs."
  (let ((pairs '()))
    (dolist (record records)
      (when (and (null (getf record :passed)) (< (length pairs) count))
        (push (cons (getf record :id)
                    (or (getf record :error)
                        (and (getf record :deferral) "agent abstained")
                        ""))
              pairs)))
    (nreverse pairs)))

(defun report-markdown (out prepared failures)
  "The report artifact for OUT over PREPARED, with FAILURES listed."
  (declare (ignore prepared))
  (with-output-to-string (stream)
    (format stream "# Score report — ~a~%~%" (getf out :task))
    (format stream "~a~%" (decision-markdown out))
    (format stream "~%~%## Risk and coverage~%~%")
    (let ((point (getf out :operating-point)))
      (format stream "~:[no per-case confidence was supplied, so no threshold can be chosen~;~
abstain below ~a: ~a of ~a selected at ~,1f% precision ~a, ~d to review~]~%"
              point
              (and point (fmt-cut (getf point :threshold)))
              (and point (fmt-share (getf point :coverage))) (getf out :unit)
              (and point (* 100d0 (getf point :precision)))
              (and point (getf point :precision-text))
              (and point (getf point :n-deferred))))
    (format stream "~%~%## Worst failures~%~%")
    (if failures
        (dolist (failure failures)
          (format stream "- ~a~@[: ~a~]~%" (car failure)
                  (let ((detail (cdr failure)))
                    (and detail (plusp (length detail)) detail))))
        (format stream "none~%"))))

(defun report-json (out prepared task explicit-ids)
  "OUT over PREPARED as the report JSON a later COMPARE pairs against."
  (declare (ignore explicit-ids))
  (with-output-to-string (stream)
    (format stream "{\"source\":\"nodecode-gonogo\",\"task\":~a,\"verdict\":~a,\"target\":~,4f,~
\"level\":~,4f,\"unit\":~a,\"n_cases\":~d~@[,\"n_groups\":~d~],\"cases\":["
            (json-quote task) (json-quote (getf out :verdict))
            (getf out :target) (getf (getf out :pass-rate) :level)
            (json-quote (getf out :unit)) (getf out :n-cases)
            (and (equal (getf out :unit) "groups") (getf out :n-trials)))
    (loop for record in (getf prepared :records)
          for first = t then nil
          do (unless first (write-string "," stream))
             (format stream "{\"id\":~a,\"passed\":~a"
                     (json-quote (getf record :id))
                     (if (getf record :passed) "true" "false"))
             (when (getf record :group)
               (format stream ",\"group\":~a" (json-quote (getf record :group))))
             (when (getf record :confidence)
               (format stream ",\"confidence\":~,6f" (getf record :confidence)))
             (write-string "}" stream))
    (write-string "]}" stream)))

(defun report-html (out prepared failures)
  "OUT over PREPARED as a standalone page."
  (with-output-to-string (stream)
    (format stream "<!doctype html>~%<html><head><meta charset=\"utf-8\"><title>Score report — ~a~
</title></head>~%<body><h1>Score report — ~a</h1>~%<pre>~a</pre>~%"
            (html-escape (getf out :task)) (html-escape (getf out :task))
            (html-escape (report-markdown out prepared failures)))
    (write-string "</body></html>" stream)))

(define-verb report (&key results-path task target level show-failures
                        save-report write-html include-report-json)
  "The full score report for a results file: the verdict, every rate with its
interval, the risk-coverage and calibration tables and the worst failures;
optionally written as report JSON or a standalone page."
  (unless (trimmed-name results-path)
    (fail "results_path is required: the report is the artifact for a results file"))
  (let* ((target (probability "target" target (default-target)))
         (level (probability "level" level 0.95d0))
         (shown (max 0 (or (count-arg "show-failures" show-failures) 5)))
         (prepared (prepare nil nil results-path nil task))
         (decision (decide-trials (getf prepared :trials) target level (getf prepared :unit)))
         (out (decision-output decision prepared (getf prepared :task)))
         (records (getf prepared :records))
         (worst (failures records shown)))
    (let ((actions (actions-table records)))
      (when actions
        (setf out (list* :actions actions out))))
    (setf (getf out :summary)
          (format nil "~a: ~a — pass rate ~a over ~d ~a, target ~a~@[; needs about ~d ~a~]"
                  (getf prepared :task) (getf out :verdict)
                  (getf (getf out :pass-rate) :text) (getf out :n-trials)
                  (getf out :unit) (pct target)
                  (getf out :needed-n) (getf out :needed-n)))
    (setf (getf out :markdown) (report-markdown out prepared worst))
    (when (trimmed-name save-report)
      (let ((path (write-text save-report
                               (report-json out prepared (getf prepared :task) t))))
        (setf (getf out :saved-report) (namestring path))))
    (when (trimmed-name write-html)
      (let ((path (write-text write-html (report-html out prepared worst))))
        (setf (getf out :html-path) (namestring path))))
    (when include-report-json
      (setf (getf out :report) (report-json out prepared (getf prepared :task) t)))
    out))

;;; --- writing text, JSON and a page ------------------------------------------------

(defun write-text (path text)
  "TEXT written at PATH, its directories made => the pathname."
  (let ((truename (nlk:expand-home path)))
    (ensure-directories-exist truename)
    (with-open-file (stream truename :direction :output :if-exists :supersede
                                       :if-does-not-exist :create :external-format :utf-8)
      (write-string text stream))
    truename))

(defun json-quote (string)
  "STRING as a JSON string literal."
  (with-output-to-string (stream)
    (write-char #\" stream)
    (loop for character across (princ-to-string string)
          do (case character
               (#\" (write-string "\\\"" stream))
               (#\\ (write-string "\\\\" stream))
               (#\Newline (write-string "\\n" stream))
               (#\Return (write-string "\\r" stream))
               (#\Tab (write-string "\\t" stream))
               (t (if (< (char-code character) 32)
                      (format stream "\\u~4,'0x" (char-code character))
                      (write-char character stream)))))
    (write-char #\" stream)))

(defun html-escape (text)
  "TEXT with the five characters a page reads specially written as entities."
  (with-output-to-string (stream)
    (loop for character across text
          do (case character
               (#\& (write-string "&amp;" stream))
               (#\< (write-string "&lt;" stream))
               (#\> (write-string "&gt;" stream))
               (#\" (write-string "&quot;" stream))
               (t (write-char character stream))))))

;;; --- two runs on the same cases ---------------------------------------------------

(defun outcomes-of (path)
  "PATH as (values TABLE UNIT MEMBERSHIP): TABLE maps a trial's key to whether
it passed, MEMBERSHIP maps a group to its sorted case ids (NIL for cases)."
  (multiple-value-bind (rows payload) (read-rows path)
    (declare (ignore payload))
    (let* ((records (loop for row in rows
                          for i from 1
                          collect (row->record row i)))
           (grouped (remove-if-not (lambda (record) (getf record :group)) records)))
      (cond
        ((null grouped)
         (let ((table (make-hash-table :test #'equal)))
           (dolist (record records)
             (let ((id (getf record :id)))
               (when (nth-value 1 (gethash id table))
                 (fail "duplicate case id ~s; ids must be unique to pair on" id))
               (setf (gethash id table) (getf record :passed))))
           (values table "cases" nil)))
        ((= (length grouped) (length records))
         (let ((table (make-hash-table :test #'equal))
               (membership (make-hash-table :test #'equal)))
           (dolist (record records)
             (let* ((group (getf record :group))
                    (id (getf record :id))
                    (ids (gethash group membership)))
               (when (member id ids :test #'string=)
                 (fail "duplicate case id ~s within group ~s" id group))
               (setf (gethash group membership) (cons id ids))
               (setf (gethash group table)
                     (and (gethash group table t) (getf record :passed)))))
           (maphash (lambda (group ids)
                      (setf (gethash group membership) (sort ids #'string<)))
                    membership)
           (values table "groups" membership)))
        (t (fail "some cases carry a group and some do not; put a group on every case, ~
or on none"))))))

(defun shared-keys (table-a table-b)
  "The keys both tables carry, sorted."
  (let ((shared '()))
    (maphash (lambda (key value)
               (declare (ignore value))
               (when (nth-value 1 (gethash key table-b))
                 (push key shared)))
             table-a)
    (sort shared #'string<)))

(defun comparison (table-a membership-a table-b membership-b unit level)
  "McNemar on the shared trials of two outcome tables => the plist a verb answers."
  (let ((shared (shared-keys table-a table-b)))
    (when (null shared)
      (fail "the two reports share no ~a ids; nothing to compare"
            (if (equal unit "cases") "case" "group")))
    (when (and membership-a membership-b)
      (dolist (group shared)
        (unless (equal (gethash group membership-a) (gethash group membership-b))
          (fail "group ~s has different case membership across runs" group))))
    (let ((both-passed 0) (both-failed 0) (only-a 0) (only-b 0))
      (dolist (key shared)
        (let ((a (gethash key table-a))
              (b (gethash key table-b)))
          (cond ((and a b) (incf both-passed))
                ((and (not a) (not b)) (incf both-failed))
                (a (incf only-a))
                (t (incf only-b)))))
      (let* ((n (length shared))
             (rate-a (/ (float (+ both-passed only-a)) n))
             (rate-b (/ (float (+ both-passed only-b)) n))
             (difference (- rate-b rate-a)))
        (multiple-value-bind (p exact) (mcnemar-p only-a only-b)
          (multiple-value-bind (low high) (paired-difference-interval only-a only-b n level)
            (let* ((test-significant (< p (- 1d0 level)))
                   (excludes (or (> low 0d0) (< high 0d0)))
                   (significant (and test-significant excludes))
                   (borderline (not (eq test-significant excludes))))
              (list :n-shared n :unit unit
                    :both-passed both-passed :both-failed both-failed
                    :only-a only-a :only-b only-b
                    :discordant (+ only-a only-b)
                    :rate-a rate-a :rate-b rate-b
                    :difference difference :difference-low low :difference-high high
                    :level level :p-value p :exact exact
                    :test-significant test-significant
                    :interval-excludes-zero excludes
                    :significant significant :borderline borderline
                    :verdict (cond (significant "distinguishable")
                                   (borderline "borderline")
                                   (t "not distinguishable"))))))))))

(defun comparison-summary (result)
  "RESULT as the few lines a report carries."
  (with-output-to-string (stream)
    (format stream "~&Shared ~a~16t~d~%" (getf result :unit) (getf result :n-shared))
    (format stream "Agent A pass rate~16t~,1f%~%" (* 100d0 (getf result :rate-a)))
    (format stream "Agent B pass rate~16t~,1f%~%" (* 100d0 (getf result :rate-b)))
    (format stream "Difference~16t~a [~a, ~a] at ~a~%"
            (signed-percent (getf result :difference))
            (signed-percent (getf result :difference-low))
            (signed-percent (getf result :difference-high))
            (pct (getf result :level)))
    (format stream "Disagreements~16t~d (~d only-A, ~d only-B)~%"
            (getf result :discordant) (getf result :only-a) (getf result :only-b))
    (format stream "McNemar p~16t~,4f~@[ (chi-square approximation)~]~%"
            (getf result :p-value) (not (getf result :exact)))
    (format stream "Verdict~16t~a~%"
            (cond ((getf result :significant)
                   (format nil "agent ~a is better on these ~a"
                           (if (plusp (getf result :difference)) "B" "A")
                           (getf result :unit)))
                  ((getf result :borderline)
                   (format nil "borderline; McNemar p and the ~a interval disagree, so not ~
established; more shared ~a needed"
                           (pct (getf result :level)) (getf result :unit)))
                  (t "no detectable difference; the sample cannot separate them")))))

(define-verb compare (&key report-a report-b level)
  "Is B actually better than A on the cases they share? McNemar's test on
paired outcomes, with the difference in pass rate and its interval. 'No
detectable difference' is a useful answer at pilot sample sizes."
  (unless (and (trimmed-name report-a) (trimmed-name report-b))
    (fail "give both :report-a and :report-b: two results files or saved reports"))
  (let* ((level (probability "level" level 0.95d0))
         (a (multiple-value-list (outcomes-of report-a)))
         (b (multiple-value-list (outcomes-of report-b))))
    (unless (string= (second a) (second b))
      (fail "one run groups its cases and the other does not; compare like with like"))
    (let* ((result (comparison (first a) (third a) (first b) (third b)
                                (second a) level))
           (summary (comparison-summary result)))
      (setf (getf result :summary) summary)
      (setf (getf result :markdown)
            (format nil "**~a**~%~%```~%~a```~%~% ~,1f% vs ~,1f% (~a [~a, ~a], p=~,3f) ~
— ~a on ~d shared ~a"
                    (if (getf result :significant)
                        "Distinguishable"
                        (if (getf result :borderline)
                            "Borderline (the test and the interval disagree; not established)"
                            "Not distinguishable"))
                    summary
                    (* 100d0 (getf result :rate-b)) (* 100d0 (getf result :rate-a))
                    (signed-percent (getf result :difference))
                    (signed-percent (getf result :difference-low))
                    (signed-percent (getf result :difference-high))
                    (getf result :p-value) (getf result :verdict)
                    (getf result :n-shared) (getf result :unit)))
      result)))

;;; --- /gonogo ------------------------------------------------------------------------

(defun status (&optional arguments session-id)
  "What /gonogo answers: the settings in force and the verbs, no file read."
  (declare (ignore arguments session-id))
  (format nil "gonogo: target ~a at a ~a level by default. ~
(gonogo:decide :passed 47 :total 50) answers a verdict, ~
(gonogo:report :results-path \"results.jsonl\") the artifact, ~
(gonogo:compare :report-a \"a.jsonl\" :report-b \"b.jsonl\") the paired test. ~
(help :gonogo) has the shapes."
          (pct (default-target)) (pct 0.95d0)))

(nle:define-cell gonogo
  ;; Nothing here is required: a call may name its own target, and this is
  ;; the one a call that names none is measured against.
  (:section ("gonogo")
    (:guide "nothing here is required; default_target_percent is the accuracy a call that names no target is measured against")
    ("default_target_percent" :integer :default 95 :min 50
     :doc "the accuracy a deployment needs, in percent"))
  (:help :gonogo "gonogo:decide turns a pass count or a results file into a ship / don't-ship verdict; gonogo:report the artifact; gonogo:compare two runs" +manual+)
  (:command "gonogo" 'status
            :description "Go/No-Go: the default target, and the three verbs"
            :argument-hint ""))

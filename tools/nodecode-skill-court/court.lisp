;;;; court.lisp --- the evidence, the paired score and the verdicts.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of saatvik333/hermes-skill-court (see NOTICE). What the
;;;; original computes in scoring.py is here: each case is one task run
;;;; twice, once with the skill and once without it; success is the case's
;;;; own check; repeated pairs of one case are not independent evidence, so
;;;; they are averaged per case first and every interval is a bootstrap over
;;;; cases with a fixed seed, which makes a report reproducible. A verdict
;;;; needs at least MIN-TASKS cases judged in both arms and its whole 95%
;;;; interval on one side; with three cases that means every case moved
;;;; the same way. The token rules are the original's too: a skill that
;;;; costs 25% more without lifting success hurts, and one that lifts
;;;; success while costing at least 20% less helps.
;;;;
;;;; What the original keeps in SQLite, this keeps in one readable file
;;;; under the home, written atomically: the cases, the pairs and the
;;;; quarantine list, and nothing of a conversation.

(in-package #:nodecode-skill-court)

;;; --- the file ---------------------------------------------------------------

(defvar *state-file* nil
  "A file a test names; NIL means the home's own skill-court/state.sexp.")

(defvar *state* nil
  "The court's whole record: (:SKILLS ((NAME :CASES (...) :PAIRS (...)) ...)
:QUARANTINE ((NAME :NOTE ... :AT ...) ...)); NIL until it is read.")

(defvar *state-lock* (bt2:make-lock :name "skill-court")
  "One writer at a time: a tool, a command and a start can all touch the file.")

(defun state-path ()
  "Where the court keeps its record."
  (or *state-file* (nlk:home "skill-court/state.sexp")))

(defun empty-state ()
  "A court with nothing in it."
  (list :skills nil :quarantine nil))

(defun load-state (&optional path)
  "The record PATH (else the state path) holds; an empty court when it is
absent or unreadable, so a torn file never takes the cell down."
  (let ((file (or path (state-path))))
    (if (uiop:file-exists-p file)
        (handler-case
            (with-open-file (in file :direction :input)
              (let ((*read-eval* nil)
                    (object (read in nil nil)))
                (if (and (listp object) (listp (getf object :skills))) object (empty-state))))
          (error () (empty-state)))
        (empty-state))))

(defun save-state (&optional path)
  "Write the record where a restart reads it, atomically."
  (let ((file (or path (state-path))))
    (ignore-errors
     (nlk:write-file-atomically
      file (with-standard-io-syntax
             (let ((*print-pretty* t) (*print-length* nil) (*print-level* nil))
               (prin1-to-string *state*)))
      :mode #o600 :directory-mode #o700))
    (values)))

(defun ensure-state-loaded ()
  "The record, read once: a tool or a command that already holds it takes it
without a second lock, which a non-recursive one would not allow."
  (unless *state*
    (bt2:with-lock-held (*state-lock*)
      (unless *state* (setf *state* (load-state)))))
  *state*)

(defmacro with-state (&body body)
  "Run BODY holding the state lock, on the record the file has."
  `(bt2:with-lock-held (*state-lock*)
     (unless *state* (setf *state* (load-state)))
     ,@body))

;;; --- one skill's record ------------------------------------------------------

(defun now-text ()
  "Now, as UTC ISO-8601 to the second."
  (nlk:iso-time (nlk:unix-now) :millis nil))

(defun skill-names ()
  "Every skill the court holds evidence for, in the order they were added."
  (mapcar #'car (getf *state* :skills)))

(defun skill-record (name)
  "NAME's plist (:CASES ... :PAIRS ...), or NIL."
  (cdr (assoc name (getf *state* :skills) :test #'equal)))

(defun ensure-skill (name)
  "NAME's plist, adding an empty record when the court has none. The plist is
never NIL, so a caller's (SETF (GETF record ...)) lands in the record."
  (let ((entry (assoc name (getf *state* :skills) :test #'equal)))
    (unless entry
      (setf entry (cons name (list :cases nil :pairs nil)))
      (push entry (getf *state* :skills)))
    (cdr entry)))

(defun court-cases (name)
  "NAME's cases, oldest first: (:ID ... :PROMPT ... :CHECK ...)."
  (getf (skill-record name) :cases))

(defun court-pairs (name)
  "NAME's recorded pairs, oldest first."
  (getf (skill-record name) :pairs))

(defun court-score (name)
  "The paired score of the evidence NAME has; NIL when it has none."
  (let ((pairs (court-pairs name)))
    (when pairs
      (score (mapcar (lambda (row)
                       (make-pair (getf row :case)
                                  (getf row :with) (getf row :without)
                                  :with-tokens (or (getf row :with-tokens) 0)
                                  :without-tokens (or (getf row :without-tokens) 0)))
                     pairs)
             :min-tasks (setting :min-tasks)
             :margin (margin-value)
             :resamples (setting :resamples)
             :seed (setting :seed)))))

(defun quarantined (name)
  "The quarantine entry for NAME, or NIL."
  (assoc name (getf *state* :quarantine) :test #'equal))

(defun quarantine-skill (name note)
  "Put NAME on the quarantine list, replacing an earlier entry."
  (setf (getf *state* :quarantine)
        (cons (list name :note note :at (now-text))
              (remove name (getf *state* :quarantine) :key #'car :test #'equal)))
  (save-state))

(defun release-skill (name)
  "Take NAME off the quarantine list; T when it was on it."
  (let ((had (quarantined name)))
    (when had
      (setf (getf *state* :quarantine)
            (remove name (getf *state* :quarantine) :key #'car :test #'equal))
      (save-state))
    (and had t)))

;;; --- the paired score --------------------------------------------------------

(defstruct (pair (:constructor make-pair (task with without
                                          &key (with-tokens 0) (without-tokens 0))))
  "One case run in both arms: WITH and WITHOUT are :PASS or :FAIL, and the
token counts are what the two runs spent, 0 when nobody counted."
  (task "" :type string)
  (with :fail)
  (without :fail)
  (with-tokens 0 :type integer)
  (without-tokens 0 :type integer))

(defun outcome (value)
  "One arm's outcome as a number."
  (if (eq value :pass) 1.0 0.0))

(defun mean (values)
  "The arithmetic mean of VALUES, which is never empty here."
  (/ (reduce #'+ values :initial-value 0.0) (length values)))

(defun make-rng (seed)
  "A reproducible generator: xorshift32, so a report reads the same on any
machine and in any image."
  (let ((state (logand (if (zerop seed) 1 seed) #xffffffff)))
    (lambda (bound)
      (setf state (logand #xffffffff (logxor state (ash state 13))))
      (setf state (logand #xffffffff (logxor state (ash state -17))))
      (setf state (logand #xffffffff (logxor state (ash state 5))))
      (mod state bound))))

(defun bootstrap-ci (values resamples rng)
  "The 95% interval of the mean of VALUES, resampled RESAMPLES times.
Answers a cons of the two ends, or NIL for no values."
  (when values
    (let* ((n (length values))
           (vector (coerce values 'vector))
           (means (make-array resamples)))
      (dotimes (r resamples)
        (let ((sum 0.0))
          (dotimes (i n)
            (incf sum (aref vector (funcall rng n))))
          (setf (aref means r) (/ sum n))))
      (let ((sorted (sort means #'<)))
        (cons (aref sorted (floor (* 0.025 resamples)))
              (aref sorted (1- (floor (* 0.975 resamples)))))))))

(defun task-groups (pairs)
  "PAIRS grouped by case, the cases in a stable order."
  (let ((table (make-hash-table :test #'equal))
        (order '()))
    (dolist (pair pairs)
      (let ((task (pair-task pair)))
        (unless (gethash task table)
          (push task order))
        (push pair (gethash task table))))
    (mapcar (lambda (task) (cons task (nreverse (gethash task table))))
            (sort order #'string<))))

(defun task-means (groups slot)
  "One number per case: the mean of SLOT over the pairs of that case."
  (mapcar (lambda (group)
            (mean (mapcar slot (cdr group))))
          groups))

(defun token-ratios (groups)
  "One number per case that counted tokens in both arms: the mean log ratio
of with-skill tokens to without-skill tokens."
  (loop for (nil . pairs) in groups
        for ratios = (loop for pair in pairs
                           when (and (plusp (pair-with-tokens pair))
                                     (plusp (pair-without-tokens pair)))
                             collect (log (/ (pair-with-tokens pair)
                                             (pair-without-tokens pair))))
        when ratios collect (mean ratios)))

(defun percent (value)
  "VALUE as a whole percentage, without a sign."
  (round (* 100 value)))

(defun signed-percent (value)
  "VALUE as a signed whole percentage, as the original prints a token change."
  (format nil "~@d%" (round (* 100 value))))

(defun score (pairs &key (min-tasks 3) (margin 0.25) (resamples 2000) (seed 1))
  "The original's paired score over PAIRS: a plist of :TASKS, :PAIRS,
:SUCCESS-WITH, :SUCCESS-WITHOUT, :DELTA, :CI, :TOKEN-CHANGE, :TOKEN-CI,
:VERDICT (helps, hurts, neutral or inconclusive) and :REASON, one sentence
that says what the numbers did."
  (let* ((groups (task-groups pairs))
         (n (length groups))
         (deltas (task-means groups (lambda (pair)
                                       (- (outcome (pair-with pair))
                                          (outcome (pair-without pair))))))
         (with-rates (task-means groups (lambda (pair) (outcome (pair-with pair)))))
         (without-rates (task-means groups (lambda (pair) (outcome (pair-without pair)))))
         (ratios (token-ratios groups))
         (delta (and deltas (mean deltas)))
         (ci (and deltas (bootstrap-ci deltas resamples (make-rng seed))))
         (success-with (and with-rates (mean with-rates)))
         (success-without (and without-rates (mean without-rates)))
         (log-change (and ratios (mean ratios)))
         (enough-ratios (>= (length ratios) min-tasks))
         (token-interval (and enough-ratios (bootstrap-ci ratios resamples (make-rng seed))))
         (token-ci (and token-interval
                        (cons (1- (exp (car token-interval))) (1- (exp (cdr token-interval))))))
         (token-change (and log-change (1- (exp log-change)))))
    (labels ((result (verdict reason)
               (list :tasks n :pairs (length pairs)
                     :success-with success-with :success-without success-without
                     :delta delta :ci ci
                     :token-change (and enough-ratios token-change)
                     :token-ci token-ci
                     :verdict verdict :reason reason))
             (change ()
               (if (plusp (abs (or delta 0)))
                   (format nil "success went from ~d% without the skill to ~d% with it"
                           (percent success-without) (percent success-with))
                   "success was the same")))
      (cond
        ((< n min-tasks)
         (result "inconclusive"
                 (format nil "~d case~:p with outcomes in both arms; at least ~d are needed"
                         n min-tasks)))
        ((minusp (cdr ci))
         (result "hurts"
                 (format nil "success fell from ~d% without the skill to ~d% with it"
                         (percent success-without) (percent success-with))))
        ((plusp (car ci))
         (result "helps"
                 (format nil "success rose from ~d% without the skill to ~d% with it"
                         (percent success-without) (percent success-with))))
        ((and token-ci (<= delta 0) (> (car token-ci) margin))
         (result "hurts"
                 (format nil "~a, and runs used ~a tokens with the skill"
                         (change) (signed-percent token-change))))
        ((and token-ci (>= delta 0) (< (cdr token-ci) (- (/ 1 (+ 1 margin)) 1)))
         (result "helps"
                 (format nil "~a, and runs used ~a tokens with the skill"
                         (change) (signed-percent token-change))))
        (t (result "neutral" "no measurable difference in success or token use"))))))

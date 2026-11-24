;;;; cell.lisp --- the manual, the verbs, /adaptive, the :round hook, START-CELL.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Three verbs the model calls through eval, one slash command for the
;;;; operator, and one hook: with `mode' set to "once" or "always" the cell
;;;; judges each new turn's task itself at the :round point and lands the
;;;; effort it chose on the session, so the operator never asks for it.
;;;;
;;;; Config, a sibling top-level key next to `index' and `cron':
;;;;   "adaptive-effort": {"mode": "off", "scorer": "jev", "endpoint": "",
;;;;                       "model": "", "custom_endpoint": "", "custom_auth": "none",
;;;;                       "timeout_seconds": 3, "prompt_chars": 4000,
;;;;                       "instructions": "", "notify": true}
;;;; The key is TYPESAFE_API_KEY (default scorer) or CUSTOM_SCORER_API_KEY
;;;; (custom, bearer), read off the environment and never written anywhere.

(in-package #:nodecode-adaptive-effort)

(defparameter +manual+
  "Adaptive Effort judges a task with a scorer model and picks this session's reasoning effort from
the judgment. A Nodecode rewrite of the Hermes plugin hermes-adaptive-effort; the scorer is asked
one System One question (default TypeSafe's jev-latest, POST /v1/systemone) whose answer, 0 (low)
to 2 (high), becomes a rung of the session's own target -- the same ladder /think offers.
  (aeffort:choose TASK &key session-id dry-run)   judge TASK and land the rung on this session
      (dry-run judges it and lands nothing); => one line naming the score, the label and the rung
  (aeffort:probe TASK)   the judgment alone: no session, no pick, nothing stored
  (aeffort:history &key limit)   the decisions this process made, newest first
  (aeffort:status)   the mode, the scorer, where its key comes from, and the last decision
The section's mode, when it is `once' or `always', does the same by itself at each turn's first
round -- once per session, or on every new turn -- and the pick lands from the next turn (an
effort binds when a turn starts). `always' waits on the scorer for up to timeout_seconds before
the turn's first request; the default scorer's own deadline is what the plugin used.
ERROR: ADAPTIVE-EFFORT-ERROR names the problem: no key, no custom endpoint, an unreachable
scorer, or a session to land the pick on. A classification that fails leaves the effort as it was."
  "What (help :adaptive-effort) answers while the cell runs.")

;;; --- the decision ledger ------------------------------------------------------------------
;;; The plugin keeps a bounded SQLite history of effort changes; here the
;;; ledger is this process's own, bounded by max_turns, newest first. It is
;;; what /adaptive history and (aeffort:history) read.

(defparameter *decisions* '()
  "The decisions this process made, newest first: (:at :session :score
:label :rung :scorer :outcome :task).")

(defparameter *last-turn* (make-hash-table :test 'equal)
  "SESSION-ID -> the durable turn object last judged, so `always' judges a
turn once rather than once a round.")

(defparameter *decided* (make-hash-table :test 'equal)
  "SESSION-ID -> T once `once' mode has decided for it.")

(defun now-iso ()
  "The current UTC time as the ISO-8601 minute the ledger stamps."
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time (get-universal-time) 0)
    (declare (ignore second))
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0dZ" year month date hour minute)))

(defun record-decision (&key session score label rung scorer outcome task)
  "Keep one decision at the head of the ledger, and drop what falls past the
section's own bound."
  (push (list :at (now-iso) :session session :score score :label label :rung rung
              :scorer scorer :outcome outcome :task (nlk:clip task 120))
        *decisions*)
  (when (> (length *decisions*) (setting :max-turns))
    (setf *decisions* (subseq *decisions* 0 (setting :max-turns))))
  (first *decisions*))

(defun decision-line (decision)
  "One ledger entry as the line /adaptive history prints."
  (format nil "~a ~@[~a ~]~@[score ~,2f, ~]~a -> ~a~@[ (~a)~]: ~a"
          (getf decision :at)
          (getf decision :session)
          (getf decision :score)
          (or (getf decision :label) "?")
          (or (getf decision :rung) "no rung")
          (getf decision :outcome)
          (getf decision :task)))

;;; --- the three verbs ----------------------------------------------------------------------

(defun require-task (task)
  "TASK as the text one question is asked about, at the section's
prompt_chars bound; a refusal when there is nothing to judge."
  (let ((text (and (stringp task) (nlk:trimmed task))))
    (unless (and text (plusp (length text)))
      (fail "adaptive-effort: give it a task to judge, as a string"))
    (nlk:clip text (setting :prompt-chars) :ellipsis "...")))

(defun session-task (session-id)
  "The newest thing the operator typed in SESSION-ID, or NIL: what `once' and
`always' judge, read off the durable log as SESSION-TASK."
  (and session-id
       (nlk:store-open-p)
       (ignore-errors
         (let ((rows (nlk:recall-log :session-id session-id :kinds '("turn.input_committed")
                                     :limit 1 :clip nil)))
           (loop for row in (reverse rows)
                 when (and (consp row) (stringp (getf row :text)) (plusp (length (getf row :text))))
                   return (getf row :text))))))

(defun land (session-id rung)
  "Make RUNG SESSION-ID's reasoning effort through the image's own pick
(NLE:SELECT-EFFORT, what a /think pick is) and answer its one line; a
refusal names what the target did instead."
  (handler-case (nle:select-effort rung :session-id session-id)
    (error (condition)
      (fail "adaptive-effort: ~a" (condition-text condition)))))

(defun condition-text (condition)
  "CONDITION as one line: the image's refusals are their own explanation."
  (nlk:one-line (princ-to-string condition)))

(defun probe (task)
  "Judge TASK without touching any session: no pick lands and nothing is
stored. => one line naming the score, the label and the scorer."
  (multiple-value-bind (score label model) (classify (require-task task))
    (format nil "adaptive-effort: ~a~@[, score ~,2f~] -> ~a (scorer ~a)"
            (or label "no label") score label (label model))))

(defun choose (task &key session-id dry-run)
  "Judge TASK and land the effort it calls for on SESSION-ID (this eval's own
session by default); DRY-RUN judges and lands nothing. => one line naming the
score, the label and the rung, and whether it landed."
  (let* ((task (require-task task))
         (session (or session-id nlk:*scribe-session-id*))
         (start (get-internal-real-time)))
    (multiple-value-bind (score label model) (classify task)
      (let* ((ladder (target-ladder session))
             (rung (pick-rung label ladder))
             (outcome
               (cond (dry-run "not landed, a dry run")
                     ((null session) "not landed: no session")
                     ((or (null rung) (and ladder (not (member rung ladder :test #'string-equal))))
                      (format nil "not landed: ~a offers no reasoning effort"
                              (or session "the target")))
                     (t (land session rung)))))
        (record-decision :session session :score score :label label :rung rung
                         :scorer (label model) :outcome (first-line outcome) :task task)
        (format nil "adaptive-effort: ~@[score ~,2f, ~]~a -> ~a; ~a (scorer ~a, ~d ms)"
                score (or label "no label") (or rung "none") outcome
                (label model)
                (round (* 1000 (- (get-internal-real-time) start))
                       internal-time-units-per-second))))))

(defun first-line (text)
  "TEXT up to its first newline, at most 200 characters: one ledger line."
  (nlk:clip (nlk:one-line text) 200))

(defun history (&key (limit 10))
  "The decisions this process made, newest first, at most LIMIT lines."
  (check-type limit (integer 1))
  (let ((bound (setting :max-turns)))
    (declare (ignore bound)))
  (if *decisions*
      (format nil "~{~a~^~%~}"
              (loop for decision in *decisions* repeat limit collect (decision-line decision)))
      "adaptive-effort: no decision yet"))

(defun status ()
  "The mode, the scorer and where its key comes from, and the last
decision: one line, asking no one."
  (let ((last (first *decisions*)))
    (format nil "adaptive-effort: mode ~a; scorer ~a at ~a, key ~a; ~@[last: ~a; ~]~d decided"
            (setting :mode) (scorer)
            (handler-case (scorer-url) (error () "not configured"))
            (let ((key (env-value (if (string= (scorer) "custom") +custom-env+ +env+))))
              (if key "set" "missing"))
            (and last (decision-line last))
            (length *decisions*))))

;;; --- the hook: deciding for the operator -----------------------------------------------------

(defun due-p (session-id turn)
  "Whether this round of TURN in SESSION-ID is one the section's mode decides
on: never in `off', once per session in `once', once per turn in `always'."
  (case (intern (string-upcase (setting :mode)) :keyword)
    (:off nil)
    (:once (not (gethash session-id *decided*)))
    (:always (not (eq turn (gethash session-id *last-turn*))))
    (t nil)))

(defun decide-quietly (session-id)
  "Judge the session's newest task and land the rung, for the mode that does
it unasked; => the outcome line, or NIL when there is nothing to judge."
  (let ((task (session-task session-id)))
    (when task
      (multiple-value-bind (score label model) (classify (nlk:clip task (setting :prompt-chars)))
        (let* ((ladder (target-ladder session-id))
               (rung (pick-rung label ladder))
               (outcome (if (and rung (or (null ladder) (member rung ladder :test #'string-equal)))
                            (handler-case (land session-id rung)
                              (adaptive-effort-error (condition) (condition-text condition)))
                            (format nil "not landed: this target offers no reasoning effort"))))
          (setf (gethash session-id *decided*) t)
          (record-decision :session session-id :score score :label label :rung rung
                           :scorer (label model) :outcome (first-line outcome) :task task)
          (let ((line (format nil "adaptive-effort: ~a (score ~,2f) -> ~a; ~a"
                               label score (or rung "none") outcome)))
            (when (setting :notify)
              (nle:notice line :level :info :key "nodecode-adaptive-effort"))
            line))))))

(defun observe-round (op next)
  "The :round hook: at a turn's first round, in `once' or `always' mode,
judge the task and land the effort it calls for. Isolated by the point, so a
scorer that cannot be reached never fails the turn."
  (let ((session-id (getf op :session-id))
        (turn (getf op :turn)))
    (when (and session-id (due-p session-id turn))
      (setf (gethash session-id *last-turn*) turn)
      (ignore-errors (decide-quietly session-id))))
  (funcall next op))

;;; --- /adaptive -----------------------------------------------------------------------------

(defun run-slash (args session-id)
  "/adaptive alone or `status': what the cell is doing; `history': the last
decisions; `probe TASK': the judgment alone; `choose TASK': judge and land
it now."
  (let* ((text (or args ""))
         (space (position #\Space text))
         (verb (string-downcase (nlk:trimmed (if space (subseq text 0 space) text))))
         (rest (and space (nlk:trimmed (subseq text (1+ space))))))
    (handler-case
        (cond ((member verb '("" "status") :test #'string=) (status))
              ((string= verb "history") (history))
              ((string= verb "probe") (if rest (probe rest) (probe-usage)))
              ((string= verb "choose") (if rest (choose rest :session-id session-id) (choose-usage)))
              (t (format nil "/adaptive: ~a is not one of status, history, probe, choose" verb)))
      (adaptive-effort-error (condition) (condition-text condition)))))

(defun probe-usage ()
  "/adaptive probe with nothing to judge: one line saying what it wants."
  "usage: /adaptive probe TASK - judge a task without touching the session")

(defun choose-usage ()
  "/adaptive choose with nothing to judge: one line saying what it wants."
  "usage: /adaptive choose TASK - judge a task and land the effort it calls for")

(nle:define-cell adaptive-effort
  (:section ("adaptive-effort")
    (:guide "the default scorer is TypeSafe's jev: make a key at https://console.typesafe.ai/ and set TYPESAFE_API_KEY, or name a custom_endpoint answering POST /v1/systemone; mode off|once|always decides by itself; /adaptive choose TASK decides now")
    ("mode" :choice :options '("off" "once" "always") :default "off"
     :doc "off decides only when asked; once decides once per session; always decides at every new turn")
    ("scorer" :choice :options '("jev" "custom") :default "jev"
     :doc "jev is TypeSafe's System One; custom is any URL answering the same route")
    ("endpoint" :string :default ""
     :doc "the TypeSafe API root; empty is TYPESAFE_BASE_URL, else https://api.typesafe.ai")
    ("model" :string :default ""
     :doc "the scorer model; empty is TYPESAFE_DEFAULT_MODEL, else jev-latest")
    ("custom_endpoint" :string :default ""
     :doc "the full URL a custom scorer posts to; required when scorer is custom")
    ("custom_auth" :choice :options '("none" "bearer") :default "none"
     :doc "none for a local or public endpoint; bearer reads CUSTOM_SCORER_API_KEY")
    ("timeout_seconds" :integer :default 3 :min 1
     :doc "one classification's deadline; a timeout leaves the effort as it was")
    ("prompt_chars" :integer :default 4000 :min 100
     :doc "the most task text one classification sends")
    ("instructions" :string :default ""
     :doc "extra guidance for the scorer, up to 2000 characters")
    ("max_turns" :integer :default 32 :min 1
     :doc "how many decisions the ledger keeps")
    ("notify" :boolean :default t
     :doc "say each automatic pick as a notice"))
  (:help :adaptive-effort "Adaptive Effort: a scorer picks this session's reasoning effort - (aeffort:choose \"task\"), (aeffort:probe \"task\"), /adaptive status|history" +manual+)
  (:command "adaptive" 'run-slash
   :description "Adaptive Effort: status, history, or judge a task and land its effort")
  (:hook :round #'observe-round)
  (:start (lambda ()
            ;; a start is a fresh ledger: nothing a previous run decided is
            ;; what this one has done.
            (setf *decisions* '())
            (clrhash *last-turn*)
            (clrhash *decided*))))

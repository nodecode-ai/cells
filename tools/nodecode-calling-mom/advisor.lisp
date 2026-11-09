;;;; advisor.lisp --- the two tiers, their prompts, their budgets.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of the original's __init__.py (_PROMPTS, _build_messages,
;;;; _advise, the ESCALATE gate) and _core.redact_for_egress. One side call
;;;; is one question to the cheap advisor (mom); only when mom herself says
;;;; ESCALATE, and only for a high-stakes category, is the expensive one
;;;; (olymp) asked, and it has a hard call limit rather than a dollar one.
;;;;
;;;; Advice is a sentence or two and nothing else: the strong model looks
;;;; over the shoulder, the working model keeps working.

(in-package #:nodecode-calling-mom)

(defparameter +prompts+
  '(("stuck" . "You advise a weaker coding agent that may be stuck. If the steps show progress and no clear \
problem, answer exactly NO_ACTION. Otherwise answer in at most 100 words: the most likely root cause, then the \
next 1-3 concrete steps. Do not restate the task.")
    ("critical_change" . "You review a critical change made by a weaker coding agent. Answer in at most 100 \
words: the biggest risk or irreversible aspect and what to double-check before going on. If it is fine, say so \
in one line. Do not restate the task.")
    ("second_opinion" . "You answer a design question from a coding agent. Answer in at most 120 words: \
recommend one option, give the deciding reason and name one risk."))
  "Each signal's instruction, as the original wrote it.")

(defparameter +gate+
  "

You are the FIRST reviewer; a far more expensive reviewer exists. Give your normal answer. Only for a rare \
high-stakes case that you cannot judge as safe from this context (irreversible production or public action \
without a fallback) answer exactly 'ESCALATE: ' plus a one-line reason. Routine or reversible changes: never \
escalate."
  "Appended to the cheap tier's instruction when the expensive tier may be reached.")

;;; --- what may leave the machine ------------------------------------------------

(defparameter +url-credentials+
  (cl-ppcre:create-scanner "(\\b[a-z][a-z0-9+.-]*://[^/\\s:@]+:)[^@\\s/]+@" :case-insensitive-mode t)
  "A password inside a URL: the original's own addition, because a redactor
that knows token names does not look inside one.")

(defparameter +secret-shapes+
  (list (cons (cl-ppcre:create-scanner "(?i)\\b(sk|rk|pk)-[A-Za-z0-9_-]{16,}") "sk-***")
        (cons (cl-ppcre:create-scanner "(?i)\\bgh[pousr]_[A-Za-z0-9]{20,}") "gh*_***")
        (cons (cl-ppcre:create-scanner "(?i)\\bxox[abposr]-[A-Za-z0-9-]{10,}") "xox*-***")
        (cons (cl-ppcre:create-scanner "\\bAKIA[0-9A-Z]{16}\\b") "AKIA***")
        (cons (cl-ppcre:create-scanner "(?i)\\b(bearer|token|api[_-]?key|password|passwd|secret)\\s*[:=]\\s*\\S+")
              "\\1=***"))
  "Token shapes blanked before anything is sent: (SCANNER . REPLACEMENT).")

(defun redact (text)
  "TEXT with passwords in URLs and the usual token shapes blanked out."
  (let ((out (if (stringp text) text (format nil "~a" text))))
    (setf out (cl-ppcre:regex-replace-all +url-credentials+ out "\\1***@"))
    (loop for (scanner . replacement) in +secret-shapes+
          do (setf out (cl-ppcre:regex-replace-all scanner out replacement)))
    out))

(defun clip (text limit)
  "TEXT at most LIMIT characters, its tail cut with an ellipsis."
  (let ((text (if (stringp text) text (format nil "~a" text))))
    (if (and limit (> (length text) limit))
        (concatenate 'string (subseq text 0 (max 0 (- limit 4))) " ...")
        text)))

(defun bounded-context (text limit)
  "TEXT at LIMIT characters, keeping its head and its most recent tail: a
long form keeps what it starts and what it ends on."
  (let ((text (if (stringp text) text (format nil "~a" text))))
    (if (<= (length text) limit)
        text
        (let ((head (floor limit 3)))
          (concatenate 'string (subseq text 0 head) "\n...\n"
                       (subseq text (- (length text) (- limit head 5))))))))

;;; --- a target, and one call ------------------------------------------------------

(defun split-target (spec)
  "=> (values PROVIDER MODEL) for a section value: \"provider/model\", a bare
model, \"route:NAME\" (unread here), or \"\" for none."
  (let ((spec (if (stringp spec) (string-trim " " spec) "")))
    (cond ((zerop (length spec)) (values nil nil))
          ((position #\/ spec)
           (let ((slash (position #\/ spec)))
             (values (subseq spec 0 slash) (subseq spec (1+ slash)))))
          (t (values nil spec)))))

(defvar *call* nil
  "A function of (SYSTEM USER) answering the model's text, which a test binds;
NIL means the real side call through NLE:COMPLETE.")

(defvar *mom-override* ""
  "The model /calling-mom mom set for this run; the section's when empty.")

(defvar *olymp-override* ""
  "The model /calling-mom olymp set for this run; the section's when empty.")

(defun chosen (key override)
  "OVERRIDE when this run set one, else the section's value for KEY, else the empty string."
  (if (and (stringp override) (plusp (length override)))
      override
      (let ((value (ignore-errors (setting key))))
        (if (stringp value) value ""))))

(defun mom-spec () (chosen :mom *mom-override*))
(defun olymp-spec () (chosen :olymp *olymp-override*))

(defun ask-one (target system user)
  "One side call on TARGET, a (PROVIDER . MODEL) pair or NIL for the
auxiliary target. => the text, or NIL when the call failed."
  (if *call*
      (funcall *call* system user)
      (handler-case
          (let ((max-tokens (or (ignore-errors (setting :max-tokens)) 400)))
            (if (or (car target) (cdr target))
                (multiple-value-bind (provider model) (nle:auxiliary-model)
                  (let ((nle::*auxiliary-model*
                          (list :provider (or (car target) provider)
                                :model (or (cdr target) model))))
                    (nle:complete system user :max-tokens max-tokens :fallback-p nil)))
                (nle:complete system user :max-tokens max-tokens :fallback-p nil)))
        (error (condition)
          (let ((text (format nil "~a" condition)))
            (setf *last-error* (clip (redact text) 200))
            nil)))))

(defvar *last-error* nil
  "Why the last call failed, for the one line that says so.")

;;; --- the day's budget ---------------------------------------------------------

(defvar *day* nil "The day the counters below belong to: the universal-time day.")
(defvar *calls-today* 0 "How many times the cheap tier was asked today.")
(defvar *olymp-today* 0 "How many times the expensive tier was asked today.")
(defvar *last-asked-at* 0 "When the last unsolicited question was asked.")
(defvar *asked-already* (make-hash-table :test #'equal)
  "Critical targets already reviewed since the start, so one command is not reviewed twice.")
(defvar *history* '() "The last few calls, newest first, for a stuck question.")
(defvar *tracker* nil "The signal tracker of this start.")
(defvar *assist* nil "Whether tool calls are watched: the section's value, /calling-mom on or off.")

(defun today () (floor (get-universal-time) 86400))

(defun roll-day ()
  "Start the counters over when the date changed."
  (unless (eql (today) *day*)
    (setf *day* (today) *calls-today* 0 *olymp-today* 0))
  t)

(defun budget-left-p ()
  "Whether the cheap tier may still be asked today."
  (roll-day)
  (< *calls-today* (or (ignore-errors (setting :mom-calls-per-day)) 20)))

(defun olymp-left-p ()
  "Whether the expensive tier may still be asked today."
  (roll-day)
  (< *olymp-today* (or (ignore-errors (setting :olymp-calls-per-day)) 6)))

;;; --- the question ------------------------------------------------------------

(defun build-user (signal steps note)
  "The one message a question carries: the recent steps, the signal, and the
note the caller wrote - all redacted, and bounded."
  (let* ((limit (or (ignore-errors (setting :context-chars)) 4000))
         (lines (loop for (tool args result) in (reverse steps)
                      collect (format nil "- ~a(~a) -> ~a" tool (clip args 120) (clip result 200))))
         (text (format nil "Signal: ~a~%~@[Last steps:~%~{~a~^~%~}~%~]~@[~%Details:~%~a~]"
                       signal lines
                       (and (stringp note) (plusp (length note)) note))))
    (redact (bounded-context text limit))))

(defun starts-with-p (prefix text)
  "Whether TEXT begins with PREFIX, case folded."
  (let ((prefix (string-upcase prefix))
        (text (string-upcase (if (stringp text) text (format nil "~a" text)))))
    (and (>= (length text) (length prefix))
         (string= prefix text :end2 (length prefix)))))

(defun advice-text (model text)
  "The line the working model reads: who answered and what it said."
  (format nil "[advisor ~a] ~a" (or model "model") (string-trim '(#\Space #\Newline #\Tab) text)))

(defun ask-advice (category signal &key note)
  "Ask one question and answer the line to append, or NIL when nothing should
be said. CATEGORY is architecture (the expensive tier may be reached), or a
cheap-only one. Fail-open: an unreachable model leaves the work alone."
  (when (budget-left-p)
    (let* ((mom (multiple-value-list (split-target (mom-spec))))
           (olymp (multiple-value-list (split-target (olymp-spec))))
           (olymp-p (and (second olymp) (first olymp) (olymp-left-p)))
           (gate (and olymp-p (string= category "architecture")))
           (system (or (cdr (assoc signal +prompts+ :test #'string=))
                       (cdr (assoc "stuck" +prompts+ :test #'string=))))
           (user (build-user signal *history* note)))
      (let* ((first (ask-one (cons (first mom) (second mom))
                              (if gate (concatenate 'string system +gate+) system)
                              user)))
        (incf *calls-today*)
        (let ((text (and first (string-trim '(#\Space #\Newline #\Tab) first))))
          (cond
            ((null text)
             (when *last-error*
               (format nil "[advisor unavailable] ~a~%The work continues without advice; the original asked a \
doctor command here." *last-error*)))
            ((zerop (length text)) nil)
            ((string-equal text "NO_ACTION") nil)
            ((and gate (starts-with-p "ESCALATE" text))
             (when (olymp-left-p)
               (incf *olymp-today*)
               (let ((strong (ask-one (cons (first olymp) (second olymp)) system user)))
                 (if (and strong (plusp (length (string-trim '(#\Space #\Newline) strong))))
                     (advice-text (second olymp) strong)
                     (advice-text (second mom) text)))))
            (t (advice-text (second mom) text))))))))

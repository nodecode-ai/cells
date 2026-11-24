;;;; cell.lisp --- loop trap, credential redaction, bounded results, a ledger.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One :tool hook, one advice on the ingress, one command and one help
;;;; topic: the whole cell. Three guards run on the :tool point, which the
;;;; core calls around every tool body (engine/turn.lisp RUN-TOOL-BODY): the
;;;; loop trap refuses the call that would repeat an identical one for the
;;;; third time, and a call allowed through has its result redacted and
;;;; bounded before the model reads it. The fourth guard, on the message a
;;;; person sends, is an advice on NLE:SUBMIT, the one in-process ingress.
;;;;
;;;; Config is a sibling top-level key beside "channels":
;;;;   "sentinel": {"enabled": true, "loop_threshold": 3}
;;;; Every guard is a switch, and the defaults are the original plugin's.

(defpackage #:nodecode-sentinel
  (:use #:cl)
  (:nicknames #:sentinel))

(in-package #:nodecode-sentinel)

(nlk:define-peripheral sentinel :not-running t)

;;; --- credential-shaped text ------------------------------------------------

(defparameter +credential-rules+
  (list (cons (cl-ppcre:create-scanner "(?i)bearer\\s+[a-zA-Z0-9_.-]{20,}")
              "[REDACTED_BEARER_TOKEN]")
        (cons (cl-ppcre:create-scanner "sk-[a-zA-Z0-9]{32,}")
              "[REDACTED_OPENAI_KEY]")
        (cons (cl-ppcre:create-scanner "ghp_[a-zA-Z0-9]{36}")
              "[REDACTED_GITHUB_PAT]")
        (cons (cl-ppcre:create-scanner "(?:AKIA|ABIA|ACCA|ASIA)[0-9A-Z]{16}")
              "[REDACTED_AWS_KEY]")
        (cons (cl-ppcre:create-scanner
               "-----BEGIN\\s+(?:RSA\\s+)?PRIVATE\\s+KEY-----[\\s\\S]+?-----END\\s+(?:RSA\\s+)?PRIVATE\\s+KEY-----")
              "[REDACTED_PRIVATE_KEY]")
        (cons (cl-ppcre:create-scanner
               "(?i)(?:api_key|apikey|secret|password|token)\\s*[:=]\\s*['\"]?[a-zA-Z0-9_.-]{16,}['\"]?")
              "[REDACTED_SECRET]"))
  "The original plugin's six patterns, each with the marker it is replaced by.
Compiled once, at load: the rule set is data, so a reader sees exactly what
is recognized.")

(defun scan-and-redact (text)
  "TEXT with every credential-shaped run replaced by its marker.
=> (values REDACTED FOUND-P)."
  (if (or (null text) (zerop (length text)))
      (values "" nil)
      (let ((redacted text) (found nil))
        (dolist (rule +credential-rules+ (values redacted found))
          (when (cl-ppcre:scan (car rule) redacted)
            (setf found t
                  redacted (cl-ppcre:regex-replace-all (car rule) redacted (cdr rule))))))))

(defun credential-shaped-p (text)
  "Whether TEXT carries a run one of the rules recognizes."
  (nth-value 1 (scan-and-redact text)))

;;; --- a bounded result ------------------------------------------------------

(defun split-lines (text)
  "TEXT's lines, without the empty one a trailing newline would leave."
  (let ((lines '()) (start 0) (n (length text)))
    (loop while (< start n)
          do (let ((end (position #\Newline text :start start)))
               (push (subseq text start (or end n)) lines)
               (setf start (if end (1+ end) n))))
    (nreverse lines)))

(defun line-bounded (lines max-lines)
  "LINES cut to MAX-LINES: the head, a marker saying what went, then the tail."
  (cond ((<= max-lines 0) "")
        ((= max-lines 1) (format nil "~a" (first lines)))
        ((= max-lines 2) (format nil "~a~%[... sentinel: truncated ~d lines ...]"
                                 (first lines) (1- (length lines))))
        (t (let* ((usable (1- max-lines))
                  (head (floor usable 2))
                  (tail (- usable head))
                  (omitted (- (length lines) usable)))
             (format nil "~{~a~^~%~}"
                     (append (subseq lines 0 head)
                             (list (format nil "[... sentinel: truncated ~d lines ...]" omitted))
                             (if (plusp tail) (subseq lines (- (length lines) tail)) '())))))))

(defun char-bounded (working original max-chars)
  "WORKING cut to MAX-CHARS, middle-out, saying how much of ORIGINAL went."
  (let* ((full (format nil " [... sentinel: truncated ~d chars ...] "
                        (- (length original) max-chars)))
         (short " [...] "))
    (cond ((>= max-chars (+ (length full) 4))
           (let* ((usable (- max-chars (length full)))
                  (head (floor usable 2))
                  (tail (- usable head)))
             (concatenate 'string (subseq working 0 head) full
                          (subseq working (- (length working) tail)))))
          ((>= max-chars (+ (length short) 2))
           (let* ((usable (- max-chars (length short)))
                  (head (floor usable 2))
                  (tail (- usable head)))
             (concatenate 'string (subseq working 0 head) short
                          (subseq working (- (length working) tail)))))
          ((> max-chars 3) (concatenate 'string (subseq working 0 (- max-chars 3)) "..."))
          (t (subseq working 0 max-chars)))))

(defun bound-output (text max-chars max-lines)
  "TEXT cut to MAX-LINES lines and MAX-CHARS characters, saying where.
=> (values TEXT TRUNCATED-P), and TEXT is unchanged when nothing had to go."
  (if (or (null text) (string= "" text))
      (values "" nil)
      (let ((max-chars (max 0 max-chars)) (max-lines (max 0 max-lines)))
        (if (or (zerop max-chars) (zerop max-lines))
            (values "" t)
            (let* ((lines (split-lines text))
                   (over-lines (> (length lines) max-lines))
                   (over-chars (> (length text) max-chars))
                   (working (if over-lines (line-bounded lines max-lines) text)))
              (when (> (length working) max-chars)
                (setf working (char-bounded working text max-chars)))
              ;; The last word, whichever half cut first.
              (when (> (length (split-lines working)) max-lines)
                (setf working (line-bounded (split-lines working) max-lines)))
              (when (> (length working) max-chars)
                (setf working (subseq working 0 max-chars)))
              (values working (or over-lines over-chars)))))))

;;; --- the loop trap ---------------------------------------------------------

(defvar *call-history* (make-hash-table :test #'equal)
  "Session id -> the calls it dispatched, newest last: (NAME CANONICAL-ARGS).")
(defvar *reset-epoch* 0
  "How many times the trap has been reset; a new epoch empties every history.")

(defun canonical-form (value)
  "VALUE in a shape two equal calls share: a table becomes its sorted pairs."
  (cond ((hash-table-p value)
         (cons :object
               (sort (loop for key being the hash-keys of value using (hash-value item)
                           collect (cons (princ-to-string key) (canonical-form item)))
                     #'string< :key #'car)))
        ((and (vectorp value) (not (stringp value)))
         (cons :array (map 'list #'canonical-form value)))
        ((consp value) (cons :list (mapcar #'canonical-form value)))
        (t value)))

(defun canonical-arguments (arguments)
  "ARGUMENTS as one printed form, so two identical calls compare equal
whatever order the table happens to hold them in."
  (with-standard-io-syntax
    (let ((*print-length* 60) (*print-level* 10) (*print-circle* t))
      (prin1-to-string (canonical-form arguments)))))

(defun history-tail (session count)
  "The last COUNT calls SESSION dispatched."
  (let ((history (gethash session *call-history*)))
    (if (<= (length history) count) history (last history count))))

(defun loop-trap (session name arguments)
  "The words a call that repeats itself for the THRESHOLD-th time earns, or NIL."
  (let* ((threshold (max 2 (setting :loop-threshold)))
         (tail (history-tail session (1- threshold)))
         (key (canonical-arguments arguments)))
    (when (and (>= (length tail) (1- threshold))
               (every (lambda (entry)
                        (and (string= (first entry) name)
                             (string= (second entry) key)))
                      tail))
      (format nil "tool ~a was dispatched ~d times in a row with identical arguments"
              name threshold))))

(defun record-call (session name arguments)
  "Note that SESSION dispatched NAME with ARGUMENTS, keeping the last 50."
  (let ((history (append (history-tail session 49)
                         (list (list name (canonical-arguments arguments))))))
    (setf (gethash session *call-history*) history))
  (when (> (hash-table-count *call-history*) 200)
    (remhash (loop for key being the hash-keys of *call-history* return key) *call-history*)))

(defun reset-trap ()
  "Forget every call history, and answer the epoch the reset made."
  (clrhash *call-history*)
  (incf *reset-epoch*))

;;; --- the ledger ------------------------------------------------------------

(defvar *ledger* '()
  "Newest first: plists of :TIME :SESSION :EVENT :ACTION :TOOL :REASON.")

(defun timestamp ()
  "The wall clock as one short line."
  (multiple-value-bind (second minute hour day month year) (get-decoded-time)
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
            year month day hour minute second)))

(defun ledger-limit ()
  "How many events the ledger keeps: the running setting, or the default."
  (if *sentinel* (setting :audit-limit) 200))

(defun record-event (session event action &key tool reason)
  "File one event, newest first, keeping no more than the configured count."
  (push (list :time (timestamp) :session session :event event :action action
              :tool tool :reason reason)
        *ledger*)
  (let ((limit (ledger-limit)))
    (when (> (length *ledger*) limit)
      (setf *ledger* (last *ledger* limit)))))

(defun ledger-text (limit)
  "The newest LIMIT events as lines, and what the ledger holds."
  (let ((events (if (<= (length *ledger*) limit) *ledger* (last *ledger* limit))))
    (if (null events)
        "sentinel ledger is empty: no tool call has been guarded yet."
        (with-output-to-string (out)
          (format out "sentinel ledger, newest first (~d shown of ~d):~%"
                  (length events) (length *ledger*))
          (dolist (event (reverse events))
            (format out "~a  ~a  ~a~@[ ~a~]~@[ -- ~a~]~%"
                    (getf event :time) (getf event :event) (getf event :action)
                    (getf event :tool) (getf event :reason)))))))

;;; --- the :tool point -------------------------------------------------------

(defun session-id ()
  "The session this call runs in, or the one name a call outside a turn gets."
  (or (getf (nle:turn) :session-id) "default"))

(defun guard (op next)
  "The :tool point: refuse a repeated call, and bound what a call answers.
OP is (:name TOOL-NAME :arguments TABLE :call-id ID) and NEXT runs the body."
  (let* ((name (or (getf op :name) "unknown"))
         (arguments (getf op :arguments))
         (session (session-id))
         (trap (and (setting :loop-guard) (loop-trap session name arguments))))
    (if trap
        (progn
          (record-event session "pre_tool_call" "BLOCKED" :tool name :reason trap)
          (nle:failure
           (format nil "ERROR: refused by nodecode-sentinel: ~a; the call did not run -- ~
change the arguments, or clear the trap with /sentinel reset" trap)))
        (progn
          (record-call session name arguments)
          (let ((raw (funcall next op)))
            (if (not (stringp raw))
                raw
                (multiple-value-bind (clean redacted)
                    (if (setting :redact) (scan-and-redact raw) (values raw nil))
                  (multiple-value-bind (final bounded)
                      (if (setting :bound-results)
                          (bound-output clean (setting :max-chars) (setting :max-lines))
                          (values clean nil))
                    (cond ((and redacted bounded) (record-event session "transform_tool_result" "REDACTED_AND_TRUNCATED" :tool name :reason (format nil "~d chars in, ~d out" (length raw) (length final))))
                          (redacted (record-event session "transform_tool_result" "REDACTED" :tool name))
                          (bounded (record-event session "transform_tool_result" "TRUNCATED" :tool name :reason (format nil "~d chars in, ~d out" (length raw) (length final))))
                          (t (record-event session "post_tool_call" "COMPLETED" :tool name :reason (format nil "~d chars" (length raw)))))
                    final))))))))

;;; --- the message a person sends -------------------------------------------

(defun check-message (next session prompt &rest keys)
  "An advice on NLE:SUBMIT: a prompt carrying a credential-shaped string is
refused before the turn it would start, and an ordinary one goes through."
  (if (and (setting :guard-messages) (stringp prompt) (credential-shaped-p prompt))
      (progn
        (record-event session "pre_llm_call" "BLOCKED"
                       :reason (format nil "credential-shaped text in a message of ~d characters"
                                       (length prompt)))
        (fail "the message carries a credential-shaped string, so sentinel stops it before ~
the turn starts: remove the secret, or send the message again without it (~d characters ~
were scanned)" (length prompt)))
      (apply next session prompt keys)))

;;; --- the command -----------------------------------------------------------

(define-verb run-slash (args session)
  "One /sentinel: status, audit, reset or clear."
  (let ((sub (string-downcase (nlk:trimmed (or args "")))))
    (cond ((or (string= sub "") (string= sub "status"))
           (format nil "sentinel is on: loop trap at ~d identical calls, results bounded to ~
~d characters and ~d lines, message guard ~:[off~;on~].~%Commands: /sentinel audit, ~
/sentinel reset, /sentinel clear."
                   (setting :loop-threshold) (setting :max-chars) (setting :max-lines)
                   (setting :guard-messages)))
          ((string= sub "audit") (ledger-text 20))
          ((string= sub "reset")
           (format nil "sentinel loop counters cleared (epoch ~d); every trap starts over."
                   (reset-trap)))
          ((string= sub "clear")
           (setf *ledger* '())
           "sentinel ledger cleared.")
          (t (format nil "sentinel: ~a is not a subcommand; try status, audit, reset or clear."
                     sub)))))

(defparameter *help*
  (format nil "sentinel: guards around every tool call. The loop trap refuses a call that ~
repeats an identical one for the third time in a row (a poll that has to repeat answers ~
with changed arguments, or is cleared with /sentinel reset); every result comes back with ~
recognized credentials replaced by a marker and cut to the configured size, so the model ~
reads a shorter text than the tool wrote; a message carrying a credential-shaped string ~
is refused before its turn starts; and each of those is filed in a ledger, newest first. ~
/sentinel status, /sentinel audit, /sentinel reset, /sentinel clear. Settings live in the ~
~s section of the config: loop_threshold, max_chars, max_lines, audit_limit, and one ~
switch each for loop_guard, bound_results, guard_messages and redact."
          "sentinel")
  "The manual topic, and the summary line that rides every request.")

(nle:define-cell sentinel
  (:section ("sentinel")
    (:guide "every guard has a switch and the defaults are the original's; the loop trap is the one users notice, and /sentinel reset clears it")
    ("loop_guard" :boolean :default t
     :doc "refuse a call that repeats an identical one for the loop_threshold-th time in a row")
    ("loop_threshold" :integer :default 3 :min 2
     :doc "how many identical calls in a row the trap allows before the next is refused")
    ("bound_results" :boolean :default t
     :doc "cut every tool result to max_chars and max_lines before the model reads it")
    ("max_chars" :integer :default 4000 :min 200
     :doc "the most characters one tool result may carry")
    ("max_lines" :integer :default 100 :min 5
     :doc "the most lines one tool result may carry")
    ("redact" :boolean :default t
     :doc "replace recognized credential-shaped runs in tool results with a marker")
    ("guard_messages" :boolean :default t
     :doc "refuse a message carrying a credential-shaped string before its turn starts")
    ("audit_limit" :integer :default 200 :min 10
     :doc "how many ledger events /sentinel audit keeps"))
  (:hook :tool #'guard)
  (:hook 'nle:submit #'check-message)
  (:help :sentinel "the loop trap, credential redaction, bounded results and the ledger" *help*)
  (:command "sentinel" 'run-slash
            :description "Sentinel: the loop trap, the ledger, and a reset"
            :argument-hint "status|audit|reset|clear")
  ;; A start is a clean slate for the trap: the counters a stopped cell held
  ;; are not a claim about the run after it. The ledger stays, so a restart
  ;; of the cell does not erase what it watched.
  (:start (lambda () (reset-trap))))

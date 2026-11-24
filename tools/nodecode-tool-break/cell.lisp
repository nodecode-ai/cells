;;;; cell.lisp --- break a stalled tool call without cancelling the turn.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The :TOOL point is around one dispatch, so this cell sees the whole
;;;; call: the row opened before NEXT is what /break-status lists and what
;;;; /break marks, and the mark is read after NEXT returns, where the note
;;;; replaces the tool's own text. An unwind -- Esc, a cancel -- closes the
;;;; row and takes any mark with it, so nothing is left behind.
;;;;
;;;; The cell never kills a process and never interrupts a body: a call that
;;;; never returns is still the shell's cancel. What it changes is what the
;;;; model reads when the call does come back -- "the operator stopped
;;;; waiting for this; do not retry it", plus the operator's steer and the
;;;; tail of whatever the call returned -- and the /again budget, so a model
;;;; cannot loop on the same call.

(defpackage #:nodecode-tool-break
  (:use #:cl))

(in-package #:nodecode-tool-break)

(nlk:define-peripheral tool-break :not-running t :lock "tool-break")

(defparameter +primer+
  "A tool call the operator stopped waiting for comes back as a break note, not
as its own output: it names the call, how long it ran, and says whether to
retry. The operator types /break in their shell to mark the newest call in
flight (its result then reads 'Broken by /break ... do not retry the same
call'), /break MESSAGE to carry their steer in that note, /again to have the
same call reissued once instead, and /break-status for the calls in flight as
JSON. Any of them on a call that has already returned answers that nothing is
in flight. Two /again on one call is the budget; the third is refused."
  "What (help :break) answers: the vocabulary a break note speaks.")

;;; --- the clock ---------------------------------------------------------------

(defun tick () (get-internal-real-time))

(defun seconds-since (started)
  "Seconds between STARTED (a TICK) and now, never negative."
  (max 0.0 (/ (float (- (tick) started) 1.0d0) internal-time-units-per-second)))

(defun duration-text (seconds)
  "SECONDS as 12s, 1m05s or 2h07m."
  (let* ((whole (max 0 (floor seconds)))
         (minutes (floor whole 60))
         (hours (floor minutes 60)))
    (cond ((< whole 60) (format nil "~ds" whole))
          ((< minutes 60) (if (zerop (mod whole 60))
                               (format nil "~dm" minutes)
                               (format nil "~dm~2,'0ds" minutes (mod whole 60))))
          (t (format nil "~dh~2,'0dm" hours (mod minutes 60))))))

;;; --- text --------------------------------------------------------------------

(defun split-words (text)
  "TEXT's words, separated by any run of whitespace."
  (loop with start = nil
        for i from 0 to (length text)
        for separator = (or (= i (length text)) (member (char text i) '(#\Space #\Tab #\Newline #\Return)))
        when (and separator start)
          collect (subseq text start i) and do (setf start nil)
        when (and (not separator) (null start))
          do (setf start i)))

(defun one-line (text limit)
  "TEXT's words on one line, cut to LIMIT characters."
  (let ((joined (format nil "~{~a~^ ~}" (split-words text))))
    (if (> (length joined) limit) (subseq joined 0 limit) joined)))

(defun canonical (value)
  "VALUE as one stable line: a JSON object's members sorted by key, an array
in its own order, so two calls with the same arguments fingerprint alike."
  (cond ((hash-table-p value)
         (format nil "{~{~a~^,~}}"
                 (sort (loop for key being the hash-keys of value using (hash-value member)
                             collect (format nil "~s:~a" key (canonical member)))
                       #'string<)))
        ((stringp value) (format nil "~s" value))
        ((vectorp value)
         (format nil "[~{~a~^,~}]" (map 'list #'canonical value)))
        (t (format nil "~s" value))))

(defun argument-text (arguments)
  "ARGUMENTS as JSON when it is an object, as printed otherwise."
  (cond ((null arguments) "{}")
        ((hash-table-p arguments)
         (handler-case (nlk:encode-json-object arguments) (error () (format nil "~s" arguments))))
        (t (format nil "~s" arguments))))

(defun salvage (text)
  "The tail of TEXT the note keeps: its last (setting :salvage-chars)
characters, trimmed, and nothing when the tool answered nothing."
  (let* ((clean (nlk:trimmed (if (stringp text) text "")))
         (limit (setting :salvage-chars)))
    (cond ((zerop (length clean)) "")
          ((or (not (integerp limit)) (<= (length clean) limit)) clean)
          (t (nlk:trimmed (subseq clean (- (length clean) limit)))))))

(defun one-line-argument (arguments)
  "The argument that says what a call runs, for a one-line label, or NIL."
  (when (hash-table-p arguments)
    (loop for key in '("command" "cmd" "query" "form" "path")
          for value = (gethash key arguments)
          when (and (stringp value) (plusp (length (nlk:trimmed value))))
            return (nlk:trimmed value))))

;;; --- the tables ---------------------------------------------------------------

(defvar *in-flight* (make-hash-table :test #'equal)
  "Call id -> the row its dispatch opened, while the call runs.")

(defvar *broken* (make-hash-table :test #'equal)
  "Call id -> the mark /break or /again laid on a call still running.")

(defvar *again* (make-hash-table :test #'equal)
  "(session name arguments) -> how many times /again reissued that call.")

(defun clear-tables ()
  "Forget every call, mark and budget: what a start and a stop leave."
  (with-tool-break-lock
    (clrhash *in-flight*) (clrhash *broken*) (clrhash *again*)))

(defun install ()
  (clear-tables)
  (nle:on-stop #'clear-tables))

(defun turn-session ()
  "The session this thread works for, or NIL outside a turn."
  (let ((turn (ignore-errors (nle:turn))))
    (and (consp turn) (getf turn :session-id))))

(defun scoped (session-id)
  "SESSION-ID as a usable scope, or NIL meaning every session."
  (let ((text (nlk:trimmed (if (stringp session-id) session-id ""))))
    (and (plusp (length text)) text)))

(defun in-session-p (row session-id)
  "Whether ROW belongs to the session asking. A row with no owner stays
visible everywhere instead of disappearing."
  (or (null session-id)
      (let ((owner (getf row :session-id)))
        (or (null owner) (equal owner session-id)))))

(defun sorted-rows (session-id)
  "This session's calls in flight, newest first. The caller holds the lock."
  (sort (loop for row being the hash-values of *in-flight*
              when (in-session-p row session-id) collect row)
        #'> :key (lambda (row) (getf row :started))))

(defun visible-rows (session-id)
  (with-tool-break-lock (sorted-rows session-id)))

(defun open-call (op)
  "Record the dispatch OP opens and answer its row."
  (let* ((given (getf op :call-id))
         (name (let ((name (getf op :name))) (if (stringp name) name "tool")))
         (id (if (and (stringp given) (plusp (length given)))
                 given
                 (format nil "~a-~d" name (tick))))
         (row (list :id id :name name :arguments (getf op :arguments)
                    :started (tick) :session-id (turn-session))))
    (with-tool-break-lock (setf (gethash id *in-flight*) row))
    row))

(defun close-call (id)
  (with-tool-break-lock
    (remhash id *in-flight*)
    (remhash id *broken*)))

(defun take-mark (id)
  "ROW's mark, taken off the table so it is read once."
  (with-tool-break-lock
    (prog1 (gethash id *broken*)
      (remhash id *broken*))))

(defun again-key (row)
  "The call a /again budget belongs to, and the session that spent it."
  (list (getf row :session-id) (getf row :name) (canonical (getf row :arguments))))

(defun again-count (row)
  "How often /again reissued ROW's call. The caller holds the lock."
  (gethash (again-key row) *again* 0))

;;; --- the note -------------------------------------------------------------------

(defun break-note (mark row text)
  "The tool result that MARK, laid on ROW while it ran, replaces TEXT with."
  (let* ((name (getf row :name))
         (again (getf mark :again))
         (hint (getf mark :hint))
         (waited (duration-text (seconds-since (getf row :started))))
         (chunk (salvage text))
         (lines (if again
                    (list (format nil "Broken by /again after ~a (~a)." waited name)
                          "Reissue this exact call once. Do not loop. The previous attempt was cut short and may have left partial work."
                          ""
                          (format nil "Call: ~a" name)
                          "Args:"
                          (argument-text (getf row :arguments)))
                    (list (format nil "Broken by /break after ~a (~a)." waited name)
                          "The call was stopped so the turn can continue. Do not retry the same call."))))
    (when hint
      (setf lines (append lines
                           (list ""
                                 (if again "Tweak:" "Steer (before any other queued steer):")
                                 hint))))
    (when (plusp (length chunk))
      (setf lines (append lines
                           (list ""
                                 (format nil "Partial output (last ~a characters):" (length chunk))
                                 chunk))))
    (format nil "~{~a~^~%~}" lines)))

(defun break-hook (op next)
  "The :TOOL point, around one dispatch: OP's call is listed while it runs,
and a call the operator broke meanwhile answers the break note, not its own
text."
  (let* ((row (open-call op))
         (id (getf row :id)))
    (unwind-protect
         (let* ((text (funcall next op))
                (mark (take-mark id)))
           (if mark (break-note mark row text) text))
      (close-call id))))

;;; --- the operator's verbs ------------------------------------------------------

(defun parse-args (args)
  "ARGS as the operator typed it => (values TOOL-CALL-ID HINT)."
  (let ((text (nlk:trimmed (if (stringp args) args ""))))
    (cond ((zerop (length text)) (values nil nil))
          ((and (>= (length text) 5) (string= "--id " text :end2 5))
           (let* ((rest (nlk:trimmed (subseq text 5)))
                  (space (position #\Space rest)))
             (if space
                 (values (subseq rest 0 space) (nlk:trimmed (subseq rest space)))
                 (values rest nil))))
          (t (values nil text)))))

(defun find-target (session-id id)
  "=> (values ROW PROBLEM): the call /break or /again acts on for
SESSION-ID, or why there is none. The caller holds the lock."
  (let ((id (nlk:trimmed (if (stringp id) id ""))))
    (if (plusp (length id))
        (let ((row (gethash id *in-flight*)))
          (cond ((null row) (values nil (format nil "No tool call ~a is in flight" id)))
                ((not (in-session-p row session-id))
                 (values nil "That tool call belongs to another session"))
                (t (values row nil))))
        (values (first (sorted-rows session-id)) nil))))

(defun mark-newest (session-id &key id hint again)
  "Mark the call SESSION-ID is waiting on => the one line the operator reads."
  (let ((session (scoped session-id)))
    (with-tool-break-lock
      (multiple-value-bind (row problem) (find-target session id)
        (cond (problem problem)
              ((null row)
               (if hint
                   (format nil "Nothing in flight. A cell's command cannot start a turn; ~
send this as a message:~%~a" hint)
                   (if again "Nothing in flight to reissue" "Nothing in flight")))
              ((and again (>= (again-count row) (setting :again-limit)))
               (format nil "Again used ~a times on this call; break it instead." (again-count row)))
              (t
               (when again (incf (gethash (again-key row) *again* 0)))
               (setf (gethash (getf row :id) *broken*) (list :again again :hint hint))
               (format nil "Broke ~a ~a~a"
                       (getf row :name)
                       (duration-text (seconds-since (getf row :started)))
                       (cond ((and again hint) " · again + hint")
                             (again " · again")
                             (hint " · hint rides the result")
                             (t "")))))))))

(defun row-json (row)
  "ROW as the status frame carries it. The caller holds the lock."
  (let ((argument (one-line-argument (getf row :arguments)))
        (used (again-count row)))
    (nlk:json-object
     "id" (getf row :id)
     "name" (getf row :name)
     "label" (if argument
                  (format nil "~a ~a" (getf row :name) (one-line argument 48))
                  (getf row :name))
     "elapsed_ms" (round (* 1000 (seconds-since (getf row :started))))
     "again_count" used
     "again_disabled" (>= used (setting :again-limit)))))

(defun status-json (session-id)
  "The calls in flight for SESSION-ID, newest first, as one JSON object."
  (with-tool-break-lock
    (let* ((rows (sorted-rows (scoped session-id)))
           (newest (first rows)))
      (nlk:encode-json-object
       (nlk:json-object
        "inflight" (and newest t)
        "count" (length rows)
        :when newest "name" (getf newest :name)
        :when newest "elapsed_ms" (round (* 1000 (seconds-since (getf newest :started))))
        "tools" (coerce (loop for row in rows collect (row-json row)) 'vector))))))

(define-verb run-break (args session-id)
    "One /break: mark the newest call in flight, or ARGS's --id one."
  (multiple-value-bind (id hint) (parse-args args)
    (mark-newest session-id :id id :hint hint :again nil)))

(define-verb run-again (args session-id)
    "One /again: mark the newest call in flight and say to reissue it once."
  (multiple-value-bind (id hint) (parse-args args)
    (mark-newest session-id :id id :hint hint :again t)))

(define-verb run-break-status (args session-id)
    "One /break-status: the calls in flight for the session asking, as JSON."
  (let* ((text (nlk:trimmed (if (stringp args) args "")))
         (explicit (if (and (>= (length text) 13) (string= "--session-id " text :end2 13))
                       (nlk:trimmed (subseq text 13))
                       "")))
    (status-json (if (plusp (length explicit)) explicit session-id))))

;;; --- the cell ------------------------------------------------------------------

(nle:define-cell tool-break
  (:section ("tool-break")
    (:guide "no key and nothing required: the defaults break a call and keep the last 8000 characters of what it returned; again_limit caps /again on one call")
    ("again_limit" :integer :default 2 :min 1
     :doc "how many times /again may reissue one call, once per session")
    ("salvage_chars" :integer :default 8000 :min 0
     :doc "how much of a broken call's own output the note keeps, in characters; 0 keeps none"))
  (:start #'install)
  (:hook :tool #'break-hook)
  (:help :break "break: /break stops the newest tool call in flight without cancelling the turn; its result comes back as a break note" +primer+)
  (:command "break" #'run-break
            :description "Break the tool call in flight; /break {message} says what to do instead"
            :argument-hint "[message]")
  (:command "again" #'run-again
            :description "Break the tool call in flight and reissue it once; /again {hint} tweaks the retry"
            :argument-hint "[hint]")
  (:command "break-status" #'run-break-status
            :description "JSON: the tool calls in flight in this session"
            :argument-hint "[--session-id ID]"))

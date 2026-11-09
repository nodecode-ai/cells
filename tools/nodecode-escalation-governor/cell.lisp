;;;; cell.lisp --- the governor: deterministic stuck-detection for a session.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of escalation-governor's governor.py and __init__.py.
;;;;
;;;; The original premise, kept: a weak model does not know when it is out of
;;;; its depth, so it retries the same failing approach until the session dies.
;;;; The decision to escalate is taken away from the model and given to signals
;;;; that are deterministic -- no judging model, no prompt trick:
;;;;
;;;;   * consecutive tool errors,
;;;;   * the same tool call (same name, same arguments) repeated,
;;;;   * provider errors since the last successful tool result,
;;;;   * model rounds since the last successful tool result.
;;;;
;;;; When a threshold holds, the session is `tripped': the packet -- the trip
;;;; reason, what was tried and how each attempt ended, and the directive to
;;;; hand the rest of the work to a subagent or a stronger model -- rides the
;;;; next tool result of the round (:TOOL point), and the next round's request
;;;; (:ROUND point, NLK:RECORD-TURN-CONTEXT-PROMPT). A successful tool result
;;;; after a trip clears it, which is how a session that recovered on its own
;;;; says so.
;;;;
;;;; Everything is fail-open: a bug in here never costs the round, and a
;;;; blocked call is nothing this cell can do -- it only adds text and writes
;;;; its own report file. Nothing leaves the machine: no network, no subprocess,
;;;; no model call of its own. The packet carries tool names, a bounded hint of
;;;; what each call aimed at, and the operator's own words as the goal.

(in-package #:nodecode-escalation-governor)

(nlk:define-peripheral escalation-governor :not-running t)

(defparameter +owner+ "nodecode-escalation-governor"
  "The cell's key: how its hooks, its notice and its command are filed.")

(defparameter +attempts-kept+ 24
  "Most attempts a session's packet remembers; the original kept the same.")

(defparameter +report-file+ "escalation-governor/reports.jsonl"
  "Where a trip and a recovery are written, under the home.")

(defparameter +hint-keys+ '("path" "file_path" "command" "url" "query" "pattern" "name")
  "The argument keys a call is labelled by, in the order they are tried.")

(defparameter +primer+
  "A weak model does not know when it is out of its depth: left alone it retries the same failing
approach until the session dies. This cell watches deterministic signals only, with no judging model:
consecutive tool errors, the same tool call with the same arguments repeated, provider errors since
the last successful tool result, and model rounds since the last successful tool result. A
successful tool result resets all of them, so only a sustained failure trips. When one threshold
holds the session is tripped, and the packet it builds -- the trip reason, the goal (as
/escalation-governor goal states it), and every attempted call with how it ended -- is appended to
the next tool result of the round and recorded into the next round's request, telling the model to
hand the remaining work to a subagent or a stronger model, or to stop and present the packet to the
operator. A successful tool result after a trip clears it: the session recovered on its own. A trip
and a recovery are appended as one JSON line each to escalation-governor/reports.jsonl under the
home -- counts, tool names and a bounded hint of what each call aimed at, never message text.
Thresholds are the section's settings; 0 turns a signal off.

  /escalation-governor           status: the signals, every session it tracks
  /escalation-governor report    the last report lines it wrote
  /escalation-governor goal TEXT what this session is for, as the packet states it
  /escalation-governor reset     forget every session it tracked"
  "What (help :escalation-governor) answers while the cell runs.")

(defvar *lock* (sb-thread:make-mutex :name "escalation-governor")
  "One lock: a turn, a round and a provider call may land at once.")

(defvar *sessions* (make-hash-table :test #'equal)
  "Session id -> the plist RECORD answers. Live-only: a restart forgets it.")

(defmacro with-state (&body body)
  "Run BODY holding the cell's lock."
  `(sb-thread:with-mutex (*lock*) ,@body))

;;; --- text, labels, signatures ------------------------------------------------

(defun clip-text (text limit)
  "TEXT on one line, at most LIMIT characters, an ellipsis where it was cut."
  (let ((one (or (ignore-errors (nlk:one-line text)) "")))
    (if (> (length one) limit)
        (concatenate 'string (subseq one 0 (max 0 (1- limit))) "...")
        one)))

(defun target-hint (arguments)
  "A bounded hint of what a call aimed at: one argument's value, never the
whole payload."
  (when (hash-table-p arguments)
    (loop for key in +hint-keys+
          for value = (gethash key arguments)
          when (and (stringp value) (plusp (length value)))
            do (return (clip-text value 80)))))

(defun attempt-label (name arguments)
  "How one call is named in a packet: `eval(sh script)', `read(src/x.lisp)'."
  (let ((hint (target-hint arguments)))
    (if hint (format nil "~a(~a)" name hint) name)))

(defun argument-blob (arguments)
  "ARGUMENTS as one comparable string, or an unreadable one's printout."
  (handler-case (nlk:encode-json-object arguments)
    (error () (or (ignore-errors (format nil "~s" arguments)) ""))))

(defun call-signature (name arguments)
  "Two calls are the same call when they share a name and their arguments."
  (list name (argument-blob arguments)))

(defun error-text-p (text)
  "Whether TEXT is a tool result that says it failed."
  (and (stringp text) (uiop:string-prefix-p "ERROR" text)))

;;; --- the session's record ----------------------------------------------------

(defun record (session)
  "SESSION's record, made on first sight. The lock is held by the caller."
  (or (gethash session *sessions*)
      (setf (gethash session *sessions*)
            (list :since (get-universal-time) :goal "" :attempts '()
                  :errors 0 :api-errors 0 :sig nil :repeated 0 :rounds 0
                  :tool-calls 0 :tool-errors 0 :tripped nil :reason ""
                  :injected nil :escalations 0 :recovered nil))))

(defun keep (session plist)
  "File PLIST as SESSION's record and answer it."
  (setf (gethash session *sessions*) plist))

(defun current-session ()
  "The session the running turn belongs to, or \"local\" outside a turn."
  (or (ignore-errors (getf (nle:turn) :session-id)) "local"))

;;; --- the decision ------------------------------------------------------------

(defun trip-reason (plist)
  "The threshold PLIST holds, in words, or NIL when the session is not stuck.
The order is the original's: errors first, then repetition, then drought."
  (let ((errors (or (getf plist :errors) 0))
        (api (or (getf plist :api-errors) 0))
        (repeated (or (getf plist :repeated) 0))
        (rounds (or (getf plist :rounds) 0)))
    (flet ((over (count threshold) (and (plusp threshold) (>= count threshold))))
      (let ((error-floor (setting :consecutive-errors))
            (api-floor (setting :api-errors))
            (repeat-floor (setting :repeat-calls))
            (round-floor (setting :no-progress-rounds)))
        (cond ((over errors error-floor)
               (format nil "~d consecutive tool errors" errors))
              ((over api api-floor)
               (format nil "~d provider errors since the last successful tool result" api))
              ((over repeated repeat-floor)
               (format nil "the same tool call was repeated ~d times without progress" repeated))
              ((over rounds round-floor)
               (format nil "~d model rounds since the last successful tool result" rounds))
              (t nil))))))

(defun packet-text (plist)
  "The escalation packet for a tripped session: why, what to do, what failed."
  (let* ((tried (let ((kept (or (getf plist :attempts) '())))
                  (last kept (min (length kept) (setting :max-packet-actions)))))
         (goal (getf plist :goal))
         (reason (getf plist :reason)))
    (with-output-to-string (out)
      (format out "[ESCALATION GOVERNOR] a deterministic stuck pattern was detected in this session: ~a.~%"
              (if (and (stringp reason) (plusp (length reason))) reason "no progress"))
      (format out "Do not keep retrying the same approach. Escalate now:~%")
      (format out "1. hand the remaining work to a subagent or a stronger model, with this packet as its brief; or~%")
      (format out "2. if there is none, stop and present this packet to the operator as the blocker report.~%")
      (format out "~%Goal: ~a~%"
              (if (and (stringp goal) (plusp (length goal))) goal
                  "(the goal, as the conversation states it)"))
      (if tried
          (progn (format out "Already tried (most recent last):~%")
                 (dolist (entry tried) (format out "- ~a~%" entry)))
          (format out "Already tried: nothing recorded~%"))
      (format out "Whoever takes over must not repeat those approaches verbatim; start from this packet."))))

(defun tripped-now? (plist)
  "Mark PLIST tripped when a threshold holds and answer the reason, else NIL."
  (when (not (getf plist :tripped))
    (let ((reason (trip-reason plist)))
      (when reason
        (setf (getf plist :tripped) t
              (getf plist :reason) reason
              (getf plist :injected) nil
              (getf plist :escalations) (1+ (or (getf plist :escalations) 0)))
        reason))))

(defun packet-ready? (plist)
  "PLIST's packet, once per round, when the session is tripped; else NIL."
  (when (and (getf plist :tripped) (not (getf plist :injected)))
    (setf (getf plist :injected) t)
    (packet-text plist)))

;;; --- the report --------------------------------------------------------------

(defun report-object (session plist event reason)
  "One report line's value: counts, never message text."
  (declare (ignore reason))
  (nlk:json-object "at" (nlk:iso-now)
                    "event" event
                    "session" session
                    "since" (nlk:iso-universal (or (getf plist :since) (get-universal-time)))
                    "tool_calls" (or (getf plist :tool-calls) 0)
                    "tool_errors" (or (getf plist :tool-errors) 0)
                    "rounds" (or (getf plist :rounds) 0)
                    "escalations" (or (getf plist :escalations) 0)
                    "recovered_without_help" (if (getf plist :recovered) 1 0)))

(defun write-report (session plist event)
  "Append one JSON line to the report; a file we cannot write costs nothing."
  (ignore-errors
    (nlk:append-json-line (nlk:home +report-file+)
                           (report-object session plist event nil))))

;;; --- the seams ----------------------------------------------------------------

(defun say-trip (reason)
  "Tell the operator once, so the escalation is not just in the model's head."
  (when (setting :notify)
    (ignore-errors
      (nle:notice (format nil "escalation governor: ~a; the packet is in the model's context now" reason)
                  :level :warning :key +owner+))))

(defun note-call (session name arguments text failed)
  "Fold one finished tool call into SESSION's record; answers the packet to
ride this result, or NIL. The lock is held by the caller."
  (let* ((plist (record session))
         (sig (call-signature name arguments))
         (entry (format nil "~a -> ~a" (attempt-label name arguments)
                         (if failed (clip-text text 80) "ok"))))
    (setf (getf plist :tool-calls) (1+ (or (getf plist :tool-calls) 0))
          (getf plist :attempts)
          (let ((all (append (getf plist :attempts) (list entry))))
            (last all (min (length all) +attempts-kept+))))
    (if failed
        (setf (getf plist :tool-errors) (1+ (or (getf plist :tool-errors) 0))
              (getf plist :errors) (1+ (or (getf plist :errors) 0))
              (getf plist :repeated) (if (equal sig (getf plist :sig))
                                          (1+ (or (getf plist :repeated) 1))
                                          1))
        (setf (getf plist :errors) 0
              (getf plist :api-errors) 0
              (getf plist :rounds) 0
              (getf plist :repeated) (if (equal sig (getf plist :sig))
                                          (1+ (or (getf plist :repeated) 0))
                                          0)
              (getf plist :recovered) (or (getf plist :recovered)
                                           (and (getf plist :tripped) t))
              (getf plist :tripped) nil))
    (when (and (null (getf plist :tripped)) (getf plist :recovered)
               (null (getf plist :reported-recovery)))
      (setf (getf plist :reported-recovery) t)
      (write-report session plist "recovered"))
    (setf (getf plist :sig) sig)
    (keep session plist)
    (let ((reason (tripped-now? plist)))
      (keep session plist)
      (when reason
        (write-report session plist "tripped")
        (say-trip reason)))
    (packet-ready? plist)))

(defun govern-call (op next)
  "The :TOOL point: judge every call, and once a round hand the packet back
appended to a result, which is how the model hears it mid-turn. Fail-open."
  (let* ((name (or (getf op :name) "tool"))
         (arguments (getf op :arguments))
         (session (current-session))
         (values (multiple-value-list (funcall next op))))
    (ignore-errors
      (let* ((first-value (first values))
             (failed (or (and (boundp 'nodecode.evolved::*tool-call-failed*)
                              nodecode.evolved::*tool-call-failed*)
                         (error-text-p first-value)))
             (packet (with-state (note-call session name arguments first-value failed))))
        (when (and packet (stringp first-value))
          (setf (first values) (format nil "~a~%~%~a" first-value packet)))))
    (values-list values)))

(defun note-round (session)
  "One model round began: count it, judge it, and answer the packet when the
session trips or stays tripped. The lock is held by the caller."
  (let ((plist (record session)))
    (setf (getf plist :injected) nil
          (getf plist :rounds) (1+ (or (getf plist :rounds) 0)))
    (let ((reason (tripped-now? plist)))
      (setf (gethash session *sessions*) plist)
      (when reason
        (write-report session plist "tripped")
        (say-trip reason)))
    (packet-ready? plist)))

(defun observe-round (op next)
  "The :ROUND point: the packet a tripped session needs is in this round's
request, before the provider is asked. Isolated by the core; fail-open here too."
  (ignore-errors
    (let* ((session (or (getf op :session-id) (current-session)))
           (turn (getf op :turn))
           (packet (with-state (note-round session))))
      (when (and packet turn)
        (ignore-errors (nlk:record-turn-context-prompt turn :text packet)))))
  (funcall next op))

(defun note-api-error (session)
  "A provider call failed: that is a signal too, and never a round."
  (ignore-errors
    (let (reason)
      (with-state
        (let ((plist (record session)))
          (setf (getf plist :api-errors) (1+ (or (getf plist :api-errors) 0)))
          (setf reason (tripped-now? plist))
          (setf (gethash session *sessions*) plist)
          (when reason (write-report session plist "tripped"))))
      (when reason (say-trip reason)))))

(defun watch-provider (next &rest args)
  "Around one provider call: an error it raises is counted, then re-signalled,
so the round fails exactly as it would have."
  (handler-case (apply next args)
    (error (condition)
      (note-api-error (current-session))
      (error condition))))

;;; --- /escalation-governor ----------------------------------------------------

(defun settings-line ()
  (format nil "trips on ~d consecutive tool errors, ~d provider errors, the same call repeated ~d times, or ~d rounds with no successful tool result (0 turns one off); packet keeps ~d attempts; notices ~a"
          (setting :consecutive-errors) (setting :api-errors) (setting :repeat-calls)
          (setting :no-progress-rounds) (setting :max-packet-actions)
          (if (setting :notify) "on" "off")))

(defun session-line (session plist)
  (format nil "  ~a: ~d calls, ~d failed, ~d rounds, ~d escalations, ~a~@[ (~a)~]~@[ · goal: ~a~]"
          session
          (or (getf plist :tool-calls) 0) (or (getf plist :tool-errors) 0)
          (or (getf plist :rounds) 0) (or (getf plist :escalations) 0)
          (if (getf plist :tripped) "tripped" "clear")
          (getf plist :reason)
          (let ((goal (getf plist :goal)))
            (and (stringp goal) (plusp (length goal)) goal))))

(defun status-text ()
  (with-state
    (let ((rows (sort (loop for session being the hash-keys of *sessions*
                             collect (cons session (gethash session *sessions*)))
                       #'string< :key #'car)))
      (format nil "escalation governor: ~a~%~{  ~a~%~}~@[~%  /escalation-governor report shows the last report lines~]"
              (settings-line)
              (mapcar (lambda (row) (session-line (car row) (cdr row))) rows)
              rows))))

(defun report-text ()
  "The last few report lines, or a word that there are none yet."
  (let* ((path (nlk:home +report-file+))
         (text (ignore-errors (uiop:read-file-string path)))
         (lines (and text (remove "" (nlk:lines text) :test #'string=))))
    (if lines
        (format nil "escalation governor: last ~d report lines~%~{~a~%~}" (length (last lines 5)) (last lines 5))
        "escalation governor: no report lines yet (one is written when a session trips)")))

(defun help-text ()
  (format nil "/escalation-governor - the signals, the thresholds and every session it tracks~%  /escalation-governor report - the last report lines it wrote~%  /escalation-governor goal TEXT - what this session is for, as the packet states it~%  /escalation-governor reset - forget every session it tracked~%  /escalation-governor help - this"))

(defun run-slash (args session-id)
  "One /escalation-governor invocation: the state, the report, the goal, a reset."
  (let* ((text (or args ""))
         (words (nlk:split-words text))
         (head (string-downcase (or (first words) ""))))
    (cond ((string= head "help") (help-text))
          ((string= head "report") (report-text))
          ((string= head "reset")
           (let ((count (with-state (hash-table-count *sessions*))))
             (with-state (clrhash *sessions*))
             (format nil "escalation governor: forgot ~d session~:p" count)))
          ((string= head "goal")
           (let ((goal (string-trim " " (subseq text (min (length text) (length (first words)))))))
             (if (zerop (length goal))
                 (format nil "escalation governor: the goal of ~a is ~s" (or session-id "local")
                         (with-state (getf (record (or session-id "local")) :goal)))
                 (progn
                   (with-state
                     (let ((plist (record (or session-id "local"))))
                       (setf (getf plist :goal) (clip-text goal 300))
                       (keep (or session-id "local") plist)))
                   (format nil "escalation governor: the packet of ~a now opens on: ~a"
                           (or session-id "local") (clip-text goal 300))))))
          ((zerop (length head)) (status-text))
          (t (format nil "escalation governor: ~a is not one of its verbs~%~a" (first words) (help-text))))))

;;; --- the declaration ------------------------------------------------------------

(nle:define-cell escalation-governor
  (:section ("escalation-governor")
    (:guide "nothing here is required; the defaults trip after 3 consecutive tool errors, 3 provider errors, the same call 3 times, or 12 rounds with no successful tool result; 0 turns a signal off")
    ("consecutive_errors" :integer :default 3 :min 0
     :doc "tool errors in a row that trip the session; 0 never trips on them")
    ("api_errors" :integer :default 3 :min 0
     :doc "provider errors since the last successful tool result; 0 turns it off")
    ("repeat_calls" :integer :default 3 :min 0
     :doc "the same call with the same arguments, repeated, that trips; 0 turns it off")
    ("no_progress_rounds" :integer :default 12 :min 0
     :doc "model rounds since the last successful tool result; 0 turns it off")
    ("max_packet_actions" :integer :default 12 :min 0
     :doc "how many attempted calls the packet lists, newest last")
    ("notify" :boolean :default t
     :doc "tell the operator, in a notice, when a session trips"))
  (:hook :tool #'govern-call)
  (:hook :round #'observe-round)
  (:hook 'nodecode.evolved::call-provider #'watch-provider)
  (:command "escalation-governor" #'run-slash
            :description "Escalation governor: the signals it watches, every session, and its report"
            :argument-hint "[report | goal TEXT | reset | help]")
  (:help :escalation-governor
   "trips a session that is measurably stuck, and puts an escalation packet in the model's context"
   +primer+)
  (:start (lambda ()
            (with-state (clrhash *sessions*))
            (nle:on-stop (lambda () (with-state (clrhash *sessions*)))))))

;;;; cell.lisp --- the whole brownout cell: one poll, one hook, one command.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of Osiyomeoh/brownout. The original ran a watcher thread and
;;;; a pre_tool_call hook; so does this. What it did through `hermes send'
;;;; (a message to a chat target) is a standing notice here: every attached
;;;; shell shows it, and the model reads it in the notices block of its next
;;;; request, so a host on battery says so where both a person and the agent
;;;; see it, with no credential and no network of its own.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "brownout": {"poll_seconds": 15, "critical_pct": 15,
;;;;                 "critical_minutes": 10, "auto_resume": true,
;;;;                 "ups": "", "allow_when_parked": []}

(in-package #:nodecode-brownout)

(nlk:define-peripheral brownout :not-running t :lock "brownout")

(defparameter +primer+
  "Brownout watches this host's power. It reads the battery every poll_seconds
(pmset on macOS, /sys/class/power_supply on Linux, or a NUT UPS named by
`ups'), stands a notice when mains power drops, and measures the real runoff
from the drain it has seen. Reply /brownout park and it holds every tool call
until the power is back or you reply /brownout resume; at critical_pct with no
answer it parks by itself, which a resume undoes. Nothing is killed, deleted
or stopped mid-write.

  /brownout              the state, the charge, the measured runway, the verbs
  /brownout park         hold new tool calls; the turn's next call is refused
  /brownout keep         carry on: no automatic park during this stretch
  /brownout resume       let tool calls through again
  /brownout test         stand a test notice, to see the path end to end
  /brownout replay FILE  run a JSONL trace of readings through the engine,
                         off the machine and with nothing performed

While parked, every tool call is refused except the brownout tool itself and
the tools named in allow_when_parked, so the way out is /brownout resume."
  "What (help :brownout) answers while the cell runs.")

(defparameter +actions+ '("status" "park" "keep" "resume" "test")
  "What the brownout tool will do, in the order its schema advertises.")

(defvar *engine* nil
  "The host's power history while the cell runs, or NIL.")

(defvar *worker* nil
  "The polling thread while it runs, or NIL.")

(defvar *recent* '()
  "The last few things Brownout said, newest first: what /brownout status
shows once the notice has been replaced by a later one.")

(defvar *workers* t
  "Whether START-CELL polls on a thread; a test polls by hand with TICK.")

(defvar *read-power* nil
  "The reader a test replaces the host with; NIL reads the host itself.")

;;; --- saying it -----------------------------------------------------------------

(defun deliver (text &key (level :info))
  "Stand TEXT under the cell's key: every attached shell shows it, and the
agent reads it in the notices block of its next request. The last few are
kept for /brownout status."
  (setf *recent* (subseq (cons text *recent*) 0 (min 5 (1+ (length *recent*)))))
  (nle:notice text :level level :key "nodecode-brownout")
  text)

(defun cfg ()
  "The decision's own settings, read once per tick."
  (list :critical-pct (setting :critical-pct)
        :critical-minutes (setting :critical-minutes)
        :auto-resume (and (setting :auto-resume) t)))

(defun current-sample ()
  "The host's reading now, or a test's scripted one."
  (let ((reader *read-power*))
    (if reader
        (funcall reader)
        (read-power-from-host (setting :ups)))))

(defun alert-text ()
  (let* ((sample (first (eng-samples *engine*)))
         (pct (and sample (sample-pct sample)))
         (left (and sample (sample-os-minutes sample))))
    (format nil "Brownout: power lost on this machine. Battery ~a.~@[ The OS estimates ~d:~2,'0d left.~] Reply /brownout park to hold tool calls, or /brownout keep to carry on."
            (if pct (format nil "~d%" pct) "unknown charge")
            (and left (floor left 60)) (and left (mod left 60)))))

(defun park-text ()
  "Brownout: parked on battery. New tool calls are held until /brownout resume or the power is back. Nothing was killed, deleted or stopped mid-write.")

(defun runway-line ()
  (let ((value (runway (eng-samples *engine*))))
    (if (runway-minutes value)
        (format nil "Brownout: measured runway ~a (~a). Reply /brownout park or /brownout keep."
                (runway-text *engine*) (runway-basis value))
        nil)))

(defun perform (decision)
  "Say what DECISION did: one standing notice per action worth a word."
  (dolist (action (getf decision :actions))
    (let ((text (case action
                  (:alert (alert-text))
                  (:park (park-text))
                  (:runway (runway-line))
                  (:notify-resumed "Brownout: power is back. Tool calls are open again.")
                  (:notify-back "Brownout: power is back. Nothing was parked.")
                  (:notify-unknown "Brownout: no power reading is available on this host, so nothing is acted on.")
                  (t nil))))
      (when text
        (deliver text :level (if (member action '(:alert :park :notify-unknown)) :warning :info)))))
  decision)

;;; --- one poll ------------------------------------------------------------------

(defun tick (&optional sample)
  "One poll: read the host unless SAMPLE is given, advance the engine, say
what changed. => the decision."
  (with-brownout-lock
    (let* ((reading (or sample (current-sample)))
           (decision (engine-tick *engine* reading (cfg))))
      (perform decision))))

;;; --- the verbs -----------------------------------------------------------------

(defun status-text ()
  (with-output-to-string (out)
    (format out "Brownout: ~a, ~a. Runway: ~a."
            (state-text *engine*) (power-text *engine*) (runway-text *engine*))
    (when (eq (eng-state *engine*) :parked)
      (format out " Tool calls are held until /brownout resume or the power is back."))
    (when (eng-kept *engine*)
      (format out " Kept: no automatic park during this battery stretch."))
    (when *recent*
      (format out "~%Last: ~a" (first *recent*)))
    (format out "~%Usage: /brownout status|park|keep|resume|test|replay <trace.jsonl>")))

(defun apply-verb (verb)
  "Apply one person's VERB and answer what it did, applied at once as the
original did rather than at the next poll."
  (with-brownout-lock (engine-command *engine* verb))
  (tick)
  (cond ((and (eq verb :park) (not (eq (eng-state *engine*) :parked)))
         (format nil "Brownout: this host is on mains power, so there is nothing to park. ~a"
                 (status-text)))
        (t (format nil "Brownout: ~(~a~) applied. ~a" verb (status-text)))))

(defun read-trace (path)
  "The readings in PATH, a JSONL trace of {\"t\", \"on_ac\", \"pct\"} lines,
oldest first. A line that is not a reading is skipped."
  (with-open-file (in path :direction :input)
    (loop for line = (read-line in nil)
          while line
          for record = (ignore-errors (nlk:decode-json (string-trim " " line)))
          when (hash-table-p record)
            collect (make-sample :time (or (nlk:json-value record :number "t") 0)
                                 :on-ac (and (nlk:json-value record :boolean "on_ac") t)
                                 :pct (nlk:json-value record :number "pct")
                                 :source "trace"))))

(defun replay-samples (samples)
  "Run SAMPLES through an engine of its own, with nothing performed, and
answer the transcript: one line per decision that did something."
  (let ((engine (make-engine))
        (out '())
        (first-time nil))
    (loop for sample in samples
          for index from 0
          do (let ((decision (engine-tick engine sample (cfg))))
               (setf first-time (or first-time (sample-time sample)))
               (when (or (getf decision :actions) (zerop index))
                 (push (format nil "+~,1f min  ~a -> ~(~a~) ~s (~a)"
                               (/ (- (sample-time sample) first-time) 60.0)
                               (power-text engine)
                               (getf decision :state)
                               (getf decision :actions)
                               (getf decision :reason))
                       out))))
    (format nil "~{~a~%~}" (nreverse out))))

(defun replay-answer (path)
  (let ((name (nonblank path)))
    (cond ((null name) "Usage: /brownout replay <trace.jsonl>")
          ((not (probe-file name)) (format nil "Brownout: ~a is not a file here" name))
          (t (handler-case (replay-samples (read-trace (pathname name)))
               (error (condition)
                 (format nil "Brownout: replay failed: ~a" condition)))))))

(defun answer-for (head value)
  "The answer to HEAD, with VALUE as the rest of the typed line: one route
for the command and the tool."
  (cond ((or (equal head "") (equal head "status")) (status-text))
        ((equal head "park") (apply-verb :park))
        ((equal head "keep") (apply-verb :keep))
        ((equal head "resume") (apply-verb :resume))
        ((equal head "test")
         (deliver "Brownout test: a notice from this cell reaches you here.")
         "Brownout: test notice posted; every attached shell shows it.")
        ((equal head "replay") (replay-answer value))
        (t (format nil "~a is not one of its choices; /brownout alone shows them" head))))

(defun split-verb (raw)
  "RAW as (values HEAD VALUE): the first word downcased, and the rest."
  (let* ((text (nlk:trimmed (or raw "")))
         (space (position #\Space text)))
    (if space
        (values (string-downcase (subseq text 0 space))
                (string-trim " " (subseq text (1+ space))))
        (values (string-downcase text) ""))))

(defun run-slash (args session-id)
  "The answer to one /brownout invocation."
  (declare (ignore session-id))
  (multiple-value-bind (head value) (split-verb args)
    (answer-for head value)))

;;; --- the hook, and the tool ------------------------------------------------------

(defun parked-p ()
  (let ((engine *engine*))
    (and engine (with-brownout-lock (eq (eng-state engine) :parked)))))

(defun allowed-tool-p (name)
  "Brownout's own tool always answers — it is how the agent and the person
see the way out — and the section may name others."
  (or (equal name "brownout")
      (member name (or (setting :allow-when-parked) '()) :test #'equal)))

(defun guard-tool (op next)
  "The :TOOL point hook: while parked, every call but Brownout's own is
refused, before any hook or body runs. Refusing is answering without calling
NEXT, so the refusal is an ordinary tool result the model reads."
  (let ((name (getf op :name)))
    (if (and (parked-p) (not (allowed-tool-p name)))
        (format nil "Brownout: this host is on battery and work is parked. The call was not run. ~
Reply /brownout resume to continue anyway. (~a)"
                (status-text))
        (funcall next op))))

(nlk:define-contract brownout-arguments
  (:doc "The power of the machine Nodecode runs on: whether it is on mains or battery, the charge and the runway measured from the real drain, and the verbs that park work when the grid is unreliable. park holds every tool call until resume or the power is back. Reading it costs nothing and performs nothing.")
  ("action" :text :required t :options +actions+
            :doc "status reads; park holds tool calls; keep cancels the automatic park; resume lets calls through; test stands a test notice"))

(defun brownout-tool (args)
  "The brownout tool: one verb, answered through the route the command uses."
  (let ((head (string-downcase (brownout-arguments-action args))))
    (cond ((member head '("park" "keep" "resume") :test #'equal)
           (apply-verb (intern (string-upcase head) :keyword)))
          (t (answer-for head "")))))

;;; --- the entry -------------------------------------------------------------------

(defun install ()
  (setf *engine* (make-engine) *recent* '() *worker* nil)
  (nle:on-stop (lambda ()
                 (setf *engine* nil *recent* '() *worker* nil)
                 (nle:notice nil :key "nodecode-brownout")))
  (when *workers*
    (tick)                     ; one reading at start, so the state is known
    (setf *worker* (nlk:worker-start "nodecode-brownout" #'tick
                                      :wake (lambda () (setting :poll-seconds))))
    (nle:on-stop (lambda () (setf *worker* (nlk:worker-stop *worker*))))))

(defmacro with-readings ((&rest samples) &body body)
  "Run BODY with the host's power replaced by SAMPLES, handed out one per
poll and then the last again, and with no polling thread: how a test makes a
power loss happen on a machine that has mains power."
  (let ((queue (gensym "QUEUE")) (all (gensym "SAMPLES")))
    `(let* ((,all (list ,@samples))
            (,queue (copy-list ,all))
            (*read-power* (lambda () (or (pop ,queue) (car (last ,all)))))
            (*workers* nil))
       ,@body)))

(nle:define-cell brownout
  (:section ("brownout")
    (:guide "nothing here is required; poll_seconds sets how fast a power loss is noticed, and the critical pair only decides when a park happens without an answer")
    ("poll_seconds" :integer :default 15 :min 2 :doc "seconds between power polls")
    ("critical_pct" :integer :default 15 :min 1 :doc "charge at or below which the host parks itself")
    ("critical_minutes" :integer :default 10 :min 1 :doc "measured runway at or below which the host parks itself")
    ("auto_resume" :boolean :default t :doc "resume what a park holds once the power is back")
    ("ups" :string :doc "a NUT UPS name for `upsc'; empty reads the OS directly")
    ("allow_when_parked" :list :doc "tools that still run while parked, beside the brownout tool"))
  (:hook :tool #'guard-tool)
  (:tool "brownout" (nlk:contract 'brownout-arguments) 'brownout-tool)
  (:help :brownout "the host's power: a notice when mains power drops, the measured runway, and a park that holds tool calls" +primer+)
  (:command "brownout" #'run-slash
            :description "The host's power: status, and park or resume the work"
            :argument-hint "status|park|keep|resume|test|replay")
  (:start #'install))

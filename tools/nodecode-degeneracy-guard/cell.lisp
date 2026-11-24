;;;; cell.lisp --- the guard itself: the :TOOL ask, the incident file, the
;;;; long-session notice, the check tool and /degeneracy.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A rewrite of degeneracy-guard's __init__.py, tool_arg_guard.py,
;;;; session_length_guard.py and paths.py.
;;;;
;;;; The original premise, kept: a model in a repeat loop can say "I am
;;;; repeating" and still cannot stop, so the breaker runs outside the model.
;;;; In Nodecode one tool -- eval -- writes the files and runs the programs,
;;;; so a loop lands in the arguments of a call, and a call is the only part
;;;; of a turn that changes anything outside the process. That is where the
;;;; guard sits: the :TOOL point, before the tool runs.
;;;;
;;;; It asks rather than refuses, because content alone cannot separate a
;;;; deliberately repeated payload from a stuck loop -- a thousand-row zero
;;;; matrix is data in one call and pathology in the next, and only the person
;;;; knows which. The question is the core's own ASK-OPERATOR gate: Allow
;;;; once, or Deny; nobody there, or no answer in time, refuses, so the guard
;;;; is fail-closed when nobody is watching. `mode' chooses instead to refuse
;;;; outright or only to record and warn.
;;;;
;;;; The gate is asked, and the answer read, the way nodecode-guard's `ask'
;;;; does it; an incident is one JSON file under
;;;; <home>/degeneracy-guard/incidents/, pruned to the newest `incidents'.

(in-package #:nodecode-degeneracy-guard)

(nlk:define-peripheral degeneracy-guard :not-running t)

(defparameter +owner+ "nodecode-degeneracy-guard"
  "The cell's key: how its hook, its notice and its command are filed.")

(defparameter +allow-once+ "Allow once")
(defparameter +deny+ "Deny")

(defparameter *renotify-every* 50
  "Model requests between one long-session notice and the next, so the notice
does not appear on every round.")

(defparameter +primer+
  "This cell watches the arguments of every tool call for a repetition loop:
the same line twelve times over, or one short block repeated many times, with
no data shape around it (a table, a JSON array, a matrix, a log, a CSV or a
YAML/TOML block all repeat on purpose and pass). When a call's argument looks
like a loop, the guard asks the person at the page: Allow once, or Deny. No
answer, or nobody there, refuses the call. Set `degeneracy_guard.mode' to
\"refuse\" to stop such a call outright, or \"warn\" to only record it.
`allow_tools' exempts tools by name.

  /degeneracy              state: the mode, the thresholds, the incidents kept
  /degeneracy check TEXT   judge TEXT as a tool argument would be judged
  /degeneracy last         the newest incident
  /degeneracy clear        drop the incident files

The tool degeneracy_check(text, kind) answers the same judgement for a text
the model hands it, without touching a call. Incidents are JSON files under
the home, one per event, and nothing leaves the machine."
  "What (help :degeneracy-guard) answers while the cell runs.")

;;; --- the incident file --------------------------------------------------------------

(defun incident-directory ()
  "Where incidents are written: the cell's own folder under the home."
  (nlk:home "degeneracy-guard/incidents/"))

(defun safe-tag (text)
  "TEXT as a file name's part: letters, digits, . _ - and nothing else."
  (let ((out (make-string-output-stream)))
    (loop for char across (or text "")
          do (write-char (if (or (alphanumericp char) (find char "._-")) char #\_) out))
    (let ((name (get-output-stream-string out)))
      (if (zerop (length name)) "incident" (subseq name 0 (min 48 (length name)))))))

(defvar *incident-stamp* 0
  "A counter inside one second's stamp, so two incidents never share a file.")

(defun incident-files ()
  "The incident files, newest first."
  (let ((files (ignore-errors (uiop:directory-files (incident-directory)))))
    (sort (copy-list files) #'string> :key #'pathname-name)))

(defun prune-incidents ()
  "Keep the newest `incidents' files, drop the rest."
  (let ((keep (or (setting :incidents) 200)))
    (loop for file in (nthcdr keep (incident-files))
          do (ignore-errors (delete-file file)))))

(defun keep-incident (kind tool text verdict)
  "Record one incident as a JSON file; answer its name, or NIL when the
write failed (an incident is a record, never a reason to fail a call)."
  (handler-case
      (let* ((stamp (format nil "~d-~6,'0d-~4,'0x"
                            (get-universal-time)
                            (mod (get-internal-real-time) 1000000)
                            (mod (incf *incident-stamp*) #x10000)))
             (path (merge-pathnames
                    (format nil "~a_~a.json" stamp
                            (safe-tag (format nil "~a_~a" kind tool)))
                    (incident-directory)))
             (metrics (getf verdict :metrics))
             (object (nlk:json-object
                      "ts" (nlk:iso-now)
                      "kind" kind
                      "tool" (or tool "")
                      "reason" (getf verdict :reason)
                      "detail" (getf verdict :detail)
                      "arg_chars" (length text)
                      "metrics" (nlk:plist-json metrics)
                      "action" "approve_requested"
                      "approval_outcome" "not observable from this hook"
                      "tail_sample" (subseq text (max 0 (- (length text) 800))))))
        (nlk:write-file-atomically path (nlk:encode-json-object object))
        (prune-incidents)
        (pathname-name path))
    (error () nil)))

;;; --- what a call's arguments say -----------------------------------------------------

(defun argument-strings (value &optional (depth 0) (found '()))
  "Every string VALUE carries, nested objects and arrays included."
  (cond ((> depth 6) found)
        ((stringp value) (cons value found))
        ((hash-table-p value)
         (loop for inner being the hash-values of value
               do (setf found (argument-strings inner (1+ depth) found)))
         found)
        ((vectorp value)
         (loop for inner across value
               do (setf found (argument-strings inner (1+ depth) found)))
         found)
        (t found)))

(defparameter +own-tool+ "degeneracy_check"
  "The guard's own verb. It is never judged: the text it is handed is exactly
the text a call would be refused for carrying.")

(defun exempt-tool-p (tool)
  "Whether TOOL is never judged: the guard's own verb, or a name the operator
listed in `allow_tools'."
  (or (string= (or tool "") +own-tool+)
      (member (or tool "") (setting :allow-tools) :test #'string=)))

(defun judge-call (arguments)
  "The first argument string of ARGUMENTS whose repetition earns a question.
=> (values TEXT VERDICT), or NIL values when the call is ordinary."
  (let ((min-chars (th *argument-thresholds* :min-chars)))
    (dolist (text (argument-strings arguments))
      (when (and (<= min-chars (length text)))
        (let ((verdict (judge-argument text)))
          (when (getf verdict :ask)
            (return-from judge-call (values text verdict))))))
    (values nil nil)))

;;; --- asking, refusing, warning --------------------------------------------------------

(defun ask-operator-function ()
  "The core's ASK-OPERATOR, or NIL in a core that has none."
  ;; Read by name, as nodecode-guard reads it: this cell also loads in a core
  ;; released before the question board.
  (let ((symbol (find-symbol "ASK-OPERATOR" '#:nodecode.evolved)))
    (and symbol (fboundp symbol) (symbol-function symbol))))

(defun question-detail (text verdict)
  "What the question shows: the judgement, then the argument's own head."
  (format nil "~a (~a).~%~a"
          (getf verdict :detail) (getf verdict :reason)
          (nlk:clip text 800)))

(defun refusal-text (tool verdict why)
  "The model's words for a call that did not run."
  (format nil "ERROR: refused by ~a: one argument of this call to ~s is a repetition loop ~a; the call did not run. Shorten the argument, or write it with varying content, and try again -- and do not work around the guard without saying so."
          +owner+ (or tool "") why))

(defun refuse (tool verdict why)
  (nle:failure (refusal-text tool verdict why)))

(defun ask-and-run (tool text verdict op next)
  "Ask the person watching: Allow once runs the call, anything else refuses it."
  (let ((ask (ask-operator-function)))
    (if (null ask)
        (refuse tool verdict "nobody was there to ask")
        (multiple-value-bind (answer why)
            (funcall ask
                     (format nil "A call to ~a looks like a repetition loop" (or tool "a tool"))
                     :detail (question-detail text verdict)
                     :choices (list +allow-once+ +deny+)
                     :seconds (max 5 (or (setting :ask-seconds) 120)))
          (cond ((equal answer +allow-once+) (funcall next op))
                ((eq why :nobody) (refuse tool verdict "nobody was there to ask"))
                ((eq why :timeout) (refuse tool verdict "the question was not answered in time"))
                (t (refuse tool verdict "the operator was asked and chose Deny")))))))

(defun mode-name ()
  "The section's `mode' as the word it is, whatever the reader answered."
  (string-downcase (string (setting :mode))))

(defun act (tool text verdict op next)
  "Record the incident, then do what `mode' says with the call."
  (keep-incident "TOOL_ARG_GUARD" tool text verdict)
  (let ((mode (mode-name)))
    (cond ((string= mode "warn")
           (nle:notice (format nil "degeneracy guard: ~a in a call to ~a; it ran"
                                (getf verdict :reason) (or tool "a tool"))
                       :level :warning :key +owner+)
           (funcall next op))
          ((string= mode "refuse")
           (refuse tool verdict "it was refused before it ran"))
          (t (ask-and-run tool text verdict op next)))))

(defun inspect-call (op next)
  "The :TOOL point: a call whose argument is a repetition loop is recorded
and then asked about, refused, or only warned about, by `mode'."
  (let ((arguments (getf op :arguments))
        (tool (or (getf op :name) "")))
    (if (and (hash-table-p arguments) (not (exempt-tool-p tool)))
        (multiple-value-bind (text verdict) (judge-call arguments)
          (if text (act tool text verdict op next) (funcall next op)))
        (funcall next op))))

;;; --- the long-session notice ----------------------------------------------------------

(defvar *rounds* (make-hash-table :test #'equal)
  "Session id -> (rounds-so-far . rounds-at-the-last-notice).")

(defun session-notice (rounds last-notified &key (max-rounds 500) (renotify *renotify-every*))
  "The long-session notice ROUNDS model requests into a session earn, or NIL.
One notice at MAX-ROUNDS and one every RENOTIFY requests after it, as the
original de-bounced its own."
  (when (and (plusp max-rounds)
             (<= max-rounds rounds)
             (or (null last-notified) (zerop last-notified)
                 (<= renotify (- rounds last-notified))))
    (format nil "[Session length notice]~%  \
This session has made ~:d model requests (the guard suggests a new session after ~:d). A very long \
session is where repetition loops come from: repetition is what a model falls back on when the \
context stops fitting, and detection only repairs the symptom.~%  \
Write the current state to disk, then start a new session with just the conclusions you need."
            rounds max-rounds)))

(defun observe-round (op)
  "The :ROUND point: a session past MAX-ROUNDS hears one notice, once."
  (let ((session (getf op :session-id))
        (turn (getf op :turn)))
    (when (and (stringp session) turn (nlk:store-open-p))
      (let* ((entry (gethash session *rounds*))
             (rounds (1+ (or (car entry) 0)))
             (last (or (cdr entry) 0))
             (text (session-notice rounds last :max-rounds (setting :max-rounds))))
        (when text
          (nlk:record-turn-context-prompt turn :text text))
        (setf (gethash session *rounds*) (cons rounds (or (and text rounds) last)))))))

;;; --- the check tool ------------------------------------------------------------------

(defparameter +kinds+ '("argument" "output")
  "What a text is judged as: tool arguments and model output have different
rules, and the rules are frozen apart.")

(nlk:define-contract degeneracy-check
  (:doc "Judge a text for a degeneracy loop: the same line, or the same short
block, repeated many times. Answers the verdict, what triggered it, and the
measurements behind it. It records nothing and changes nothing.")
  ("text" :text :required t :doc "the text to judge")
  ("kind" :text :default "argument" :options +kinds+
   :doc "argument judges one tool argument's payload (data shapes exempt); output judges model text"))

(defun check-tool (report)
  "The model-facing verb: a judgement and the numbers behind it, in words."
  (let* ((text (degeneracy-check-text report))
         (kind (degeneracy-check-kind report))
         (verdict (if (string= kind "output")
                      (check-text text)
                      (judge-argument text))))
    (if (string= kind "output")
        (if (getf verdict :triggered)
            (format nil "LOOP: ~a (~a), at line ~d. ~a"
                    (getf verdict :reason) (getf verdict :detail)
                    (getf verdict :line-index) (metric-line text kind))
            (format nil "CLEAN: no repetition loop found. ~a" (metric-line text kind)))
        (if (getf verdict :ask)
            (format nil "LOOP: ~a (~a). A call carrying this argument would be asked about before it ran. ~a"
                    (getf verdict :reason) (getf verdict :detail) (metric-line text kind))
            (format nil "CLEAN: ~a. ~a"
                    (if (plusp (length (getf verdict :detail)))
                        (getf verdict :detail) "no repetition worth a question")
                    (metric-line text kind))))))

(defun metric-line (text kind)
  "The measurements behind a judgement, one line: what a person reads to see
why the rule answered as it did."
  (let* ((lines (remove "" (nlk:lines text) :test #'string=))
         (distinct (length (nlk:distinct lines))))
    (format nil "~:d chars, ~:d non-blank lines, ~:d distinct (~a rules)."
            (length text) (length lines) distinct kind)))

;;; --- /degeneracy ---------------------------------------------------------------------

(defun status-text ()
  (format nil "degeneracy guard: mode ~a, ~:d incidents kept~%  \
a call is asked about when one argument is at least ~d chars in ~d non-blank lines, has no data shape, \
and repeats ~d identical lines or a block of at most ~d lines ~d times over ~d% of it~%  \
/check TEXT judges a text; /last shows the newest incident; /clear drops them"
          (mode-name) (length (incident-files))
          (th *argument-thresholds* :min-chars) (th *argument-thresholds* :min-lines)
          (th *argument-thresholds* :min-identical-run) (th *argument-thresholds* :max-period)
          (th *argument-thresholds* :min-reps)
          (round (* 100 (th *argument-thresholds* :min-share)))))

(defun last-incident-text ()
  "The newest incident, in one answer, or NIL when there is none."
  (let ((file (first (incident-files))))
    (when file
      (handler-case
          (let ((object (nlk:decode-json (uiop:read-file-string file))))
            (format nil "~a: ~a on ~a -- ~a"
                    (nlk:json-value object :string "ts")
                    (nlk:json-value object :string "reason")
                    (nlk:json-value object :string "tool")
                    (nlk:json-value object :string "detail")))
        (error () (format nil "~a (unreadable)" (pathname-name file)))))))

(defun clear-incidents ()
  (let ((files (incident-files)))
    (dolist (file files)
      (ignore-errors (delete-file file)))
    (format nil "~:d incident~:p dropped" (length files))))

(defun run-slash (value session-id)
  "The /degeneracy command: the state, a check, the newest incident, or a clear."
  (declare (ignore session-id))
  (let* ((text (or value ""))
         (words (nlk:split-words text))
         (head (string-downcase (or (first words) ""))))
    (cond ((string= head "") (status-text))
          ((string= head "check")
           (let ((subject (string-trim " " (subseq text (min (length text)
                                                              (length (first words)))))))
             (if (zerop (length subject))
                 "usage: /degeneracy check TEXT"
                 (let ((verdict (judge-argument subject)))
                   (if (getf verdict :ask)
                       (format nil "LOOP: ~a; a call carrying this would be asked about. ~a"
                               (getf verdict :detail) (metric-line subject "argument"))
                       (format nil "CLEAN: ~a. ~a" (getf verdict :detail)
                               (metric-line subject "argument")))))))
          ((string= head "last") (or (last-incident-text) "no incidents yet"))
          ((string= head "clear") (clear-incidents))
          (t (format nil "~a is not one of its verbs; /degeneracy alone shows the state" head)))))

;;; --- the entry -----------------------------------------------------------------------

(defun install ()
  "Start here: a clean round count and the incident folder in place. The
count is what the long-session notice reads, so a restart starts it over."
  (clrhash *rounds*)
  (ensure-directories-exist (incident-directory))
  (nle:on-stop (lambda () (clrhash *rounds*))))

(nle:define-cell degeneracy-guard
  (:section ("degeneracy-guard")
    (:guide "mode ask asks before a loop-shaped call runs, and refuses when nobody answers; refuse stops it outright; warn only records it. allow_tools exempts tools by name")
    ("mode" :choice :options '("ask" "refuse" "warn") :default "ask"
     :doc "ask: a loop-shaped call waits for Allow once or Deny; refuse: it never runs; warn: it runs and is only recorded")
    ("allow_tools" :list
     :doc "tool names never judged, as they are called")
    ("ask_seconds" :integer :default 120 :min 5
     :doc "how long the Allow once or Deny question waits")
    ("incidents" :integer :default 200 :min 0
     :doc "how many incident files to keep, newest first")
    ("max_rounds" :integer :default 500 :min 0
     :doc "model requests in one session before the long-session notice; 0 never says it"))
  (:start #'install)
  (:help :degeneracy-guard
         "asks about a tool call whose argument looks like a repetition loop; /degeneracy, and the degeneracy_check tool"
         +primer+)
  (:hook :tool #'inspect-call)
  (:hook :round (nlk:observer #'observe-round +owner+))
  (:tool "degeneracy_check" (nlk:contract 'degeneracy-check) 'check-tool)
  (:command "degeneracy" #'run-slash
            :description "Repeat-loop guard: its mode and thresholds, a check, the newest incident"
            :argument-hint "check TEXT | last | clear"))

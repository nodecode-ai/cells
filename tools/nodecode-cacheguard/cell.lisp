;;;; cell.lisp --- cacheguard: warn or block a big prompt after a pause.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A provider bills a cache hit far below its input price, and after a pause
;;;; the provider may have dropped the cache: the next big prompt is then
;;;; billed at full price, or more where a cache write costs extra. This cell
;;;; wraps the ONE function that makes a provider call (NLK: turn machinery's
;;;; CALL-PROVIDER) and keeps, per session, when the last call ran and how
;;;; large its prompt was. A call after a pause, on a prompt over the size
;;;; floor, is reported afterwards from the usage the round brought back
;;;; (`cache lost, 48k rebilled, 12 min'), and in BLOCK mode the provider is
;;;; not called at all: the round ends with the BLOCKED text as the answer,
;;;; so the operator can send again (which they will, on a fresh pause clock)
;;;; or start a new chat.
;;;;
;;;; Cache availability is unknown BEFORE the call, so a block says only how
;;;; often the cache was lost after similar pauses before, out of this
;;;; image's own journal. Fails open: any error in here lets the round run.
;;;;
;;;; Nothing leaves the machine: no network, no subprocess, no model call of
;;;; its own. The journal holds counts and minutes, never message text.

(defpackage #:nodecode-cacheguard
  (:use #:cl))

(in-package #:nodecode-cacheguard)

(nlk:define-peripheral cacheguard :not-running t)

;;; --- what the cell remembers -----------------------------------------------

(defparameter *history* (make-hash-table :test #'equal)
  "Session id -> (:last-at UNIVERSAL-TIME :prompt-tokens N). Live-only: a
restart forgets it, and the first call of a session is never guarded.")

(defparameter *journal* '()
  "Newest-first rows (:pause MINUTES :tokens N :cached N :lost BOOL) for
calls after a pause of a minute or more: counts, never text.")

(defparameter *settings* nil
  "The values the hooks run on, a plist copy of the section: (:MODE :PAUSE
:SIZE-K :DISPLAY-ON-SUCCESS :JOURNAL-ROWS). /cacheguard set changes it.")

(defparameter +pause-floor+ 1
  "Minutes under which a gap is not a pause at all; the journal's own floor.")

(defun option (key)
  (getf *settings* key))

(defun now ()
  (get-universal-time))

(defun take-settings ()
  "The declared values, read once at start into the plist the hooks use."
  (list :mode (setting :mode)
        :pause (setting :pause-minutes)
        :size-k (setting :size-k)
        :display-on-success (setting :display-on-success)
        :journal-rows (setting :journal-rows)))

;;; --- the decision ---------------------------------------------------------

(defun session-record (session)
  (gethash session *history*))

(defun (setf session-record) (value session)
  (if value
      (setf (gethash session *history*) value)
      (remhash session *history*)))

(defun session-pause (session &optional (at (now)))
  "Minutes since SESSION's last provider call, or NIL when it has none."
  (let ((last (getf (session-record session) :last-at)))
    (and last (max 0 (/ (- at last) 60.0d0)))))

(defun session-tokens (session)
  "The prompt size SESSION's last round filled, or 0 when unknown."
  (or (getf (session-record session) :prompt-tokens) 0))

(defun decision (session &optional (at (now)))
  "=> (values VERDICT PAUSE TOKENS): VERDICT is :BLOCK when the mode says
block and this is a big enough prompt after a long enough pause, else :SEND."
  (let* ((pause (session-pause session at))
         (tokens (session-tokens session))
         (block (and pause
                     (string= (or (option :mode) "warn") "block")
                     (>= tokens (* 1000 (or (option :size-k) 20)))
                     (>= pause (or (option :pause) 10)))))
    (values (if block :block :send) pause tokens)))

(defun mark-sent (session &optional (at (now)))
  "The pause clock restarts: this call is the one a later pause is read from."
  (let ((record (session-record session)))
    (setf (session-record session) (list :last-at at
                                         :prompt-tokens (getf record :prompt-tokens)))))

;;; --- tokens, minutes, and the lines ----------------------------------------

(defun token-text (n)
  "48000 as `48k', 1200000 as `1.2M'."
  (cond ((null n) "?")
        ((>= n 1000000) (format nil "~d.~dM" (floor n 1000000) (round (- (/ n 100000) (* 10 (floor n 1000000))))))
        ((>= n 1000) (format nil "~dk" (round n 1000)))
        (t (format nil "~d" n))))

(defun minute-text (minutes)
  (cond ((null minutes) "?")
        ((>= minutes 60) (format nil "~dh~d" (floor minutes 60) (round (- minutes (* 60 (floor minutes 60))))))
        (t (format nil "~dm" (round minutes)))))

(defun clock-text (&optional (at (now)))
  (multiple-value-bind (second minute hour) (decode-universal-time at)
    (format nil "~2,'0d:~2,'0d" hour minute)))

(defun journal-push (row)
  (push row *journal*)
  (let ((cap (or (option :journal-rows) 10000)))
    (when (> (length *journal*) cap)
      (setf *journal* (subseq *journal* (ceiling (- (length *journal*) cap))))))
  row)

(defun similar-pause-p (a b)
  "Whether two pauses fall in one column of the report's table."
  (string= (bucket-name a) (bucket-name b)))

(defun bucket-name (minutes)
  (cond ((null minutes) "?")
        ((< minutes 5) "1-5m")
        ((< minutes 15) "5-15m")
        ((< minutes 60) "15-60m")
        (t "1h+")))

(defun bucket-order (name)
  (position name '("1-5m" "5-15m" "15-60m" "1h+") :test #'string=))

(defun lost-similar (minutes)
  "=> (values LOST TOTAL) over journal rows in MINUTES' own column."
  (let ((rows (remove-if-not (lambda (row) (similar-pause-p (getf row :pause) minutes))
                              *journal*)))
    (values (count-if (lambda (row) (getf row :lost)) rows) (length rows))))

(defun rebilled-text (prompt cached)
  "The tokens the round may have been billed at full price."
  (token-text (max 0 (- (or prompt 0) (or cached 0)))))

(defun lost-line (pause prompt cached)
  "The result line for a round whose cache was lost."
  (multiple-value-bind (lost total) (lost-similar pause)
    (format nil "cacheguard ~a: cache lost, ~a rebilled, ~a pause~@[ · lost ~d of ~d similar pauses~]"
            (clock-text) (rebilled-text prompt cached) (minute-text pause)
            (and (>= total 10) lost) (and (>= total 10) total))))

(defun kept-line (pause prompt cached)
  (declare (ignore prompt))
  (format nil "cacheguard ~a: ~a reused, ~a pause"
          (clock-text) (token-text (or cached 0)) (minute-text pause)))

(defun block-line (pause tokens)
  (multiple-value-bind (lost total) (lost-similar pause)
    (format nil "cacheguard ~a: BLOCKED, ~a pause, ~a tokens may be rebilled~@[ · lost ~d of ~d similar pauses~]. Send it again to send it anyway, or start a new chat."
            (clock-text) (minute-text pause) (token-text tokens)
            (and (>= total 10) lost) (and (>= total 10) total))))

(defun blocked-message (text)
  "The round's answer when the provider was never called: the same shape a
provider returns, so the turn ends with this text in front of the operator."
  (nlk:json-object "role" "assistant" "content" text))

(defun say (text &key (level :info) key)
  (ignore-errors (nle:notice text :level level :key key)))

;;; --- the usage a round brought back ----------------------------------------

(defun usage-count (usage reader)
  (ignore-errors (funcall reader usage)))

(defun usage-input (usage)
  (usage-count usage #'nodecode.evolved::provider-usage-input-tokens))

(defun usage-cached (usage)
  (usage-count usage #'nodecode.evolved::provider-usage-cached-input-tokens))

(defun usage-written (usage)
  (usage-count usage #'nodecode.evolved::provider-usage-cache-write-tokens))

(defun note-usage (session usage pause)
  "Fold one round's usage: the prompt it filled, whether the cache read
anything, and one journal row when the pause was a real one."
  (when usage
    (let* ((input (usage-input usage))
           (cached (usage-cached usage))
           (written (usage-written usage)))
      (when (or input cached written)
        (let* ((prompt (+ (or input 0) (or cached 0) (or written 0)))
               (record (session-record session))
               (lost (and (plusp prompt) (zerop (or cached 0)))))
          (setf (session-record session)
                (list :last-at (or (getf record :last-at) (now)) :prompt-tokens prompt))
          (when (and pause (>= pause +pause-floor+))
            (journal-push (list :pause pause :tokens prompt :cached (or cached 0) :lost lost)))
          (values prompt (or cached 0) lost))))))

(defun report-usage (session usage pause)
  "Say what the round cost above the floor, and only when it is worth a line."
  (multiple-value-bind (prompt cached lost) (note-usage session usage pause)
    (when (and prompt pause (>= pause +pause-floor+)
               (>= prompt (* 1000 (or (option :size-k) 20))))
      (cond (lost
             (say (lost-line pause prompt cached)
                  :level (if (string= (or (option :mode) "warn") "warn") :warning :info)
                  :key "nodecode-cacheguard"))
            ((option :display-on-success)
             (say (kept-line pause prompt cached) :key "nodecode-cacheguard"))))
    (values prompt cached lost)))

;;; --- the one seam: around a provider call ----------------------------------

(defun turn-session ()
  "The session this round belongs to, or \"local\" outside a turn."
  (or (getf (nle:turn) :session-id) "local"))

(defun guard-call (next &rest args)
  "Around CALL-PROVIDER: decide before the call, report after it. In block
mode the provider is not called and the round answers the block text."
  (handler-case
      (let* ((session (turn-session))
             (pause (session-pause session)))
        (multiple-value-bind (verdict pause tokens) (decision session)
          (if (eq verdict :block)
              (let ((text (block-line pause tokens)))
                ;; The pause clock restarts, so sending it again goes out.
                (mark-sent session)
                (say text :level :warning :key "nodecode-cacheguard")
                (values (blocked-message text) nil nil nil nil))
              (progn
                (mark-sent session)
                (multiple-value-bind (message usage finish metadata active)
                    (apply next args)
                  (report-usage session usage pause)
                  (values message usage finish metadata active))))))
    (error (condition)
      ;; Fails open: a bug here never costs the operator a round.
      (declare (ignore condition))
      (mark-sent (turn-session))
      (apply next args))))

(defun reset-history ()
  (clrhash *history*)
  (setf *journal* '()))

;;; --- /cacheguard ------------------------------------------------------------

(defparameter +setting-names+ '("pause_minutes" "size_k" "mode" "display_on_success" "journal_rows")
  "The settings /cacheguard set writes, by their config names.")

(defun setting-key (name)
  (cond ((string= name "pause_minutes") :pause)
        ((string= name "size_k") :size-k)
        ((string= name "mode") :mode)
        ((string= name "display_on_success") :display-on-success)
        ((string= name "journal_rows") :journal-rows)
        (t nil)))

(defun parse-value (name text)
  "=> (values VALUE PROBLEM) for one setting's typed value."
  (cond ((member name '("pause_minutes" "size_k" "journal_rows") :test #'string=)
         (let ((n (ignore-errors (parse-integer (or text "") :junk-allowed t))))
           (if (and n (plusp n)) (values n nil) (values nil (format nil "~a wants a whole number, got ~s" name text)))))
        ((string= name "mode")
         (if (member text '("off" "warn" "block") :test #'string=)
             (values text nil)
             (values nil (format nil "mode is off, warn or block, got ~s" text))))
        ((string= name "display_on_success")
         (cond ((member text '("true" "yes" "on" "t" "1") :test #'string=) (values t nil))
               ((member text '("false" "no" "off" "nil" "0") :test #'string=) (values nil nil))
               (t (values nil (format nil "display_on_success is true or false, got ~s" text)))))
        (t (values nil (format nil "~a is not a cacheguard setting" name)))))

(defun save-setting (name value)
  "Keep it in the shared config; a config we cannot write keeps it for this run only."
  (handler-case
      (progn (nle:config-set (list "cacheguard" name) value) t)
    (error () nil)))

(defun table-text ()
  "How often the cache was lost, by how long the pause was."
  (if (null *journal*)
      "(no observations yet)"
      (let ((buckets (sort (remove-duplicates (mapcar (lambda (row) (bucket-name (getf row :pause))) *journal*)
                                               :test #'string=)
                            #'< :key #'bucket-order)))
        (format nil "~{  ~a: ~d calls, ~d lost~^~%~}"
                (loop for bucket in buckets
                      for rows = (remove-if-not (lambda (row) (string= bucket (bucket-name (getf row :pause))))
                                                *journal*)
                      append (list bucket (length rows)
                                   (count-if (lambda (row) (getf row :lost)) rows)))))))

(defun settings-text ()
  (format nil "mode ~a · pause ~dm · size ~dk tokens · display_on_success ~a · journal_rows ~d"
          (option :mode) (option :pause) (option :size-k)
          (if (option :display-on-success) "on" "off") (option :journal-rows)))

(defun help-text ()
  (format nil "/cacheguard - settings and how often the cache was lost, by pause~%  /cacheguard set <name> <value> - ~{~a~^, ~}~%  /cacheguard reset - forget what it learned about pauses~%  /cacheguard help - this"
          +setting-names+))

(defun set-text (name text)
  (multiple-value-bind (value problem) (parse-value name text)
    (if problem
        (format nil "cacheguard: ~a" problem)
        (let* ((key (setting-key name))
               (old (getf *settings* key)))
          (setf (getf *settings* key) value)
          (format nil "cacheguard: ~a ~a -> ~a~@[ (this run only)~]"
                  name old value (not (save-setting name value)))))))

(defun run-slash (args session-id)
  "One /cacheguard invocation: the report, a changed setting, or a reset."
  (declare (ignore session-id))
  (let* ((words (remove "" (uiop:split-string (or args "") :separator '(#\Space #\Tab))))
         (head (string-downcase (or (first words) ""))))
    (cond ((string= head "") (format nil "cacheguard: ~a~%~a" (settings-text) (table-text)))
          ((string= head "help") (help-text))
          ((string= head "reset") (reset-history) "cacheguard: history cleared")
          ((string= head "set")
           (let ((name (string-downcase (or (second words) "")))
                 (value (third words)))
             (if (string= name "")
                 "cacheguard: set what? (try /cacheguard help)"
                 (set-text name value))))
          (t (format nil "cacheguard: unknown argument ~s (try /cacheguard help)" (first words))))))

;;; --- the declaration --------------------------------------------------------

(nle:define-cell cacheguard
  (:section ("cacheguard")
    (:guide "nothing here is required; warn reports after the call, block stops before it; defaults: warn, 10 minutes, 20k tokens")
    ("mode" :choice :options '("off" "warn" "block") :default "warn"
     :doc "off, warn (report afterwards) or block (stop before sending)")
    ("pause_minutes" :integer :default 10 :min 1
     :doc "idle minutes that count as a pause")
    ("size_k" :integer :default 20 :min 0
     :doc "smallest prompt to act on, in thousands of tokens")
    ("display_on_success" :boolean :default nil
     :doc "show the result line when the cache was kept")
    ("journal_rows" :integer :default 10000 :min 1
     :doc "most calls kept in the stats"))
  (:command "cacheguard" 'run-slash
            :description "Cacheguard: how often the cache was lost after a pause, and its settings"
            :argument-hint "[set <name> <value> | reset | help]")
  (:help :cacheguard
   "cacheguard: warns or blocks a big prompt after a pause - /cacheguard [set <name> <value> | reset | help]"
   "After a pause a provider may have dropped its prompt cache, so the next big prompt is billed at full input price. cacheguard keeps, per session, when the last provider call ran and how large its prompt was, reports afterwards what the round reused or rebilled, and in block mode stops before the provider is called: the round answers the BLOCKED text, and sending again goes out on a fresh pause clock. Settings: mode (off, warn, block), pause_minutes, size_k, display_on_success, journal_rows. Everything stays on this machine; the journal holds counts and minutes, never text.")
  (:hook 'nodecode.evolved::call-provider #'guard-call)
  (:start (lambda ()
            (setf *settings* (take-settings))
            (reset-history)
            (setf *cacheguard* *settings*))))

;;;; cell.lisp --- what a session spent, and the gate that stops it overrunning.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One row a provider round in a local ledger, one row a tool call, and a
;;;; daily/monthly ceiling read before every call: that is the whole cell.
;;;;
;;;; The seams, and why these:
;;;;   :frame  every durable fact passes through the :FRAME point, and
;;;;           turn.usage carries the round's provider, model, the eight token
;;;;           counts and the cost the core already priced. That is the row:
;;;;           no price list is fetched and nothing is estimated here.
;;;;           turn.provider_request carries the round's timings, kept for the
;;;;           usage fact that follows it in the same turn.
;;;;   :tool   the around shape of one call, so a call's name, wall time and
;;;;           failure are recorded -- and the one place a call can be
;;;;           refused before it runs, which is how a cap is enforced.
;;;;   :round  where a warning rides into the next request, in observe mode.
;;;;
;;;; Nothing here fails a turn: the frame hook records inside HANDLER-CASE,
;;;; the tool hook records in an unwind-protect, and an unreadable ledger is
;;;; the cell's problem, said as one standing notice.

(in-package #:nodecode-telemetry)

;;; --- what the cell keeps --------------------------------------------------------

(defparameter +ledger-file+ "telemetry/ledger.jsonl"
  "The ledger under the home: one JSON row a line, appended, never rewritten.")
(defparameter +rows-kept+ 50000
  "How many rows this start holds in memory; the file keeps every one.")
(defparameter +reset-word+ "00:00 UTC"
  "When a day's window opens again.")
(defparameter +windows+ '("today" "week" "month" "sessions" "tools" "models" "raw")
  "What /stats reads, beside the tool's own choice of the first six.")
(defparameter +modes+ '("observe" "enforce")
  "What mode reads: observe records and warns, enforce refuses a call past a cap.")
(defparameter +scopes+ '("daily" "monthly" "session")
  "What /budget set may name.")

(defvar *rows* nil
  "The rows this start has read or written, newest first.")
(defvar *row-count* 0)
(defvar *day-total* nil
  "Day key -> the money spent that day.")
(defvar *day-session* nil
  "(\"DAY\" . \"SESSION\") -> the money that session spent that day.")
(defvar *month-total* nil
  "Month key -> the money spent that month.")
(defvar *pending* nil
  "(\"SESSION\" . \"TURN\") -> (TTFT-MS . STREAM-MS) from turn.provider_request,
waiting for the usage fact that follows in the same turn.")
(defvar *notified* :none
  "The breach text the standing notice last spoke, so one breach says it once.")
(defvar *warned* nil
  "The turns whose round already carried the over-budget warning.")
(defvar *last-error* nil
  "Why the last row could not be recorded, or NIL.")

;;; --- settings, read leniently ----------------------------------------------------

(defun option (key fallback)
  "KEY's setting, FALLBACK when the cell is stopped."
  (let ((value (ignore-errors (setting key))))
    (if (null value) fallback value)))

(defun mode () (option :mode "observe"))
(defun enforcing-p () (equal (mode) "enforce"))
(defun daily-cap () (option :daily-usd 5d0))
(defun monthly-cap () (option :monthly-usd 100d0))
(defun session-cap () (option :session-usd 0d0))

(defun parse-usd (raw key)
  "RAW, a written amount of US dollars, as a double; KEY names it in the refusal."
  (let* ((text (string-trim " $" (or raw "")))
         (value (handler-case (multiple-value-bind (number end) (read-from-string text nil nil)
                                  (and (realp number) (= end (length text)) number))
                  (error () nil))))
    (unless (and value (not (minusp value)))
      (nlk:config-error "telemetry.~a is ~s; it is a number of US dollars, 0 or more"
                        key (if (stringp raw) raw "")))
    (coerce value 'double-float)))

;;; --- money, counts, keys ------------------------------------------------------------

(defun money (value)
  "VALUE as dollars, rounded for reading."
  (let ((number (coerce (or value 0) 'double-float)))
    (cond ((zerop number) "$0.00")
          ((< (abs number) 0.01d0) (format nil "$~,4f" number))
          (t (format nil "$~,2f" number)))))

(defun day-key (at) (floor at 86400))

(defun month-key (at)
  "AT's month as YYYY-MM, the way the ledger groups one."
  (subseq (nlk:iso-time at) 0 7))

(defun window-since (window &aux (now (nlk:unix-now)) (today (* 86400 (floor now 86400))))
  "The oldest second WINDOW reads, or NIL for one the rows are filtered by month."
  (cond ((equal window "week") (- today (* 6 86400)))
        ((equal window "month") nil)
        (t today)))

(defun window-month (window)
  (and (equal window "month") (month-key (nlk:unix-now))))

(defun row-kind (row) (nlk:json-value row :string "kind"))
(defun row-at (row) (or (nlk:json-value row :integer "at") 0))
(defun row-session (row) (or (nlk:json-value row :string "session") ""))

(defun row-cost (row)
  "ROW's cost as a double, 0 when the core could not price the round."
  (let ((value (nlk:json-value row :number "cost")))
    (if (numberp value) (coerce value 'double-float) 0d0)))

(defun row-priced-p (row)
  (let ((known (nlk:json-value row :boolean "cost-known")))
    (and known t)))

;;; --- the ledger ----------------------------------------------------------------------

(defun ledger-path () (nlk:home +ledger-file+))

(defun rows ()
  "Every row this start knows, oldest first."
  (reverse *rows*))

(defun read-ledger ()
  "The rows on disk, oldest first: what a start folds in."
  (nlk:read-json-lines (ledger-path)))

(defun index-row (row)
  "Fold ROW into the day, session and month totals the gate reads."
  (when (equal "round" (row-kind row))
    (let ((cost (row-cost row))
          (at (row-at row))
          (session (row-session row)))
      (when (plusp cost)
        (incf (gethash (day-key at) *day-total* 0d0) cost)
        (incf (gethash (month-key at) *month-total* 0d0) cost)
        (incf (gethash (cons (day-key at) session) *day-session* 0d0) cost))))
  row)

(defun record! (row)
  "Append ROW to the ledger and fold it in. The caller holds the lock."
  (nlk:append-json-line (ledger-path) row)
  (push row *rows*)
  (incf *row-count*)
  (when (> *row-count* +rows-kept+)
    (setf *rows* (nbutlast *rows* (ceiling +rows-kept+ 10))
          *row-count* (length *rows*)))
  (index-row row))

(defun rebuild (rows)
  "Start this start's state from ROWS: the totals, and the newest kept."
  (setf *day-total* (make-hash-table :test #'eql)
        *day-session* (make-hash-table :test #'equal)
        *month-total* (make-hash-table :test #'equal)
        *pending* (make-hash-table :test #'equal)
        *warned* (make-hash-table :test #'equal)
        *rows* nil
        *row-count* 0)
  (dolist (row rows) (index-row row))
  (let ((kept (min (length rows) +rows-kept+)))
    (setf *rows* (reverse (if (plusp kept) (last rows kept) '()))
          *row-count* kept)))

(defun reset-ledger ()
  "Forget every row, in memory and on disk: what a test starts from."
  (ignore-errors (delete-file (ledger-path)))
  (rebuild nil)
  nil)

;;; --- folding rows -----------------------------------------------------------------------

(defstruct (tally (:constructor make-tally))
  "One window of spend, rows folded in."
  (rounds 0) (spend 0d0) (unpriced 0)
  (in 0) (out 0) (cache 0) (reasoning 0)
  (calls 0) (failed 0) (seconds 0d0) (blocked 0))

(defun row-in-window-p (row &key since month session)
  (and (or (null session) (equal session (row-session row)))
       (or (null since) (>= (row-at row) since))
       (or (null month) (equal month (month-key (row-at row))))))

(defun fold-row (tally row)
  (ecase (intern (string-upcase (or (row-kind row) "")) :keyword)
    (:round
     (incf (tally-rounds tally))
     (if (row-priced-p row)
         (incf (tally-spend tally) (row-cost row))
         (incf (tally-unpriced tally)))
     (incf (tally-in tally) (or (nlk:json-value row :integer "in") 0))
     (incf (tally-out tally) (or (nlk:json-value row :integer "out") 0))
     (incf (tally-cache tally) (or (nlk:json-value row :integer "cache-read") 0))
     (incf (tally-reasoning tally) (or (nlk:json-value row :integer "reasoning") 0)))
    (:tool
     (incf (tally-calls tally))
     (unless (let ((ok (nlk:json-value row :boolean "ok"))) (or (null ok) ok))
       (incf (tally-failed tally)))
     (incf (tally-seconds tally) (let ((s (nlk:json-value row :number "seconds")))
                                    (if (numberp s) (coerce s 'double-float) 0d0))))
    (:blocked (incf (tally-blocked tally))))
  tally)

(defun tally-of (rows &key since month session)
  "ROWS folded into one TALLY over the window."
  (let ((tally (make-tally)))
    (dolist (row rows tally)
      (when (row-in-window-p row :since since :month month :session session)
        (fold-row tally row)))))

(defun group-rows (rows key &key since month)
  "ROWS grouped by KEY (:session, :model, :tool), heaviest spender first, as
(TALLY . NAME)."
  (let ((groups (make-hash-table :test #'equal)))
    (dolist (row rows)
      (when (row-in-window-p row :since since :month month)
        (let* ((name (ecase key
                       (:session (row-session row))
                       (:model (or (nlk:json-value row :string "model") "unknown"))
                       (:tool (or (nlk:json-value row :string "tool") "unknown"))))
               (tally (gethash name groups)))
          (unless tally (setf tally (setf (gethash name groups) (make-tally))))
          (fold-row tally row))))
    (sort (loop for name being the hash-keys of groups using (hash-value tally)
                collect (cons tally name))
          #'> :key (lambda (entry) (+ (* 1000 (tally-spend (car entry))) (tally-calls (car entry)))))))

;;; --- one line a window --------------------------------------------------------------------

(defun tally-line (tally window &key session)
  "WINDOW's one line: the money, the tokens, the calls, the time."
  (format nil "~@[~a ~]~a · ~a over ~d round~:p~@[ (~d unpriced)~] · in ~:d · out ~:d · cache ~:d~@[ · reasoning ~:d~] · ~d tool call~:p~@[ (~d failed)~]~@[ · ~,1fs of tool time~]~@[ · ~d blocked~]"
          (and session (format nil "~a·" session))
          window
          (money (tally-spend tally))
          (tally-rounds tally)
          (and (plusp (tally-unpriced tally)) (tally-unpriced tally))
          (tally-in tally) (tally-out tally) (tally-cache tally)
          (and (plusp (tally-reasoning tally)) (tally-reasoning tally))
          (tally-calls tally)
          (and (plusp (tally-failed tally)) (tally-failed tally))
          (and (plusp (tally-seconds tally)) (tally-seconds tally))
          (and (plusp (tally-blocked tally)) (tally-blocked tally))))

(defun breakdown-line (label entries &optional (limit 6))
  "LABEL and the LIMIT heaviest ENTRIES, each a (TALLY . NAME)."
  (let ((parts (loop for (tally . name) in (subseq entries 0 (min limit (length entries)))
                     when (or (plusp (tally-spend tally)) (plusp (tally-calls tally)))
                       collect (format nil "~a ~a (~d round~:p, ~d call~:p)"
                                       name (money (tally-spend tally))
                                       (tally-rounds tally) (tally-calls tally)))))
    (if parts
        (format nil "~a: ~{~a~^ · ~}" label parts)
        (format nil "~a: nothing recorded yet" label))))

(defun raw-lines (&optional (limit 5))
  (format nil "~{~a~^~%~}"
          (loop for row in (subseq *rows* 0 (min limit (length *rows*)))
                collect (nlk:encode-json-object row))))

(defun stats-text (window &optional session)
  "One /stats answer: WINDOW's line, or a breakdown of it."
  (let* ((rows (rows))
         (since (window-since window))
         (month (window-month window)))
    (cond
      ((equal window "sessions")
       (breakdown-line "sessions" (group-rows rows :session :since since :month month)))
      ((equal window "tools")
       (breakdown-line "tools" (group-rows rows :tool :since since :month month)))
      ((equal window "models")
       (breakdown-line "models" (group-rows rows :model :since since :month month)))
      ((equal window "raw")
       (if *rows* (raw-lines) "telemetry: the ledger is empty"))
      (t
       (tally-line (tally-of rows :since since :month month :session session)
                   window :session session)))))

;;; --- the gate ------------------------------------------------------------------------------

(defun spent-today (&optional session)
  (if session
      (gethash (cons (day-key (nlk:unix-now)) session) *day-session* 0d0)
      (gethash (day-key (nlk:unix-now)) *day-total* 0d0)))

(defun spent-this-month ()
  (gethash (month-key (nlk:unix-now)) *month-total* 0d0))

(defun breach (&optional session)
  "The first ceiling SESSION's spend is past, as a plist, or NIL."
  (or (let ((cap (session-cap)) (spent (and session (spent-today session))))
        (when (and (plusp cap) spent (>= spent cap))
          (list :scope "session" :cap cap :spent spent :session session)))
      (let ((cap (daily-cap)) (spent (spent-today)))
        (when (and (plusp cap) (>= spent cap))
          (list :scope "daily" :cap cap :spent spent)))
      (let ((cap (monthly-cap)) (spent (spent-this-month)))
        (when (and (plusp cap) (>= spent cap))
          (list :scope "monthly" :cap cap :spent spent)))))

(defun breach-text (b &key refusal)
  "One sentence for a breach B, as a notice or as the refusal a call answers."
  (let ((spent (format nil "~a~@[ for ~a~] ~a"
                        (if (equal (getf b :scope) "monthly") "this month" "today")
                        (getf b :session)
                        (money (getf b :spent)))))
    (if refusal
        (format nil "the ~a cap of ~a is spent (~a)" (getf b :scope) (money (getf b :cap)) spent)
        (format nil "the ~a cap of ~a is spent (~a); it resets at ~a"
                (getf b :scope) (money (getf b :cap)) spent +reset-word+))))

(defun announce (b)
  "Say B once where it stands, and take the notice down when it clears."
  (let ((text (and b (format nil "telemetry: ~a" (breach-text b)))))
    (unless (equal text *notified*)
      (setf *notified* text)
      (handler-case (if text
                        (nle:notice text :level :warning :key "nodecode-telemetry")
                        (nle:notice nil :key "nodecode-telemetry"))
        (error (condition) (setf *last-error* (princ-to-string condition)))))))

;;; --- recording ------------------------------------------------------------------------------

(defun json-integer (object key)
  (let ((value (nlk:json-value object :integer key)))
    (and value value)))

(defun round-row (payload metadata &key session turn timings)
  "The ledger's row for one provider round: what the core priced, and how
long the provider took."
  (nlk:json-object
   "kind" "round"
   "at" (nlk:unix-now)
   "session" (or session "")
   "turn" (or turn "")
   "provider" (or (nlk:json-value payload :string "provider") "")
   "model" (or (nlk:json-value payload :string "model") "")
   "in" (or (json-integer payload "input-tokens") 0)
   "out" (or (json-integer payload "output-tokens") 0)
   "cache-read" (or (json-integer payload "cached-input-tokens") 0)
   "cache-write" (or (json-integer payload "cache-write-tokens") 0)
   "reasoning" (or (json-integer payload "reasoning-tokens") 0)
   "total" (or (json-integer payload "total-tokens") 0)
   "cost" (let ((cost (nlk:json-value payload :number "cost-usd")))
            (if (numberp cost) (coerce cost 'double-float) 0d0))
   "cost-known" (let ((known (nlk:json-value payload :boolean "cost-known")))
                  (and known t))
   "estimated" (let ((estimated (nlk:json-value payload :boolean "estimated")))
                 (and estimated t))
   "voided" (let ((voided (nlk:json-value payload :boolean "voided")))
              (and voided t))
   "ttft-ms" (car timings)
   "seconds" (let ((ms (cdr timings)))
               (and ms (/ (coerce ms 'double-float) 1000)))
   "round" (json-integer payload "provider-round")
   "turn-id" (and metadata (nlk:json-value metadata :string "turn_id"))))

(defun record-frame (op)
  "File one durable fact OP carries, and answer the row it made, or NIL."
  (multiple-value-bind (kind payload metadata turn-id) (nlk:frame-fact op)
    (when kind
      (let ((session (getf op :session-id))
            (turn (or turn-id (and metadata (nlk:json-value metadata :string "turn_id")))))
        (cond
          ((equal kind "turn.provider_request")
           (let ((key (cons (or session "") (or turn ""))))
             (setf (gethash key *pending*)
                   (cons (json-integer payload "ttft-ms")
                         (json-integer payload "stream-duration-ms"))))
           nil)
          ((equal kind "turn.usage")
           (let* ((key (cons (or session "") (or turn "")))
                  (timings (gethash key *pending*))
                  (row (round-row payload metadata :session session :turn turn :timings timings)))
             (remhash key *pending*)
             (with-telemetry-lock (record! row))
             (announce (breach session))
             row))
          (t nil))))))

(defun observe-frame (op next)
  "The :FRAME watcher: NEXT publishes the frame, then the fact is filed. A
recording error is the cell's, never the frame's."
  (prog1 (funcall next op)
    (handler-case (record-frame op)
      (error (condition) (setf *last-error* (princ-to-string condition))))))

;;; --- tool calls, and the refusal a cap makes ------------------------------------------------

(defun current-session ()
  "The session the running turn belongs to, or NIL outside a turn."
  (let ((turn (ignore-errors (nle:turn))))
    (and turn (getf turn :session-id))))

(defun current-turn ()
  (let ((turn (ignore-errors (nle:turn))))
    (and turn (getf turn :turn-id))))

(defun tool-failed-p (answer signals)
  (or signals
      (and (stringp answer) (uiop:string-prefix-p "ERROR:" answer))
      nil))

(defun record-call (session name seconds ok)
  (with-telemetry-lock
    (record! (nlk:json-object "kind" "tool" "at" (nlk:unix-now)
                               "session" (or session "") "tool" (or name "")
                               "seconds" (coerce seconds 'double-float)
                               "ok" (and ok t)))))

(defun refuse (b session tool)
  "The refusal a blocked call answers, journaled as its own row."
  (with-telemetry-lock
    (record! (nlk:json-object "kind" "blocked" "at" (nlk:unix-now)
                               "session" (or session "") "tool" (or tool "")
                               "scope" (getf b :scope)
                               "cap" (getf b :cap) "spent" (getf b :spent))))
  (format nil "budget: every tool call is refused while ~a — /budget shows the windows, ~
               /budget set ~a N raises the cap, and mode observe records without enforcing."
          (breach-text b :refusal t) (getf b :scope)))

(defun observe-tool (op next)
  "The :TOOL around hook: refuse a call past a cap, else run it and journal
its name, wall time and failure. Answers exactly what NEXT answered."
  (let* ((session (current-session))
         (name (getf op :name))
         (blocked (and (enforcing-p) (breach session))))
    (if blocked
        (handler-case (refuse blocked session name)
          (error (condition) (setf *last-error* (princ-to-string condition))
                 "budget: tools are blocked on this machine until the cap resets"))
        (let ((started (get-internal-real-time)) (answer nil) (signals nil))
          (unwind-protect
               (handler-case (setf answer (funcall next op))
                 (error (condition) (setf signals t) (error condition)))
            (handler-case
                (record-call session name
                             (/ (float (- (get-internal-real-time) started))
                                internal-time-units-per-second)
                             (not (tool-failed-p answer signals)))
              (error (condition) (setf *last-error* (princ-to-string condition)))))
          answer))))

;;; --- the round warning ------------------------------------------------------------------------

(defun over-budget-warning (b)
  (format nil "budget: ~a. Keep the work small, or say so before spending more."
          (breach-text b)))

(defun observe-round (op next)
  "The :ROUND hook: in observe mode only, one line in the round about to be
asked when a cap is spent, once a turn."
  (prog1 (funcall next op)
    (when (and (not (enforcing-p))
               (ignore-errors (breach)))
      (let ((turn (getf op :turn))
            (turn-id (or (current-turn) (format nil "~a" (getf op :turn)))))
        (when (and turn (not (gethash turn-id *warned*)))
          (setf (gethash turn-id *warned*) t)
          (handler-case
              (nlk:record-turn-context-prompt turn :text (over-budget-warning (breach)))
            (error (condition) (setf *last-error* (princ-to-string condition)))))))))

;;; --- /stats ---------------------------------------------------------------------------------------

(defun stats-panel (line)
  (nle:list-dialog
   :title "Telemetry" :summary line
   :rows (list (nle:run-row "Today" "/stats today")
               (nle:run-row "The last seven days" "/stats week")
               (nle:run-row "This month" "/stats month")
               (nle:run-row "Sessions today" "/stats sessions" :detail "the heaviest first")
               (nle:run-row "Tools today" "/stats tools")
               (nle:run-row "Models today" "/stats models")
               (nle:run-row "Budgets and their caps" "/budget"))))

(defun run-stats (args session-id)
  "One /stats invocation."
  (declare (ignore session-id))
  (let ((window (or (first (nlk:split-words args)) "today")))
    (nlk:with-handlers ((error (condition)
                          (format nil "telemetry: ~a" (princ-to-string condition))))
      (unless (member window +windows+ :test #'string=)
        (return-from run-stats
          (values (format nil "telemetry: ~a is not one of ~{~a~^ | ~}" window +windows+)
                  (stats-panel (stats-text "today")))))
      (let ((line (stats-text window)))
        (values line (stats-panel line))))))

;;; --- /budget -----------------------------------------------------------------------------------------

(defun budget-line ()
  (let* ((session (current-session))
         (today (spent-today))
         (month (spent-this-month))
         (b (breach session)))
    (format nil "budget (~a): today ~a of ~a · this month ~a of ~a~@[ · one session ~a~]~@[ · BLOCKED on ~a~] · days reset ~a"
            (mode) (money today) (money (daily-cap))
            (money month) (money (monthly-cap))
            (and (plusp (session-cap)) (money (session-cap)))
            (and b (getf b :scope))
            +reset-word+)))

(defun budget-panel (line)
  (nle:list-dialog
   :title "Budget" :summary line
   :rows (list (nle:ask-row "Set the daily cap" "US dollars, for example 10" "/budget set daily "
                             :placeholder "10" :verb "set")
               (nle:ask-row "Set the monthly cap" "US dollars" "/budget set monthly " :verb "set")
               (nle:ask-row "Set one session's daily cap" "US dollars, 0 for none"
                             "/budget set session " :verb "set")
               (nle:run-row "Enforce the caps" "/budget mode enforce"
                             :detail "a call past a cap is refused")
               (nle:run-row "Only record" "/budget mode observe"
                             :detail "the model is warned, every call runs")
               (nle:run-row "What was spent" "/stats today"))))

(defun config-member (scope)
  (cond ((equal scope "daily") "daily_usd")
        ((equal scope "monthly") "monthly_usd")
        ((equal scope "session") "session_usd")
        (t (fail "~a is not one of ~{~a~^ | ~}: what a cap is set on" scope +scopes+))))

(defun setting-key (scope)
  (cond ((equal scope "daily") :daily-usd)
        ((equal scope "monthly") :monthly-usd)
        ((equal scope "session") :session-usd)
        (t (fail "~a is not one of ~{~a~^ | ~}" scope +scopes+))))

(defun set-cap (scope written)
  "Write the cap SCOPE names into the shared config, and run on the value now."
  (let* ((member (config-member scope))
         (value (parse-usd written scope))
         (unwritten nil))
    (handler-case (nle:config-set (list "telemetry" member) (format nil "~a" written))
      (error (condition) (setf unwritten (princ-to-string condition))))
    (setf (getf *telemetry* (setting-key scope)) value)
    (announce (breach (current-session)))
    (format nil "budget: the ~a cap is now ~a~@[ (the config was not written: ~a)~]"
            scope (money value) unwritten)))

(defun set-mode (word)
  (unless (member word '("observe" "enforce") :test #'string=)
    (fail "~@[~a ~]is not a mode: observe or enforce" word))
  (let ((unwritten nil))
    (handler-case (nle:config-set '("telemetry" "mode") word)
      (error (condition) (setf unwritten (princ-to-string condition))))
    (setf (getf *telemetry* :mode) word)
    (announce (breach (current-session)))
    (format nil "telemetry: mode ~a — ~a~@[ (the config was not written: ~a)~]"
            word (if (equal word "enforce")
                     "a tool call past a cap is refused"
                     "the model is warned and every call runs")
            unwritten)))

(defun run-budget (args session-id)
  "One /budget invocation: the windows and their caps, or one set, or a mode."
  (declare (ignore session-id))
  (let* ((words (nlk:split-words args))
         (head (first words)))
    (nlk:with-handlers ((error (condition)
                          (format nil "budget: ~a" (princ-to-string condition))))
      (cond
        ((null head) (let ((line (budget-line))) (values line (budget-panel line))))
        ((string-equal head "set")
         (let* ((scope (or (second words) "daily"))
                (written (format nil "~{~a~^ ~}" (cddr words))))
           (when (zerop (length written))
             (return-from run-budget
               (values (format nil "budget set ~a needs an amount, for example /budget set ~a 10"
                               scope scope)
                       (budget-panel (budget-line)))))
           (let ((line (set-cap scope written)))
             (values line (budget-panel (budget-line))))))
        ((string-equal head "mode")
         (let ((line (set-mode (or (second words) ""))))
           (values line (budget-panel (budget-line)))))
        (t (values (format nil "budget: ~a is not one of set | mode" head)
                   (budget-panel (budget-line))))))))

;;; --- the tool and the manual ----------------------------------------------------------------------

(nlk:define-contract telemetry-report
  (:doc "What this machine spent on models over a window: rounds, tokens,
cost, tool calls and time, and how much of the budget is left. Ask before a
long job, or to answer what the last hour cost.")
  ("window" :string :default "today" :options +windows+
            :doc "today, week, month, sessions, tools or models")
  ("session" :string :default ""
            :doc "one session id to narrow to, or empty for every session"))

(defun report-spend (report)
  "The telemetry tool's answer: one window's line, and the budget's own."
  (let* ((window (telemetry-report-window report))
         (session (let ((id (telemetry-report-session report)))
                    (and id (plusp (length id)) id))))
    (format nil "~a~%~a" (stats-text window session) (budget-line))))

(defparameter +manual+
  "telemetry keeps a local ledger of what every provider round cost: the
tokens the provider reported, the cost the core priced, the model, the round's
latency, and one row per tool call with its wall time and whether it failed.
/stats [today|week|month|sessions|tools|models|raw] reads it, /budget shows
the daily and monthly caps and sets them (/budget set daily 10, /budget mode
enforce). In enforce mode a tool call past a cap is refused until the window
resets at 00:00 UTC; in observe mode the model is warned and every call runs.
Nothing leaves this machine: the ledger is a JSON line file under the home.")

(nle:define-cell telemetry
  (:section ("telemetry")
    ("mode" :choice :options +modes+ :default "observe"
            :doc "observe records and warns; enforce refuses a call past a cap")
    ("daily_usd" :string :default "5"
                 :doc "the daily cap in US dollars; 0 turns it off")
    ("monthly_usd" :string :default "100"
                   :doc "the monthly cap in US dollars; 0 turns it off")
    ("session_usd" :string :default "0"
                   :doc "one session's daily cap in US dollars; 0 turns it off"))
  ;; The caps are written as text so a fraction is a fraction ($0.50 of a
  ;; test run) rather than a whole-dollar rounding of one; the parse is here,
  ;; where a wrong value is the loader's refusal naming the member.
  (:settings (lambda (values table)
               (declare (ignore table))
               (dolist (pair '(("daily_usd" . :daily-usd)
                               ("monthly_usd" . :monthly-usd)
                               ("session_usd" . :session-usd)))
                 (setf (getf values (cdr pair))
                       (parse-usd (getf values (cdr pair)) (car pair))))
               values))
  (:hook :frame #'observe-frame)
  (:hook :tool #'observe-tool)
  (:hook :round #'observe-round)
  (:command "stats" 'run-stats
            :description "What was spent: tokens, cost, latency and tool calls"
            :argument-hint "today | week | month | sessions | tools | models | raw")
  (:command "budget" 'run-budget
            :description "The daily and monthly caps, and how much of them is spent"
            :argument-hint "set daily|monthly|session USD | mode observe|enforce")
  (:tool "telemetry" (nlk:contract 'telemetry-report) 'report-spend)
  (:help :telemetry "telemetry: what every round cost, and the caps that stop a run overrunning" +manual+)
  (:start (lambda ()
            (with-telemetry-lock
              (rebuild (or (ignore-errors (read-ledger)) '()))
              (setf *notified* :none
                    *last-error* nil))
            (announce (breach))
            (nle:on-stop (lambda ()
                           (with-telemetry-lock
                             (rebuild nil)
                             (setf *notified* :none)))))))

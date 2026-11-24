;;;; cell.lisp --- a brake pedal for model spend: meter, warn, brake.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What it does. Dashboards show what an agent already spent; this cell
;;;; enforces a budget while there is still time for it to matter. Every
;;;; provider round of every turn leaves a durable turn.usage fact in the
;;;; store -- the counts the provider reported, priced with the catalog's own
;;;; price when the round was recorded. This cell reads those facts, folds
;;;; them per session and per local day, warns the model once a budget is
;;;; nearly spent (a :ROUND hook recording one line into the turn), and stops
;;;; tool-driven work once one is exceeded (a :TOOL hook answering
;;;; NLE:FAILURE). With no budget configured it only meters and reports, and
;;;; never blocks.
;;;;
;;;; One honest limit, as the original says: the brake cannot cancel the
;;;; reply already being generated when a budget trips; it stops the work that
;;;; reply asks for next. Runaway spend is tool loops, one more provider
;;;; round each, and that is what the :TOOL refusal ends.
;;;;
;;;; What is read. The store's turn.usage facts only -- token counts, cost
;;;; and the clock -- and nothing else on the machine: no network, no keys,
;;;; no files of its own. A provider that reported no usage for a round left
;;;; no counts, and that round meters as nothing rather than an estimate.
;;;;
;;;; Config, the "costguard" section:
;;;;   {"session_token_budget": 200000, "daily_token_budget": 1000000,
;;;;    "session_cost_budget_usd": "2.00", "daily_cost_budget_usd": "10.00",
;;;;    "warn_percent": 80}
;;;; each budget 0 or absent means off; the cost budgets are written as
;;;; strings because a section has no decimal member.

(defpackage #:nodecode-costguard
  (:use #:cl)
  (:export #:report #:spend #:breached))

(in-package #:nodecode-costguard)

(nlk:define-peripheral costguard :not-running t)

;;; --- what the rounds spent ---------------------------------------------------

(defparameter +usage-kind+ "turn.usage"
  "The fact every provider round leaves: its counts, its price, its clock.")

(defun payload-number (key)
  "The SQL column reading KEY, a payload member, as a number (0 when absent)."
  (format nil "coalesce(json_extract(payload, '$.\"~a\"'), 0)" key))

(defparameter +usage-columns+
  (list (payload-number "input-tokens")
        (payload-number "output-tokens")
        (payload-number "reasoning-tokens")
        (payload-number "cost_usd")
        (payload-number "cost_known")
        "occurred_at")
  "One row per metered round: (INPUT OUTPUT REASONING COST KNOWN AT).")

(defvar *usage-reader* nil
  "NIL reads the store; a test binds a function of the same keywords
answering rows in +USAGE-COLUMNS+' shape, so nothing here needs a database.")

(defun store-rows (&key session-id since)
  "The store's turn.usage rows, oldest first, optionally one session's or
those at or after the UTC stamp SINCE. An unreadable store reads as none:
metering is never a turn's precondition."
  (when (nlk:store-open-p)
    (handler-case
        (nlk:events :kind +usage-kind+
                    :session-id session-id
                    :where (and since (cons "occurred_at >= ?" (list since)))
                    :order :oldest
                    :as :rows
                    :columns +usage-columns+)
      (error () '()))))

(defun usage-rows (&key session-id since)
  "Every metered round this cell can see: SESSION-ID's, or the home's at or
after SINCE."
  (if *usage-reader*
      (funcall *usage-reader* :session-id session-id :since since)
      (store-rows :session-id session-id :since since)))

(defun utc-stamp (universal)
  "UNIVERSAL as the UTC ISO-8601 stamp the event log writes."
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time universal 0)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d.000Z"
            year month date hour minute second)))

(defun day-start (&optional (now (get-universal-time)))
  "NOW's local midnight, as the UTC stamp that compares against occurred_at."
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time now)
    (declare (ignore second minute hour))
    (utc-stamp (encode-universal-time 0 0 0 date month year 0))))

(defun empty-bucket ()
  "No round yet: (:TOKENS :COST :CALLS :UNPRICED)."
  (list :tokens 0 :cost 0d0 :calls 0 :unpriced 0))

(defun fold-rows (rows &optional (bucket (empty-bucket)))
  "ROWS summed into BUCKET: billable tokens (input, output, reasoning; cache
reads are free), the stored cost, and the rounds the catalog had no price for."
  (dolist (row rows bucket)
    (destructuring-bind (input output reasoning cost known at) row
      (declare (ignore at))
      (incf (getf bucket :tokens) (+ (or input 0) (or output 0) (or reasoning 0)))
      (incf (getf bucket :cost) (coerce (or cost 0) 'double-float))
      (incf (getf bucket :calls))
      (unless (and known (plusp known))
        (incf (getf bucket :unpriced))))))

(defun spend (&key session-id (now (get-universal-time)))
  "What is spent this session and this local day, as
(:SESSION BUCKET :DAY BUCKET :DAY-KEY \"YYYY-MM-DD\")."
  (let ((day (day-start now)))
    (list :session (if session-id (fold-rows (usage-rows :session-id session-id)) (empty-bucket))
          :day (fold-rows (usage-rows :since day))
          :day-key (subseq day 0 10))))

(defun recent-days (&key (now (get-universal-time)) (count 3))
  "The last COUNT local days' spend, oldest first, as ((DATE . BUCKET) ...)."
  (let ((start (day-start (- now (* 86400 (1- count)))))
        (buckets (make-hash-table :test #'equal))
        (order '()))
    (dolist (row (usage-rows :since start))
      (let* ((date (subseq (sixth row) 0 10))
             (bucket (or (gethash date buckets)
                         (progn (push date order) (setf (gethash date buckets) (empty-bucket))))))
        (fold-rows (list row) bucket)))
    (loop for date in (sort order #'string<) collect (cons date (gethash date buckets)))))

;;; --- the budgets ---------------------------------------------------------------

(defun dollars (value)
  "VALUE -- a number, or the string a section holds -- as US dollars."
  (cond ((realp value) (coerce value 'double-float))
        ((stringp value)
         (let* ((text (string-trim '(#\Space #\Tab #\$ #\u #\U #\s #\S #\d #\D) value))
                (dot (position #\. text))
                (whole (if dot (subseq text 0 dot) text))
                (fraction (if dot (subseq text (1+ dot)) "")))
           (if (and (plusp (length whole))
                    (every #'digit-char-p whole)
                    (every #'digit-char-p fraction))
               (coerce (+ (parse-integer whole)
                          (if (plusp (length fraction))
                              (/ (parse-integer fraction) (expt 10 (length fraction)))
                              0))
                       'double-float)
               0.0d0)))
        (t 0.0d0)))

(defun budgets ()
  "The four budgets as a plist; 0 is off."
  (list :session-tokens (or (ignore-errors (setting :session-token-budget)) 0)
        :daily-tokens (or (ignore-errors (setting :daily-token-budget)) 0)
        :session-cost (dollars (ignore-errors (setting :session-cost-budget-usd)))
        :daily-cost (dollars (ignore-errors (setting :daily-cost-budget-usd)))))

(defun budget-any-p (&optional (budgets (budgets)))
  "Whether any budget is on; with none, nothing here blocks or warns."
  (some (lambda (value) (and (realp value) (plusp value)))
        (list (getf budgets :session-tokens) (getf budgets :daily-tokens)
              (getf budgets :session-cost) (getf budgets :daily-cost))))

(defun warn-percent ()
  "The percent of a budget past which the model is told to finish, 80 by default."
  (let ((percent (ignore-errors (setting :warn-percent))))
    (if (and (integerp percent) (<= 1 percent)) percent 80)))

(defun checks (spend)
  "Each budget beside what is spent of it: (LABEL USED LIMIT KIND)."
  (let ((budgets (budgets)))
    (list (list "session token budget" (getf (getf spend :session) :tokens)
                (getf budgets :session-tokens) :tokens)
          (list "daily token budget" (getf (getf spend :day) :tokens)
                (getf budgets :daily-tokens) :tokens)
          (list "session cost budget" (getf (getf spend :session) :cost)
                (getf budgets :session-cost) :cost)
          (list "daily cost budget" (getf (getf spend :day) :cost)
                (getf budgets :daily-cost) :cost))))

(defun breached (spend)
  "Every budget SPEND is over, as (LABEL USED LIMIT KIND)."
  (remove-if-not (lambda (check) (destructuring-bind (label used limit kind) check
                                    (declare (ignore label kind))
                                    (and (realp limit) (plusp limit) (> used limit))))
                  (checks spend)))

(defun warned (spend)
  "Every budget SPEND is at or past the warn percent of, and not over."
  (let ((ratio (/ (warn-percent) 100.0d0)))
    (remove-if-not (lambda (check)
                     (destructuring-bind (label used limit kind) check
                       (declare (ignore label kind))
                       (and (realp limit) (plusp limit)
                            (<= used limit)
                            (>= used (* limit ratio)))))
                   (checks spend))))

(defun amount (value kind)
  "VALUE as the report writes it, in tokens or in dollars."
  (if (eq kind :cost)
      (format nil "$~,4f" (coerce value 'double-float))
      (format nil "~:d tokens" (round value))))

(defun limit-text (limit kind)
  "LIMIT as the report writes it, in tokens or in dollars."
  (if (eq kind :cost)
      (format nil "$~,2f" (coerce limit 'double-float))
      (format nil "~:d tokens" (round limit))))

(defun breach-line (breach)
  "One exceeded budget, in words."
  (destructuring-bind (label used limit kind) breach
    (format nil "~a: ~a used of ~a" label (amount used kind) (limit-text limit kind))))

(defun budget-line ()
  "The configured budgets, in words."
  (let ((budgets (budgets)))
    (format nil "Budgets: ~{~a~^, ~}"
            (or (loop for (key label) in '((:session-tokens "session tokens")
                                           (:daily-tokens "daily tokens")
                                           (:session-cost "session cost")
                                           (:daily-cost "daily cost"))
                      for value = (getf budgets key)
                      when (and (realp value) (plusp value))
                        collect (if (member key '(:session-cost :daily-cost))
                                    (format nil "~a ~a" label (limit-text value :cost))
                                    (format nil "~a ~d" label value)))
                '("none configured (metering only)")))))

;;; --- the report ------------------------------------------------------------------

(defun bucket-line (label bucket)
  "One bucket as a line, or NIL when it metered nothing."
  (when (plusp (getf bucket :calls))
    (format nil "- ~a: ~:d tokens in ~:d call~:p, est. cost $~,4f~@[ (~:d unpriced call~:p)~]"
            label (getf bucket :tokens) (getf bucket :calls) (getf bucket :cost)
            (let ((unpriced (getf bucket :unpriced))) (and (plusp unpriced) unpriced)))))

(defun report (&key session-id days)
  "What this session and this day have spent, the budgets, and any breach.
With DAYS, the last three local days are listed too."
  (let* ((spend (spend :session-id session-id))
         (session-line (bucket-line "this session" (getf spend :session)))
         (day-line (bucket-line (format nil "~a (today)" (getf spend :day-key))
                                (getf spend :day)))
         (day-lines (and days (loop for (date . bucket) in (recent-days)
                                    collect (bucket-line date bucket))))
         (breaches (breached spend)))
    (format nil "~{~a~^~%~}"
            (remove nil
                    (append (list "CostGuard usage:")
                            (if (or session-line day-line day-lines)
                                (list session-line day-line)
                                (list "CostGuard has not metered any API call yet."))
                            day-lines
                            (list (budget-line))
                            (when breaches
                              (list (format nil "Over budget: ~{~a~^; ~}"
                                            (mapcar #'breach-line breaches))))))))) 

;;; --- the line a turn is told ---------------------------------------------------------

(defvar *told* (make-hash-table :test #'equal)
  "SESSION and LEVEL -> T: one note per session and level, never one per round.")

(defun tell-once (session level text)
  "Record TEXT as the turn's prompt note, once per SESSION and LEVEL."
  (let ((key (format nil "~a|~a" (or session "") level)))
    (unless (gethash key *told*)
      (when (> (hash-table-count *told*) 2000) (clrhash *told*))
      (setf (gethash key *told*) t)
      t)))

(defun breach-note (breaches)
  "What a turn over budget is told."
  (format nil "CostGuard: BUDGET EXCEEDED (~{~a~^; ~}). Your tool calls are now blocked. ~
Wrap up with a final summary for the user; do not attempt more tool-driven work."
          (mapcar #'breach-line breaches)))

(defun warn-note (warnings)
  "What a turn near a budget is told."
  (format nil "CostGuard warning: approaching budget at ~d% (~{~a~^; ~}). ~
Prefer finishing over starting new sub-tasks."
          (warn-percent) (mapcar #'breach-line warnings)))

(defun budget-note ()
  "What a turn's first round is told when budgets are on."
  (format nil "CostGuard is active with budgets configured (~{~a~^, ~}). Work efficiently; ~
when a budget is exceeded your tool calls will be blocked."
          (loop for (key label) in '((:session-tokens "session tokens")
                                     (:daily-tokens "daily tokens")
                                     (:session-cost "session cost usd")
                                     (:daily-cost "daily cost usd"))
                for value = (getf (budgets) key)
                when (and (realp value) (plusp value))
                  collect (if (member key '(:session-cost :daily-cost))
                              (format nil "~a ~,2f" label value)
                              (format nil "~a ~d" label value)))))

(defun session-of (op)
  "The session a call belongs to: the op's own, else the running turn's."
  (or (getf op :session-id) (getf (nle:turn) :session-id)))

(defun warn-at-round (op next)
  "A :ROUND point hook: a note rides the request once any budget nears, and
once when one is exceeded. Never the turn's failure."
  (prog1 (funcall next op)
    (handler-case
        (when (and (getf op :turn) (budget-any-p))
          (let* ((session (session-of op))
                 (spend (spend :session-id session))
                 (breaches (breached spend))
                 (warnings (warned spend))
                 (level (cond (breaches "breach") (warnings "warn")
                              ((eql 0 (or (getf op :round) 0)) "budget"))))
            (when (and level (tell-once session level
                                         (cond (breaches (breach-note breaches))
                                               (warnings (warn-note warnings))
                                               (t (budget-note)))))
              (nlk:record-turn-context-prompt
               (getf op :turn) :text
               (cond (breaches (breach-note breaches))
                     (warnings (warn-note warnings))
                     (t (budget-note)))))))
      (error () nil))))

;;; --- the brake -------------------------------------------------------------------

(defparameter +own-tools+ '("costguard_status")
  "This cell's own tool: it stays callable while the brake is on, so the
model can read the numbers it is asked to act on.")

(defun brake (op next)
  "A :TOOL point hook: past a budget, the call is refused and NEXT never
runs; under it, and with no budget, the call is the call."
  (let ((name (getf op :name)))
    (if (or (member name +own-tools+ :test #'string=) (not (budget-any-p)))
        (funcall next op)
        (let ((breaches (breached (spend :session-id (session-of op)))))
          (if breaches
              (nle:failure
               (format nil "ERROR: CostGuard blocked this tool call: budget exceeded (~{~a~^; ~}). ~
No further tool-driven work is allowed until the budget is raised in the CostGuard settings or a ~
new budget period starts. Give the user a final summary of what was accomplished."
                       (mapcar #'breach-line breaches)))
              (funcall next op))))))

;;; --- the surfaces -------------------------------------------------------------------

(defun status-tool (&rest ignored)
  "costguard_status: this session's and today's spend, the budgets, and any breach."
  (declare (ignore ignored))
  (report :session-id (getf (nle:turn) :session-id) :days t))

(defun run-slash (args session-id)
  "/cost: the report the operator reads; `budgets' names the configured limits."
  (let ((verb (string-downcase (nlk:trimmed (or args "")))))
    (cond ((member verb '("" "usage" "status") :test #'string=)
           (report :session-id session-id))
          ((equal verb "budgets") (budget-line))
          (t (format nil "costguard: ~a is not one of its choices: usage, budgets" verb)))))

(defparameter +manual+
  "CostGuard meters what this image spends and enforces a budget while there is still time to matter.
Every provider round's own usage -- input, output and reasoning tokens, priced with the catalog's
price when the round was recorded -- is folded per session and per local day.
  (costguard_status)   this session's and today's spend, the budgets, and any breach; the tool the
      model calls when the operator asks what a run has cost.
  (costguard:report :session-id \"s\" :days t)   the same from eval.
  /cost   the report for this session; /cost budgets   the configured limits.
The budgets are the section's session_token_budget, daily_token_budget, session_cost_budget_usd and
daily_cost_budget_usd; 0 or absent is off, and with none set CostGuard only meters and reports.
Past warn_percent (80 by default) of a budget one line rides the turn: prefer finishing over
starting new sub-tasks. Once a budget is exceeded, tool calls are refused until it is raised or a
new day or session begins -- the loop that spends is the loop the brake stops -- and the reply
already streaming is not cancelled, so the model is told to summarise. Cache reads are free, as the
provider bills them; a round whose provider reported no usage meters as nothing, never an estimate.
ERROR: COSTGUARD is text; it reads the store and nothing else on the machine, and never blocks with
no budget configured."
  "What (help :costguard) answers while the cell runs.")

(nle:define-cell costguard
  (:section ("costguard")
    (:guide "set a budget to enforce one: session_token_budget or daily_token_budget (tokens), session_cost_budget_usd or daily_cost_budget_usd (dollars, written as a string like \"2.00\"); 0 or absent is off, and with none CostGuard only meters; warn_percent is where the model is told to finish; /cost shows what is spent")
    ("session_token_budget" :integer :default 0 :min 0
     :doc "billable tokens one session may spend; 0 is off")
    ("daily_token_budget" :integer :default 0 :min 0
     :doc "billable tokens one local day may spend across sessions; 0 is off")
    ("session_cost_budget_usd" :string :default ""
     :doc "US dollars one session may spend, written as a string such as \"2.00\"; empty is off")
    ("daily_cost_budget_usd" :string :default ""
     :doc "US dollars one local day may spend, written as a string such as \"10.00\"; empty is off")
    ("warn_percent" :integer :default 80 :min 1
     :doc "percent of a budget at which the model is told to finish what it started"))
  (:tool "costguard_status"
         (list :description "CostGuard's metered spend: this session's and today's tokens and estimated cost, the configured budgets, and any budget breach. Takes no argument."
               :parameters (nlk:json-object "type" "object"
                                             "properties" (nlk:json-object)))
         #'status-tool)
  (:hook :round #'warn-at-round)
  (:hook :tool #'brake)
  (:command "cost" 'run-slash
            :description "What this session has spent, and the budgets"
            :argument-hint "[usage|budgets]")
  (:help :costguard
         "costguard: what this image spent in tokens and dollars, with budgets that warn and then refuse tool calls - (help :costguard)"
         +manual+))

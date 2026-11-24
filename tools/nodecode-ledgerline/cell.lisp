;;;; cell.lisp --- what every session costs, by day, model and session, with budgets.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every figure here is a fact the store already holds: each model round
;;;; records a `turn.usage' event whose payload names the model, the provider
;;;; and the four token counts, and, when the gateway could price it, its
;;;; cost. Nothing is repriced and nothing is fetched: an unpriced round
;;;; stays unpriced here, counted and named, never guessed. That is the whole
;;;; of the reading -- one query over the log, aggregated in Lisp -- so the
;;;; ledger answers the same whether a session ran a minute or a month ago.
;;;;
;;;; Three tools and one command over that one read: ledger_spend (a range,
;;;; its days, its models, its costliest sessions), ledger_session (one
;;;; session's receipt, split into cache reads, fresh input, cache writes and
;;;; output) and ledger_budget (this month against the monthly budget, and
;;;; one session against the per-session budget, with the pace to month end
;;;; and the two warning lines). /ledger is the operator's way in.

(defpackage #:nodecode-ledgerline
  (:use #:cl)
  (:nicknames #:ledgerline)
  (:export
   ;; the verbs, every one a string
   #:spend #:receipt #:budget
   ;; the one seam: a function of (:WHERE . ARGS) (:SESSION ID) => USAGE-ROUNDs
   #:*rounds*
   ;; the one condition
   #:ledgerline-error))

(in-package #:nodecode-ledgerline)

(nlk:define-peripheral ledgerline :not-running t)

;;; --- the unit the ledger is made of -----------------------------------------------

(defstruct (usage-round (:constructor make-usage-round))
  "One priced-or-not model round as the store recorded it. COST is NIL when
the gateway could not price it -- absent, not zero."
  (session "" :type string)
  (at "" :type string)
  (model "" :type string)
  (provider "" :type string)
  (input 0 :type integer)
  (cached 0 :type integer)
  (write 0 :type integer)
  (output 0 :type integer)
  (reasoning 0 :type integer)
  (cost nil)
  (voided nil))

;;; --- the one read ----------------------------------------------------------------

(defun range-where (range)
  "The clock a RANGE starts at, as the store's WHERE clause and its
arguments, or NIL for all of time. The store keeps UTC clocks; a day and a
month are the operator's own."
  (let ((range (string-downcase (nlk:trimmed (or range "")))))
    (cond ((member range '("" "all" "all time") :test #'equal) nil)
          ((string= range "today")
           (list "date(occurred_at, 'localtime') = date('now', 'localtime')"))
          ((string= range "week") (list "occurred_at >= datetime('now', '-7 days')"))
          ((string= range "7d") (list "occurred_at >= datetime('now', '-7 days')"))
          ((string= range "30d") (list "occurred_at >= datetime('now', '-30 days')"))
          ((string= range "month")
           (list "strftime('%Y-%m', occurred_at, 'localtime') = strftime('%Y-%m', 'now', 'localtime')"))
          (t (fail "~s is no range: today, 7d, 30d, month, all" range)))))

(defparameter +round-columns+
  '("session_id" "occurred_at"
    "coalesce(json_extract(payload, '$.model'), '')"
    "coalesce(json_extract(payload, '$.provider'), '')"
    "coalesce(json_extract(payload, '$.input-tokens'), 0)"
    "coalesce(json_extract(payload, '$.cached-input-tokens'), 0)"
    "coalesce(json_extract(payload, '$.cache-write-tokens'), 0)"
    "coalesce(json_extract(payload, '$.output-tokens'), 0)"
    "coalesce(json_extract(payload, '$.reasoning-tokens'), 0)"
    "case when json_extract(payload, '$.cost_known') = 1
          then json_extract(payload, '$.cost_usd') end"
    "coalesce(json_extract(payload, '$.voided') = 1, 0)")
  "One row per recorded round: the envelope's session and clock, then what
the payload says, each count 0 when the payload is silent and the cost
NULL when the gateway had none. This is the cell's only SQL.")

(defun store-rounds (&key where session)
  "The rounds the store holds, narrowed to WHERE (a SQL clause and its
arguments, or NIL) and to SESSION when one is named."
  (unless (nlk:store-open-p)
    (fail "no store is open: the ledger reads the log this organism keeps"))
  (loop for (session-id at model provider input cached write output reasoning cost voided)
          in (nlk:events :kind "turn.usage" :session-id session :where where
                         :order :oldest :as :rows :columns +round-columns+)
        collect (make-usage-round :session (or session-id "")
                                  :at (or at "")
                                  :model (or model "")
                                  :provider (or provider "")
                                  :input (or input 0)
                                  :cached (or cached 0)
                                  :write (or write 0)
                                  :output (or output 0)
                                  :reasoning (or reasoning 0)
                                  :cost cost
                                  :voided (and voided (not (zerop voided))))))

(defparameter *rounds* #'store-rounds
  "Where every tool reads its rounds: a function of the same keys
STORE-ROUNDS takes. The one seam a test stubs, so no test needs a store.")

(defun rounds (range &key session)
  "The rounds of RANGE (and of SESSION when one is named)."
  (funcall *rounds* :where (range-where range) :session session))

;;; --- adding them up ---------------------------------------------------------------

(defun tally (rounds)
  "ROUNDS as one plist: :CALLS, the :PRICED ones, :UNPRICED ones, :VOIDED
attempts, the :COST of the priced ones, the four token counts and
:SESSIONS, each session id and its own tally."
  (let ((sum (list :calls 0 :priced 0 :unpriced 0 :voided 0 :cost 0.0 :input 0
                   :cached 0 :write 0 :output 0 :reasoning 0))
        (sessions (make-hash-table :test #'equal)))
    (dolist (round rounds)
      (when (usage-round-voided round) (incf (getf sum :voided)))
      (incf (getf sum :calls))
      (if (usage-round-cost round)
          (progn (incf (getf sum :priced))
                 (incf (getf sum :cost) (usage-round-cost round)))
          (incf (getf sum :unpriced)))
      (incf (getf sum :input) (usage-round-input round))
      (incf (getf sum :cached) (usage-round-cached round))
      (incf (getf sum :write) (usage-round-write round))
      (incf (getf sum :output) (usage-round-output round))
      (incf (getf sum :reasoning) (usage-round-reasoning round))
      (let ((one (or (gethash (usage-round-session round) sessions)
                     (setf (gethash (usage-round-session round) sessions)
                           (list :calls 0 :priced 0 :unpriced 0 :voided 0 :cost 0.0 :input 0
                                 :cached 0 :write 0 :output 0 :reasoning 0)))))
        (incf (getf one :calls))
        (if (usage-round-cost round)
            (progn (incf (getf one :priced)) (incf (getf one :cost) (usage-round-cost round)))
            (incf (getf one :unpriced)))
        (incf (getf one :input) (usage-round-input round))
        (incf (getf one :cached) (usage-round-cached round))
        (incf (getf one :write) (usage-round-write round))
        (incf (getf one :output) (usage-round-output round))
        (incf (getf one :reasoning) (usage-round-reasoning round))))
    (setf (getf sum :sessions) sessions)
    sum))

(defun grouped (rounds key)
  "ROUNDS grouped by KEY, each group's name, its tally, and the rounds
themselves, costliest first."
  (let ((table (make-hash-table :test #'equal))
        (order '()))
    (dolist (round rounds)
      (let* ((name (funcall key round))
             (cell (gethash name table)))
        (unless cell
          (setf cell (setf (gethash name table) (list :rounds '())))
          (push name order))
        (push round (getf cell :rounds))))
    (sort (loop for name in order
                for cell = (gethash name table)
                collect (list name (tally (reverse (getf cell :rounds)))))
          #'> :key (lambda (group) (getf (second group) :cost)))))

(defun pct (part whole)
  "PART of WHOLE in whole percent, 0 when WHOLE is 0."
  (if (and whole (plusp whole)) (round (* 100 (/ part whole))) 0))

(defun commas (n)
  "N with its thousands kept apart, so a token count reads."
  (let* ((s (princ-to-string (abs n)))
         (parts '()))
    (loop with end = (length s)
          for start = (max 0 (- end 3))
          while (plusp end)
          do (push (subseq s start end) parts)
             (setf end start))
    (format nil "~@[~a~]~{~a~^,~}" (and (minusp n) "-") parts)))

(defun money (value)
  "VALUE in dollars, or \"unknown\" when the gateway priced nothing."
  (cond ((null value) "unknown")
        ((zerop value) "$0")
        ((< (abs value) 0.01) (format nil "$~,5f" value))
        (t (format nil "$~,2f" value))))

(defun call-word (n) (if (= n 1) "call" "calls"))

(defun session-word (n) (if (= n 1) "session" "sessions"))

(defun session-label (id)
  "ID with the session's own title when the store still holds one."
  (let ((title (ignore-errors (nlk::session-title id))))
    (if (and (stringp title) (plusp (length title)))
        (format nil "~a (~a)" id (nlk:clip (nlk:one-line title) 48))
        id)))

(defun cache-line (sum &aux (asked (+ (getf sum :input) (getf sum :cached) (getf sum :write))))
  (format nil "cache: ~d% of ~a input tokens read from cache, ~a written"
          (pct (getf sum :cached) asked) (commas asked) (commas (getf sum :write))))

;;; --- what is worth a look ---------------------------------------------------------

(defun advice (sum groups &key (budget-share nil) (limit 3))
  "The lines a person should read after the numbers: rounds nothing could
price, a cache read that saved little, one model carrying the bill, a
session that is most of it, and the budget when BUDGET-SHARE names one."
  (let ((lines '()))
    (when (plusp (getf sum :unpriced))
      (push (format nil "~d ~a the gateway could not price: unknown, not zero"
                    (getf sum :unpriced) (call-word (getf sum :unpriced)))
            lines))
    (let ((asked (+ (getf sum :input) (getf sum :cached) (getf sum :write))))
      (when (and (>= asked 100000) (< (pct (getf sum :cached) asked) 50))
        (push (format nil "only ~d% of ~a input tokens were cache reads: a stable prompt pays"
                      (pct (getf sum :cached) asked) (commas asked))
              lines)))
    (when (and groups (plusp (getf sum :cost)))
      (destructuring-bind (name group) (first groups)
        (when (>= (pct (getf group :cost) (getf sum :cost)) 60)
          (push (format nil "~a is ~d% of the spend"
                        (if (stringp name) name (princ-to-string name))
                        (pct (getf group :cost) (getf sum :cost)))
                lines))))
    (when budget-share
      (push budget-share lines))
    (when lines
      (format nil "~%worth a look:~%~{  - ~a~^~%~}" (subseq lines 0 (min limit (length lines)))))))

;;; --- the three answers ------------------------------------------------------------

(defun guard ()
  "Refuse before reading when the cell was never started."
  (unless *ledgerline*
    (fail "the ledger is not running: turn the nodecode-ledgerline cell on")))

(defun spend (range)
  "What RANGE cost, by day, by model and by session, and what to change."
  (guard)
  (let* ((rounds (rounds range))
         (sum (tally rounds))
         (days (sort (grouped rounds (lambda (r) (subseq (usage-round-at r) 0 (min 10 (length (usage-round-at r))))))
                     #'string< :key #'first))
         (models (grouped rounds (lambda (r)
                                   (if (plusp (length (usage-round-model r)))
                                       (format nil "~a~@[ (~a)~]" (usage-round-model r)
                                               (and (plusp (length (usage-round-provider r)))
                                                    (usage-round-provider r)))
                                       "no model recorded"))))
         (sessions (grouped rounds #'usage-round-session)))
    (when (null rounds) (return-from spend (format nil "ledger ~a: nothing ran" range)))
    (format nil "ledger ~a: ~a over ~d ~a in ~d ~a~@[ (~d unpriced)~]~%~a~%~
                 by day:~%~{  ~a  ~a  ~d ~a~^~%~}~%~
                 by model:~%~{  ~a  ~a  ~d ~a~^~%~}~%~
                 costliest sessions:~%~{  ~a  ~a  ~d ~a~^~%~}~a"
            range (money (getf sum :cost)) (getf sum :calls) (call-word (getf sum :calls))
            (hash-table-count (getf sum :sessions)) (session-word (hash-table-count (getf sum :sessions)))
            (and (plusp (getf sum :unpriced)) (getf sum :unpriced))
            (cache-line sum)
            (loop for (day group) in (last days (min 14 (length days)))
                  collect day collect (money (getf group :cost)) collect (getf group :calls)
                  collect (call-word (getf group :calls)))
            (loop for (name group) in (subseq models 0 (min 6 (length models)))
                  collect name collect (money (getf group :cost)) collect (getf group :calls)
                  collect (call-word (getf group :calls)))
            (loop for (name group) in (subseq sessions 0 (min (setting :top-sessions) (length sessions)))
                  collect (or name "(no session)") collect (money (getf group :cost))
                  collect (getf group :calls) collect (call-word (getf group :calls)))
            (or (advice sum models) ""))))

(defun receipt (session-id)
  "One session's receipt: its calls, its token split, its models, and where
it stands against the per-session budget."
  (guard)
  (let* ((rounds (rounds "all" :session session-id))
         (sum (tally rounds))
         (models (grouped rounds (lambda (r) (usage-round-model r))))
         (budget (or (setting :session-budget-usd) 0)))
    (when (null rounds)
      (return-from receipt (format nil "session ~a: no priced or unpriced round is recorded" session-id)))
    (format nil "session ~a: ~a over ~d ~a~%~a~%~
                 released: ~a out~@[, ~a reasoning~]~%~
                 models: ~{~a ~d ~a ~a~^; ~}~@[~%~a~]"
            session-id (money (getf sum :cost)) (getf sum :calls) (call-word (getf sum :calls))
            (cache-line sum)
            (commas (getf sum :output))
            (and (plusp (getf sum :reasoning)) (commas (getf sum :reasoning)))
            (loop for (name group) in models
                  collect (if (plusp (length name)) name "no model recorded")
                  collect (getf group :calls) collect (call-word (getf group :calls))
                  collect (money (getf group :cost)))
            (when (plusp budget)
              (format nil "budget: ~a of ~a for the session (~d%)"
                      (money (getf sum :cost)) (money budget)
                      (pct (getf sum :cost) budget))))))

(defun days-in-month (month year)
  (if (member month '(4 6 9 11)) 30
      (if (and (= month 2)
               (or (and (zerop (mod year 4)) (not (zerop (mod year 100))))
                   (zerop (mod year 400))))
          29 28)))

(defun budget (&optional session-id)
  "This month against the monthly budget, with the pace to its end, and
SESSION-ID against the per-session budget. Nothing set reads as nothing."
  (guard)
  (let* ((monthly (or (setting :monthly-budget-usd) 0))
         (sum (tally (rounds "month")))
         (spent (getf sum :cost))
         (today (multiple-value-list (decode-universal-time (get-universal-time))))
         (date (third today))
         (length (days-in-month (fourth today) (fifth today)))
         (lines '()))
    (push (if (plusp monthly)
              (format nil "ledger month: ~a of ~a monthly budget (~d%), day ~d of ~d: about ~a by month end"
                      (money spent) (money monthly) (pct spent monthly) date length
                      (money (* spent (/ length (max 1 date)))))
              (format nil "ledger month: ~a so far, no monthly budget set (ledgerline.monthly_budget_usd)"
                      (money spent)))
          lines)
    (when (plusp monthly)
      (push (format nil "warn at ~d% (~a) and at 100% (~a); now at ~d%"
                    (setting :warn-percent) (money (* monthly (/ (setting :warn-percent) 100.0)))
                    (money monthly) (pct spent monthly))
            lines))
    (when (and session-id (plusp (length session-id)))
      (let ((one (tally (rounds "all" :session session-id))))
        (push (if (plusp (or (setting :session-budget-usd) 0))
                  (format nil "session ~a: ~a of ~a (~d%)"
                          session-id (money (getf one :cost))
                          (money (setting :session-budget-usd))
                          (pct (getf one :cost) (setting :session-budget-usd)))
                  (format nil "session ~a: ~a, no per-session budget set"
                          session-id (money (getf one :cost))))
              lines)))
    (format nil "~{~a~^~%~}~@[~%~a~]" (nreverse lines)
            (and (plusp monthly) (>= (pct spent monthly) (setting :warn-percent))
                 (if (>= spent monthly)
                     (format nil "OVER: the month is at ~d% of its budget" (pct spent monthly))
                     (format nil "WARNING: the month is at ~d% of its budget" (pct spent monthly)))))))

;;; --- the tools' boundaries --------------------------------------------------------

(nlk:define-contract ledger-spend
  (:doc "What this machine's sessions cost over a range: the total, the
cache share, a line per day and per model, the costliest sessions, and the
few things worth changing. Every figure is what the store recorded.")
  ("range" :text :default "7d" :options '("today" "7d" "30d" "month" "all")
   :doc "the window: today, 7d, 30d, this month, or all of it"))

(nlk:define-contract ledger-session
  (:doc "One session's receipt: its calls, its input split into cache reads,
fresh input and cache writes, its output, its models, and its share of the
per-session budget.")
  ("session_id" :text :required t :doc "the session's id, as the store names it"))

(nlk:define-contract ledger-budget
  (:doc "This month against the monthly budget, with the pace to month end,
and one session against the per-session budget when you name one.")
  ("session_id" :text :default "" :doc "a session to check as well, or empty"))

(defun spend-tool (report)
  (spend (ledger-spend-range report)))

(defun session-tool (report)
  (receipt (ledger-session-session-id report)))

(defun budget-tool (report)
  (budget (ledger-budget-session-id report)))

;;; --- /ledger ---------------------------------------------------------------------

(defun panel ()
  "The dialog bare /ledger answers: one range a row."
  (nle:list-dialog
   :title "Ledgerline" :summary "What each window cost"
   :rows (list (nle:run-row "Today" "/ledger today" :detail "since this morning")
               (nle:run-row "Seven days" "/ledger 7d" :detail "the week so far")
               (nle:run-row "Thirty days" "/ledger 30d" :detail "the month behind")
               (nle:run-row "This month" "/ledger month" :detail "against the monthly budget")
               (nle:run-row "All of it" "/ledger all" :detail "every round the store holds"))))

(defun run-slash (args session-id)
  "/ledger [RANGE|session ID|budget]: what the sessions cost, by day, model
and session, or one session's receipt, or the month against its budget."
  (declare (ignore session-id))
  (let* ((text (nlk:trimmed (or args "")))
         (space (position-if (lambda (c) (member c '(#\Space #\Tab))) text))
         (head (if space (string-downcase (subseq text 0 space)) (string-downcase text)))
         (rest (if space (nlk:trimmed (subseq text space)) "")))
    (cond ((string= text "") (values (spend "today") (panel)))
          ((string= head "session") (values (receipt rest) (panel)))
          ((string= head "budget") (values (budget rest) (panel)))
          (t (values (spend text) (panel))))))

;;; --- the declaration -------------------------------------------------------------

(defparameter +primer+
  "Ledgerline reads what every session already recorded and adds it up:
ledger_spend (a range: today, 7d, 30d, month, all: total, cache share, a
line per day and per model, the costliest sessions, what to look at),
ledger_session (one session's receipt: cache reads, fresh input, cache
writes, output, models, its budget share) and ledger_budget (this month
against the monthly budget with the pace to month end, and one session
against the per-session budget). /ledger is the same for the operator.
Every figure is what the store recorded, at the price it recorded: a round
the gateway could not price is named unknown, never guessed. The budgets
are whole dollars in the `ledgerline' section; 0 is none."
  "What (help :ledgerline) answers while the cell runs.")

(nle:define-cell ledgerline
  (:section ("ledgerline")
    (:guide "nothing here is required: the ledger reads what the store already recorded. monthly_budget_usd and session_budget_usd are whole dollars, 0 for none; warn_percent is where a budget line starts warning")
    ("monthly_budget_usd" :integer :default 0 :min 0
                          :doc "whole dollars this month may cost; 0 is no budget")
    ("session_budget_usd" :integer :default 0 :min 0
                          :doc "whole dollars one session may cost; 0 is no budget")
    ("warn_percent" :integer :default 80 :min 1
                    :doc "where a budget starts warning, in percent")
    ("top_sessions" :integer :default 5 :min 1
                    :doc "how many sessions a summary names"))
  (:tool "ledger_spend" (nlk:contract 'ledger-spend) 'spend-tool)
  (:tool "ledger_session" (nlk:contract 'ledger-session) 'session-tool)
  (:tool "ledger_budget" (nlk:contract 'ledger-budget) 'budget-tool)
  (:help :ledgerline
         "ledgerline: what every session cost - ledger_spend, ledger_session, ledger_budget, /ledger"
         +primer+)
  (:command "ledger" 'run-slash
            :catches 'ledgerline-error
            :description "What the sessions cost: a range, one session, or the month's budget"))

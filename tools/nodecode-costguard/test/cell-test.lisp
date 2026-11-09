;;;; cell-test.lisp --- lifecycle, metering, the warning, the brake, the surfaces.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here opens a store, dials a provider or reads a file: every
;;;; metered round is a row this file hands the cell through its own
;;;; *USAGE-READER*, in the shape the store's turn.usage query answers.

(in-package #:nodecode.test)

;;; --- lifecycle ---------------------------------------------------------------------

(define-test-slice "costguard" "COSTGUARD-CELL-" :start nodecode-costguard:start-cell)

(define-cell-lifecycle-tests "costguard"
  (:hooks :round :tool)
  (:help :costguard)
  (:command "cost")
  (:running (is (nle::find-tool "costguard_status") "start registers the costguard_status tool"))
  (:stopped (is (null (nle::find-tool "costguard_status")) "stop unregisters the tool"))
  (:refused ("warn_percent" 0)))

;;; --- fixtures: metered rounds ---------------------------------------------------------

(defvar *rows* '()
  "The metered rounds the cell reads: (SESSION INPUT OUTPUT REASONING COST KNOWN AT).")

(defun stub-rows (&key session-id since)
  "What the store's turn.usage query answers over *ROWS*: one session's, or
every round at or after SINCE, each as (INPUT OUTPUT REASONING COST KNOWN AT)."
  (loop for (session input output reasoning cost known at) in *rows*
        when (and (or (null session-id) (equal session-id session))
                  (or (null since) (string>= at since)))
          collect (list input output reasoning cost known at)))

(defmacro with-rows ((&rest rows) &body body)
  "Run BODY with the cell reading ROWS instead of the store."
  `(let ((*rows* (list ,@rows))
         (nodecode-costguard::*usage-reader* #'stub-rows))
     (clrhash nodecode-costguard::*told*)
     ,@body))

(defun today (&optional (offset 0))
  "The local day OFFSET days from today, as the log's own UTC stamp."
  (nodecode-costguard::day-start (- (get-universal-time) (* offset 86400))))

(defun row (session input output &key (reasoning 0) (cost 0d0) (known t) (at (today)))
  "One metered round of SESSION."
  (list session input output reasoning cost (if known 1 0) at))

(defun call-with-costguard (pairs thunk)
  "Run THUNK with the cell started over PAIRS, stopped after."
  (let ((stop (apply #'costguard-start pairs)))
    (unwind-protect (funcall thunk) (funcall stop))))

(defun lines (text)
  "TEXT's lines."
  (uiop:split-string text :separator '(#\Newline)))

;;; --- metering ------------------------------------------------------------------------

(deftest costguard-cell-meters-tokens-and-recorded-cost ()
  (call-with-costguard
   nil
   (lambda ()
     (with-rows ((row "s1" 1000 200 :cost 0.0055d0)
                 (row "s1" 500 100 :reasoning 40 :cost 0.00125d0)
                 (row "s2" 10 10))
       (let ((spend (nodecode-costguard:spend :session-id "s1")))
         (is (= 1840 (getf (getf spend :session) :tokens)) "input, output and reasoning add up; cache reads are not counted")
         (is (= 2 (getf (getf spend :session) :calls)) "both of this session's rounds count")
         (is (< (abs (- 0.00675d0 (getf (getf spend :session) :cost))) 1d-9) "the stored cost sums")
         (is (= 1860 (getf (getf spend :day) :tokens)) "the day covers every session's rounds"))))))

(deftest costguard-cell-an-unpriced-round-is-counted-and-said ()
  (call-with-costguard
   nil
   (lambda ()
     (with-rows ((row "s1" 700 300 :known nil))
       (let ((session (getf (nodecode-costguard:spend :session-id "s1") :session)))
         (is (= 1000 (getf session :tokens)) "tokens are metered for a model the catalog does not price")
         (is (= 0d0 (getf session :cost)) "and no dollar figure is invented")
         (is (= 1 (getf session :unpriced)) "the round is flagged unpriced"))))))

(deftest costguard-cell-yesterdays-rounds-stay-out-of-today ()
  (call-with-costguard
   nil
   (lambda ()
     (with-rows ((row "s1" 100 100 :at (today 3))
                 (row "s1" 10 10))
       (let ((spend (nodecode-costguard:spend :session-id "s1")))
         (is (= 220 (getf (getf spend :session) :tokens)) "the session total keeps every round")
         (is (= 20 (getf (getf spend :day) :tokens)) "the daily total is the local day's"))))))

(deftest costguard-cell-the-cost-budgets-are-read-as-dollars ()
  (call-with-costguard
   '("session_cost_budget_usd" "2.50" "daily_cost_budget_usd" "10")
   (lambda ()
     (is (= 2.5d0 (getf (nodecode-costguard::budgets) :session-cost)) "a decimal string is dollars")
     (is (= 10d0 (getf (nodecode-costguard::budgets) :daily-cost)) "and a whole one too")))
  (call-with-costguard
   '("session_cost_budget_usd" "")
   (lambda ()
     (is (= 0d0 (getf (nodecode-costguard::budgets) :session-cost)) "an empty member is off"))))

;;; --- the brake -----------------------------------------------------------------------

(defun call-tool (name session)
  "Run NAME through the cell's :TOOL hook with a NEXT that answers :ran."
  (let ((hook (cell-hook "nodecode-costguard" :tool))
        (ran nil))
    (values (funcall hook (list :name name :arguments (nlk:make-json-object) :call-id "c1"
                                 :session-id session)
                     (lambda (op) (declare (ignore op)) (setf ran t) :ran))
            ran)))

(deftest costguard-cell-the-brake-refuses-only-over-budget ()
  (call-with-costguard
   '("session_token_budget" 1000)
   (lambda ()
     (with-rows ((row "s1" 900 200))
       (multiple-value-bind (answer ran) (call-tool "eval" "s1")
         (is (stringp answer) "a metered session past its token budget is refused")
         (is (search "CostGuard blocked this tool call" answer) "and the refusal says who refused it")
         (is (search "budget exceeded" answer) "and what was exceeded")
         (is (not ran) "the call never runs")))
     (with-rows ((row "s2" 10 5))
       (multiple-value-bind (answer ran) (call-tool "eval" "s2")
         (is (eq :ran answer) "a session under its budget runs")
         (is ran "and the call really ran"))))))

(deftest costguard-cell-the-brake-lets-its-own-tool-through ()
  (call-with-costguard
   '("session_token_budget" 10)
   (lambda ()
     (with-rows ((row "s1" 900 200))
       (multiple-value-bind (answer ran) (call-tool "costguard_status" "s1")
         (is (eq :ran answer) "the status tool stays callable while blocked")
         (is ran "and runs"))))))

(deftest costguard-cell-no-budget-never-blocks ()
  (call-with-costguard
   nil
   (lambda ()
     (with-rows ((row "s1" 10000000 1000000))
       (multiple-value-bind (answer ran) (call-tool "eval" "s1")
         (is (eq :ran answer) "with no budget configured the brake does not engage")
         (is ran "and the call runs"))))))

(deftest costguard-cell-a-daily-budget-blocks-across-sessions ()
  (call-with-costguard
   '("daily_token_budget" 1000)
   (lambda ()
     (with-rows ((row "other" 1200 0))
       (multiple-value-bind (answer ran) (call-tool "eval" "s1")
         (is (stringp answer) "today's budget is the home's, not the session's")
         (is (search "daily token budget" answer) "and the refusal names it")
         (is (not ran) "the call is refused"))))))

;;; --- the warning ---------------------------------------------------------------------

(deftest costguard-cell-a-turn-near-a-budget-is-told-once ()
  (call-with-costguard
   '("session_token_budget" 1000)
   (lambda ()
     (with-rows ((row "s1" 850 0))
       (let ((seen '())
             (hook (cell-hook "nodecode-costguard" :round)))
         (is (functionp hook) "the round hook is installed")
         (with-stubbed-fdefinitions
             ((nlk:record-turn-context-prompt (turn &key text)
                (declare (ignore turn))
                (push text seen)))
           (flet ((one-round (round)
                    (funcall hook (list :session-id "s1" :turn "t1" :round round)
                             (lambda (op) (declare (ignore op)) nil))))
             (one-round 0)
             (one-round 1)))
         (is (= 1 (length seen)) "one note for the session, not one per round")
         (is (search "CostGuard warning" (first seen)) "the note is a warning")
         (is (search "850" (first seen)) "and names what was spent")
         (is (search "80%" (first seen)) "and the ratio it crossed"))))))

(deftest costguard-cell-a-turn-first-round-hears-the-budgets ()
  (call-with-costguard
   '("daily_token_budget" 500000)
   (lambda ()
     (with-rows ()
       (let ((seen '())
             (hook (cell-hook "nodecode-costguard" :round)))
         (with-stubbed-fdefinitions
             ((nlk:record-turn-context-prompt (turn &key text)
                (declare (ignore turn))
                (push text seen)))
           (funcall hook (list :session-id "s1" :turn "t1" :round 0)
                    (lambda (op) (declare (ignore op)) nil)))
         (is (= 1 (length seen)) "the first round is told the budgets are on")
         (is (search "CostGuard is active" (first seen)) "in the cell's own words")
         (is (search "daily tokens 500000" (first seen)) "naming what is configured"))))))

(deftest costguard-cell-an-exceeded-turn-is-told-to-wrap-up ()
  (call-with-costguard
   '("session_token_budget" 100)
   (lambda ()
     (with-rows ((row "s1" 900 0))
       (let ((seen '())
             (hook (cell-hook "nodecode-costguard" :round)))
         (with-stubbed-fdefinitions
             ((nlk:record-turn-context-prompt (turn &key text)
                (declare (ignore turn))
                (push text seen)))
           (funcall hook (list :session-id "s1" :turn "t1" :round 2)
                    (lambda (op) (declare (ignore op)) nil)))
         (is (search "BUDGET EXCEEDED" (first seen)) "a breached budget is stated plainly")
         (is (search "blocked" (first seen)) "and says the tool calls are blocked"))))))

;;; --- the surfaces --------------------------------------------------------------------

(deftest costguard-cell-the-tool-and-the-command-answer ()
  (call-with-costguard
   '("daily_token_budget" 1000)
   (lambda ()
     (with-rows ((row "s1" 900 200 :cost 0.02d0))
       (let ((tool (nle::find-tool "costguard_status")))
         (is tool "the tool is registered")
         (let ((text (funcall (third tool))))
           (is (search "CostGuard usage" text) "the tool answers the report")
           (is (search "today" text) "with today's line")
           (is (search "1,100 tokens" text) "and the tokens spent")
           (is (search "daily tokens 1000" text) "and the configured budget")
           (is (search "Over budget" text) "and the breach")))
       (let ((text (nodecode-costguard:report :session-id "s1")))
         (is (search "- this session: 1,100 tokens in 1 call" text) "/cost names this session's spend"))
       (is (search "daily tokens 1000" (cell-entry "nodecode-costguard" "cost" "budgets")) "/cost budgets names the limits")
       (is (search "not one of its choices" (cell-entry "nodecode-costguard" "cost" "nope")) "and refuses a word it does not know")))))

(deftest costguard-cell-with-nothing-metered-it-says-so ()
  (call-with-costguard
   nil
   (lambda ()
     (with-rows ()
       (is (search "has not metered any API call yet" (nodecode-costguard:report)) "an empty store answers one line rather than an empty table")
       (is (search "none configured (metering only)" (nodecode-costguard:report)) "and the budgets line says metering only")))))

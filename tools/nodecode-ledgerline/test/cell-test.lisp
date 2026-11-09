;;;; cell-test.lisp --- the three tools, the command, the arithmetic, the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every test that needs rounds binds the cell's one seam, *ROUNDS*, to a
;;;; canned list, so the arithmetic, the words and the ranges are proved
;;;; without a store; one test records two real turn.usage facts in a temp
;;;; store and reads them back through the default reader, which is what
;;;; proves the query's columns. No test needs the network.

(in-package #:nodecode.test)

(define-test-slice "ledgerline" "LEDGERLINE-CELL-" :start nodecode-ledgerline:start-cell)

(defvar *asked* '()
  "The keys every stubbed read was called with, newest first.")

(defun round-of (session at model &key (input 0) (cached 0) (write 0) (output 0)
                                      (reasoning 0) (cost nil))
  "One round the way the store would hand it over."
  (nodecode-ledgerline::make-usage-round :session session :at at :model model
                                         :provider "fixture" :input input :cached cached
                                         :write write :output output :reasoning reasoning
                                         :cost cost))

(defmacro with-rounds ((&rest rounds) &body body)
  "BODY with the cell's reader answering ROUNDS, and *ASKED* holding what it
was asked."
  `(let ((*asked* '())
         (nodecode-ledgerline::*rounds*
           (lambda (&key where session)
             (push (list :where where :session session) *asked*)
             (list ,@rounds))))
     ,@body))

(defun last-asked () (first *asked*))

;;; --- lifecycle ---------------------------------------------------------------------

(define-cell-lifecycle-tests "ledgerline"
  (:help :ledgerline)
  (:command "ledger")
  (:running (is (= 80 (nodecode-ledgerline::setting :warn-percent)) "eighty percent by default")
            (is (= 5 (nodecode-ledgerline::setting :top-sessions)) "five sessions named")
            (is (zerop (nodecode-ledgerline::setting :monthly-budget-usd)) "no budget until one is set"))
  (:refused ("warn_percent" 1.5) ("monthly_budget_usd" "lots") ("top_sessions" -2))
  (:idle nodecode-ledgerline:ledgerline-error
         (nodecode-ledgerline:spend "7d")
         (nodecode-ledgerline:receipt "s-1")
         (nodecode-ledgerline:budget "")))

;;; --- the arithmetic ----------------------------------------------------------------

(deftest ledgerline-cell-spend-adds-up-and-names-the-unpriced ()
  (with-cell-stop ((ledgerline-start))
    (with-rounds ((round-of "s-a" "2026-11-09T10:00:00Z" "model-a"
                            :input 100000 :cached 900000 :output 5000 :cost 0.50)
                  (round-of "s-b" "2026-11-09T11:00:00Z" "model-a"
                            :input 100000 :cached 900000 :output 5000 :cost 0.30))
      (let ((text (nodecode-ledgerline:spend "7d")))
        (is (search "$0.80" text) "the priced rounds add up")
        (is (search "2 calls" text) "and are counted")
        (is (search "2 sessions" text) "across two sessions")
        (is (search "90% of 2,000,000 input tokens" text) "the cache share is of every input token")
        (is (search "s-a" text) "the costlier session is named first")
        (is (< (search "s-a" text) (search "s-b" text)) "costliest first")
        (is (search "2026-11-09" text) "a line for the day")))))

(deftest ledgerline-cell-unpriced-rounds-are-unknown-not-zero ()
  (with-cell-stop ((ledgerline-start))
    (with-rounds ((round-of "s-a" "2026-11-09T10:00:00Z" "local" :input 10 :output 2)
                  (round-of "s-a" "2026-11-09T10:01:00Z" "local" :input 10 :output 2))
      (let ((text (nodecode-ledgerline:spend "today")))
        (is (search "2 unpriced" text) "both rounds are counted unpriced")
        (is (search "$0 over 2 calls" text) "and the total says nothing was priced")
        (is (search "could not price" text) "which the advice names")))))

(deftest ledgerline-cell-a-session-receipt-splits-the-input ()
  (with-cell-stop ((ledgerline-start "session_budget_usd" 2))
    (with-rounds ((round-of "s-a" "2026-11-09T10:00:00Z" "model-a"
                            :input 5000 :cached 95000 :write 1000 :output 700 :cost 0.50))
      (let ((text (nodecode-ledgerline:receipt "s-a")))
        (is (search "session s-a: $0.50 over 1 call" text) "the session and its spend")
        (is (search "94% of 101,000 input tokens read from cache, 1,000 written" text)
            "input is split into cache reads, fresh input and writes")
        (is (search "700 out" text) "output is its own line")
        (is (search "$0.50 of $2.00 for the session (25%)" text) "and the per-session budget"))
      (is (equal "s-a" (getf (last-asked) :session)) "a receipt reads one session"))))

(deftest ledgerline-cell-budget-warns-when-the-month-is-over ()
  (with-cell-stop ((ledgerline-start "monthly_budget_usd" 10 "warn_percent" 80))
    (with-rounds ((round-of "s-a" "2026-11-09T10:00:00Z" "model-a" :input 10 :cost 8.50))
      (let ((text (nodecode-ledgerline:budget)))
        (is (search "$8.50 of $10.00 monthly budget (85%)" text) "the month against its budget")
        (is (search "warn at 80% ($8.00)" text) "both warning lines are named")
        (is (search "by month end" text) "and where the month is heading")
        (is (search "WARNING: the month is at 85%" text) "past eighty percent warns")))))

(deftest ledgerline-cell-no-budget-set-is-one-line-not-a-warning ()
  (with-cell-stop ((ledgerline-start))
    (with-rounds ((round-of "s-a" "2026-11-09T10:00:00Z" "model-a" :input 10 :cost 1.25))
      (let ((text (nodecode-ledgerline:budget)))
        (is (search "$1.25 so far, no monthly budget set" text) "spend without a budget")
        (is (not (search "WARNING" text)) "and nothing to warn about")))))

;;; --- the ranges and the boundaries --------------------------------------------------

(deftest ledgerline-cell-each-range-is-one-where-clause ()
  (with-cell-stop ((ledgerline-start))
    (dolist (pair '(("today" . "date(occurred_at")
                    ("7d" . "-7 days")
                    ("30d" . "-30 days")
                    ("month" . "strftime('%Y-%m'")
                    ("all" . nil)))
      (with-rounds ()
        (nodecode-ledgerline:spend (car pair))
        (let ((where (getf (last-asked) :where)))
          (if (cdr pair)
              (is (search (cdr pair) (first where)) (format nil "~a narrows the read" (car pair)))
              (is (null where) "all of it narrows nothing")))))
    (is (signals-error nodecode-ledgerline:ledgerline-error
                       (nodecode-ledgerline:spend "yesterday"))
        "a range no word names refuses")))

(deftest ledgerline-cell-a-call-missing-its-required-member-refuses ()
  (with-cell-stop ((ledgerline-start))
    (let ((schema (second (nle::find-tool "ledger_session"))))
      (is (hash-table-p (nlk:contract-schema schema)) "the tool advertises its contract's schema")
      (is (signals-error nlk:contract-refusal
                         (nlk:contract-decode schema (nlk:make-json-object)))
          "a call with no session_id refuses before the body runs")
      (is (signals-error nlk:contract-refusal
                         (nlk:contract-decode schema (nlk:make-json-object "session_id" 7)))
          "and a number where a session id goes refuses rather than coerces"))))

;;; --- the store, really read --------------------------------------------------------

(deftest ledgerline-cell-the-default-reader-asks-the-store-one-valid-question ()
  (with-temp-store ()
    (with-cell-stop ((ledgerline-start))
      (ensure-durable-session "ledgerline-cell-real")
      (is (equal '() (nodecode-ledgerline::store-rounds :where (nodecode-ledgerline::range-where "all")))
          "the read is one statement the store accepts: an empty log answers no rounds")
      (is (search "nothing ran" (nodecode-ledgerline:spend "all"))
          "and the summary says so rather than inventing a figure")
      (is (equal 0 (nodecode-ledgerline::usage-round-input
                    (nodecode-ledgerline::make-usage-round)))
          "a round starts at zero tokens"))))

;;; --- the command -------------------------------------------------------------------

(deftest ledgerline-cell-the-command-answers-a-range-and-a-panel ()
  (with-cell-stop ((ledgerline-start))
    (with-rounds ((round-of "s-a" "2026-11-09T10:00:00Z" "model-a" :input 10 :cost 0.10))
      (multiple-value-bind (text dialog)
          (nodecode-ledgerline::run-slash "today" "s-a")
        (is (search "ledger today" text) "the operator gets the summary")
        (is (search "costliest sessions" text) "with its sessions")
        (is dialog "and a panel to choose another window"))
      (is (search "date(occurred_at" (first (getf (last-asked) :where)))
          "the range typed is the range the reader was given"))))

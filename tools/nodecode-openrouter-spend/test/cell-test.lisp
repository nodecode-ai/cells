;;;; cell-test.lisp --- lifecycle, the tool, the command, the rate, the record.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring over stubbed endpoints:
;;;; that a start installs the tool, /openrouter-spend and the manual and a
;;;; stop takes every one of them back, that a section naming a variable no
;;;; environment holds refuses, that OpenRouter's counters are parsed and
;;;; shown in both currencies, that a rejected key and an unreachable
;;;; reference rate each say what happened instead of failing the report,
;;;; that a reading is reused until refresh asks again and kept for the
;;;; ledger, and that an option the command does not know is refused.
;;;; Nothing dials: every exchange goes through the cell's own *HTTP*.

(require :sb-posix)

(in-package #:nodecode.test)

(define-test-slice "openrouter-spend" "OPENROUTER-SPEND-CELL-"
  :start nodecode-openrouter-spend:start-cell)

;;; --- the seam ---------------------------------------------------------------

(defvar *asked* ()
  "Every url the stubbed exchange was asked for, newest first.")

(defvar *answers* '()
  "(URL-SUBSTRING STATUS BODY) rows; the last row whose substring the url
contains answers it.")

(defvar *store* nil
  "This test's record file, so no run reads another's readings.")

(defun stub-http (method url &rest args)
  (declare (ignore method args))
  (push url *asked*)
  (let ((row (find-if (lambda (row) (search (first row) url)) *answers* :from-end t)))
    (if row
        (values (third row) (second row) '())
        (values "{}" 404 '()))))

(defun answer (substring status body)
  (push (list substring status body) *answers*))

(defun forget (substring)
  "Drop every answer for SUBSTRING, so the next ANSWER is the only one."
  (setf *answers* (remove substring *answers* :key #'first :test #'equal)))

(defmacro with-env ((name value) &body body)
  "Run BODY with the environment variable NAME set to VALUE, put back after."
  (let ((saved (gensym "SAVED")))
    `(let ((,saved (uiop:getenv ,name)))
       (unwind-protect
            (progn (sb-posix:setenv ,name ,value 1) ,@body)
         (if ,saved (sb-posix:setenv ,name ,saved 1) (sb-posix:unsetenv ,name))))))

(defun fresh-store ()
  "A record file of this test's own, in the scratch area."
  (namestring (merge-pathnames (format nil "openrouter-spend-test-~a.json" (random 1000000))
                                (uiop:temporary-directory))))

(defmacro with-spend (&body body)
  "Run BODY with one OpenRouter key in the environment, every exchange
stubbed, and a record file of its own."
  `(with-env ("OPENROUTER_API_KEY" "test-openrouter-key")
     (let ((nodecode-openrouter-spend::*http* #'stub-http)
           (*asked* '())
           (*answers* '())
           (*store* (fresh-store)))
       (unwind-protect (progn ,@body)
         (ignore-errors (delete-file *store*))))))

(defparameter +key-answer+
  "{\"data\":{\"label\":\"sk-or-v1-abcd\",\"limit\":20.0,\"limit_remaining\":16.6,\"usage\":3.4,\"usage_daily\":0.12,\"usage_weekly\":1.2,\"usage_monthly\":3.4,\"byok_usage\":0.5}}"
  "What OpenRouter's /key answers for the test key: four counters, a budget
with 16.60 of 20.00 left, and a BYOK figure it keeps apart.")

(defparameter +rate-answer+
  "{\"base\":\"USD\",\"quote\":\"INR\",\"rate\":88.5,\"date\":\"2026-11-07\"}"
  "What the public reference-rate endpoint answers.")

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "openrouter-spend"
  (:fixture with-spend)
  (:config "cache_seconds" 0 "history_file" *store*)
  (:help :openrouter-spend)
  (:command "openrouter-spend")
  (:running (is (nle::find-tool "openrouter_spend") "the tool is registered")
            (is (equal "test-openrouter-key" (nodecode-openrouter-spend:credential)) "the section's variable resolves to its key")
            (is (= 16 (length (nodecode-openrouter-spend:fingerprint "test-openrouter-key"))) "a reading is filed under a 16-hex fingerprint, never the key"))
  (:refused ("key_env" "NODECODE_OPENROUTER_SPEND_TEST_UNSET"))
  (:idle nodecode-openrouter-spend:openrouter-spend-error
         (nodecode-openrouter-spend:report))
  (:stopped (is (null (nle::find-tool "openrouter_spend")) "stop unregisters the tool")
            (is (zerop (hash-table-count nodecode-openrouter-spend::*cache*)) "and forgets what it cached")))

;;; --- the tool, through the dispatch seam ----------------------------------------

(deftest openrouter-spend-cell-the-tool-reads-both-currencies ()
  (with-spend
    (with-cell-stop ((openrouter-spend-start "cache_seconds" 0 "history_file" *store*))
      (answer "/key" 200 +key-answer+)
      (answer "frankfurter" 200 +rate-answer+)
      (let ((text (nle::message-content
                   (execute-wire-call "u1" "openrouter_spend" "{\"period\":\"month\"}"))))
        (is (search "OpenRouter API-key spend" text) "the report is named")
        (is (search "Scope: every application using this API key" text) "the scope the provider's counters cover is said plainly")
        (is (search "This month: $3.400000" text) "the period asked for, in the provider's own figures")
        (is (search "/ \u20b9300.90" text) "and the same amount in INR at the reference rate")
        (is (search "Key budget remaining: $16.600000 / \u20b91,469.10 of $20.000000" text) "what is left of the key's budget")
        (is (search "BYOK usage reported separately: $0.500000" text) "and the BYOK figure, kept apart from spend")
        (is (search "2026-11-07" text) "the reference rate's own date")
        (is (not (search "All time:" text)) "one period, not four")
        (is (= 2 (length *asked*)) "one GET for the key, one for the rate")
        (is (find "/key" *asked* :test #'search) "the key's accounting was read too")))))

(deftest openrouter-spend-cell-a-rejected-key-is-said-plainly ()
  (with-spend
    (with-cell-stop ((openrouter-spend-start "cache_seconds" 0 "history_file" *store*))
      (answer "/key" 401 "{}")
      (let ((text (nle::message-content
                   (execute-wire-call "u2" "openrouter_spend" "{}"))))
        (is (search "ERROR: OPENROUTER-SPEND-ERROR" text) "the answer says it failed")
        (is (search "OpenRouter answered HTTP 401" text))
        (is (search "a key it does not know answers 401" text) "and what that usually means")))))

(deftest openrouter-spend-cell-an-unreachable-rate-leaves-the-spend-standing ()
  (with-spend
    (with-cell-stop ((openrouter-spend-start "cache_seconds" 0 "history_file" *store*))
      (answer "/key" 200 +key-answer+)
      (answer "frankfurter" 503 "{}")
      (let ((text (nodecode-openrouter-spend:report :period "today")))
        (is (search "Today: $0.120000" text) "the USD figure survives a failed rate")
        (is (search "INR unavailable" text) "and the INR half says it is missing")
        (is (search "spend is shown in USD only" text))))))

(deftest openrouter-spend-cell-a-manual-rate-costs-no-second-dial ()
  (with-spend
    (with-cell-stop ((openrouter-spend-start "cache_seconds" 0 "history_file" *store*
                                              "usd_inr_rate" "90"))
      (answer "/key" 200 +key-answer+)
      (let ((text (nodecode-openrouter-spend:report :period "today")))
        (is (search "/ \u20b910.80" text) "12 cents at the manual rate")
        (is (search "manual" text) "the line says whose rate it is")
        (is (= 1 (length *asked*)) "one GET only: the rate was not fetched")))))

(deftest openrouter-spend-cell-a-reading-is-reused-until-refresh-asks-again ()
  (with-spend
    (with-cell-stop ((openrouter-spend-start "cache_seconds" 600 "history_file" *store*
                                              "usd_inr_rate" "90"))
      (answer "/key" 200 +key-answer+)
      (is (search "All time: $3.400000" (nodecode-openrouter-spend:report)))
      (is (search "All time: $3.400000" (nodecode-openrouter-spend:report)) "the second answer is the first's")
      (is (= 1 (length *asked*)) "one dial for two answers")
      (nodecode-openrouter-spend:report :refresh t)
      (is (= 2 (length *asked*)) "refresh asks OpenRouter again"))))

(deftest openrouter-spend-cell-a-failed-refresh-answers-the-last-reading-stale ()
  (with-spend
    (with-cell-stop ((openrouter-spend-start "cache_seconds" 0 "history_file" *store*
                                              "usd_inr_rate" "90"))
      (answer "/key" 200 +key-answer+)
      (is (search "Fetched:" (nodecode-openrouter-spend:report :period "total")))
      (forget "/key")
      (answer "/key" 500 "{}")
      (let ((text (nodecode-openrouter-spend:report :period "total")))
        (is (search "STALE - refresh failed" text) "a stale answer says so")
        (is (search "All time: $3.400000" text) "and still shows the last reading")))))

;;; --- the record, and the command -------------------------------------------------

(deftest openrouter-spend-cell-each-reading-is-kept-and-shown-as-a-ledger ()
  (with-spend
    (with-cell-stop ((openrouter-spend-start "cache_seconds" 0 "history_file" *store*
                                              "usd_inr_rate" "90"))
      (answer "/key" 200 +key-answer+)
      (nodecode-openrouter-spend:report)
      (forget "/key")
      (answer "/key" 200 (substitute #\5 #\4 +key-answer+ :count 3))
      (nodecode-openrouter-spend:report)
      (let ((text (nodecode-openrouter-spend::run-command "ledger" nil)))
        (is (search "Local readings - 2 kept" text) "both readings are kept")
        (is (search "$0.100000" text) "and the change between them")
        (is (search "Since the first reading" text) "and the difference between them is shown")
        (is (search "all time $3.400000" text) "the first reading is listed"))
      (let ((status (nodecode-openrouter-spend::run-command "status" nil)))
        (is (search "fingerprint" status) "status names the key's fingerprint")
        (is (search *store* status) "and the file the readings are kept in")
        (is (not (search "test-openrouter-key" status)) "never the key itself")))))

(deftest openrouter-spend-cell-the-command-answers-what-it-knows ()
  (with-spend
    (with-cell-stop ((openrouter-spend-start "cache_seconds" 0 "history_file" *store*
                                              "usd_inr_rate" "90"))
      (answer "/key" 200 +key-answer+)
      (let ((full (nodecode-openrouter-spend::run-command "" nil)))
        (is (search "Today: $0.120000" full) "no argument is every period")
        (is (search "All time: $3.400000" full))
        (is (search "Month so far" full) "with the baseline line"))
      (is (search "This week: $1.200000" (nodecode-openrouter-spend::run-command "week" nil)) "one word is one period")
      (is (search "unknown /openrouter-spend option"
                  (nodecode-openrouter-spend::run-command "nonsense" nil)) "a word it does not know is refused")
      (is (search "Local readings - 2 kept"
                  (nodecode-openrouter-spend::run-command "ledger" nil)) "the ledger counts the two readings those two reports kept"))))

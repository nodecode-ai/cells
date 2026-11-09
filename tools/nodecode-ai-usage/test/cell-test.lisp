;;;; cell-test.lisp --- lifecycle, the tool, the command, the cache, a refused key.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring over stubbed plans: that a
;;;; start installs the tool, /quotas and the manual and a stop takes every
;;;; one of them back, that a section naming no variable that holds a key
;;;; refuses, that each provider's answer is parsed into windows (a percentage
;;;; given, one computed, a balance, a rejection), that a provider with no key
;;;; names the variable to set, that an id it does not read is refused, and
;;;; that an answer is reused until refresh asks again. Nothing dials: every
;;;; exchange goes through the cell's own *HTTP*, and the environment of the
;;;; run holds one key only.

(require :sb-posix)

(in-package #:nodecode.test)

(define-test-slice "ai-usage" "AI-USAGE-CELL-"
  :start nodecode-ai-usage:start-cell)

;;; --- the seam ----------------------------------------------------------------

(defvar *asked* ()
  "Every url the stubbed exchange was asked for, newest first.")

(defvar *answers* '()
  "(URL-SUBSTRING STATUS BODY) rows; the newest row whose substring the url
contains answers it.")

(defun stub-http (method url &rest args)
  (declare (ignore method args))
  (push url *asked*)
  (let ((row (find-if (lambda (row) (search (first row) url)) *answers* :from-end t)))
    (if row
        (values (third row) (second row) '())
        (values "{}" 404 '()))))

(defun answer (substring status body)
  (push (list substring status body) *answers*))

(defmacro with-env ((name value) &body body)
  "Run BODY with the environment variable NAME set to VALUE, put back after."
  (let ((saved (gensym "SAVED")))
    `(let ((,saved (uiop:getenv ,name)))
       (unwind-protect
            (progn (sb-posix:setenv ,name ,value 1) ,@body)
         (if ,saved (sb-posix:setenv ,name ,saved 1) (sb-posix:unsetenv ,name))))))

(defmacro with-usage (&body body)
  "Run BODY with one provider key in the environment and every exchange stubbed."
  `(with-env ("GLM_API_KEY" "test-glm-key")
     (let ((nodecode-ai-usage::*http* #'stub-http)
           (*asked* '())
           (*answers* '()))
       ,@body)))

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "ai-usage"
  (:fixture with-usage)
  (:config "cache_seconds" 0)
  (:help :ai-usage)
  (:command "quotas")
  (:running (is (nle::find-tool "ai_usage") "the tool is registered")
            (is (equal "test-glm-key" (nodecode-ai-usage:credential "zai")) "the section's variable resolves to its key")
            (is (member "zai" (nodecode-ai-usage:provider-ids) :test #'equal)))
  (:refused ("zai_key_env" "NODECODE_AI_USAGE_TEST_UNSET"))
  (:stopped (is (null (nle::find-tool "ai_usage")) "stop unregisters the tool")
            (is (zerop (hash-table-count nodecode-ai-usage::*cache*)) "and forgets what it cached")))

;;; --- the tool, through the dispatch seam ----------------------------------------

(deftest ai-usage-cell-the-tool-parses-a-plan-into-windows ()
  (with-usage
    (with-cell-stop ((ai-usage-start "cache_seconds" 0))
      (answer "quota/limit" 200
              "{\"limits\":[{\"type\":\"TOKENS_LIMIT\",\"percentage\":25,\"remaining\":\"75 left\"},{\"type\":\"TIME_LIMIT\",\"currentValue\":3,\"total\":10}]}")
      (let ((text (nle::message-content
                   (execute-wire-call "u1" "ai_usage" "{\"provider\":\"zai\"}"))))
        (is (search "zai (Z.AI / GLM)" text) "the provider is named")
        (is (search "Session (5h): 25% used, 75% left" text) "a given percentage reads as a window")
        (is (search "Tool quota: 30% used, 70% left" text) "one without a percentage is computed from its counts")
        (is (search "75 left" text) "and the plan's own detail rides along")
        (is (search "1 provider" text) "one provider was read")))))

(deftest ai-usage-cell-a-balance-is-a-detail-not-a-window ()
  (with-usage
    (with-cell-stop ((ai-usage-start "cache_seconds" 0))
      (with-env ("DEEPSEEK_API_KEY" "test-deepseek-key")
        (answer "user/balance" 200
                "{\"balance_infos\":[{\"currency\":\"USD\",\"total_balance\":\"12.50\"}]}")
        (let ((text (nle::message-content
                     (execute-wire-call "u2" "ai_usage" "{\"provider\":\"deepseek\"}"))))
          (is (search "DeepSeek balance" text))
          (is (search "USD 12.50 balance" text) "the balance reads as money"))))))

(deftest ai-usage-cell-a-provider-without-a-key-says-what-to-set ()
  (with-usage
    (with-cell-stop ((ai-usage-start "cache_seconds" 0))
      (let ((text (nle::message-content
                   (execute-wire-call "u3" "ai_usage" "{\"provider\":\"deepseek\"}"))))
        (is (search "no DeepSeek key" text))
        (is (search "DEEPSEEK_API_KEY" text) "the variable to export is named")
        (is (null *asked*) "and nothing was dialed")))))

(deftest ai-usage-cell-an-id-it-does-not-read-is-refused ()
  (with-usage
    (with-cell-stop ((ai-usage-start "cache_seconds" 0))
      (let ((text (nle::message-content
                   (execute-wire-call "u4" "ai_usage" "{\"provider\":\"gemini\"}"))))
        (is (search "ERROR: AI-USAGE-ERROR" text) "the answer says it failed")
        (is (search "not a provider this cell reads" text))
        (is (search "copilot" text) "and lists the ones it does")))))

(deftest ai-usage-cell-a-rejected-key-is-said-plainly ()
  (with-usage
    (with-cell-stop ((ai-usage-start "cache_seconds" 0))
      (answer "quota/limit" 401 "{}")
      (is (search "Z.AI rejected the key (HTTP 401)"
                  (nodecode-ai-usage:report :provider "zai"))))))

;;; --- the cache, and the command -------------------------------------------------

(deftest ai-usage-cell-an-answer-is-reused-until-refresh-asks-again ()
  (with-usage
    (with-cell-stop ((ai-usage-start "cache_seconds" 600))
      (answer "quota/limit" 200 "{\"limits\":[{\"type\":\"TOKENS_LIMIT\",\"percentage\":10}]}")
      (is (search "10% used" (nodecode-ai-usage:report :provider "zai")))
      (is (search "10% used" (nodecode-ai-usage:report :provider "zai")) "the second answer is the first's")
      (is (= 1 (length *asked*)) "one dial for two answers")
      (nodecode-ai-usage:report :provider "zai" :refresh t)
      (is (= 2 (length *asked*)) "refresh asks the plan again"))))

(deftest ai-usage-cell-the-command-prints-the-report ()
  (with-usage
    (with-env ("MINIMAX_API_KEY" "test-minimax-key")
    (with-cell-stop ((ai-usage-start "cache_seconds" 0))
      (answer "openplatform/coding_plan/remains" 200
              "{\"model_remains\":[{\"model_name\":\"minimax-m2\",\"current_interval_total_count\":100,\"current_interval_usage_count\":40}]}")
      (let ((text (nodecode-ai-usage::run-command "minimax" nil)))
        (is (search "MiniMax coding plan limits" text))
        (is (search "minimax-m2 . 5h: 60% used, 40% left" text) "a model's 5h window is one row"))
      (is (search "asked just now" (nodecode-ai-usage::run-command "refresh" nil)) "refresh asks the plans again")
      (is (search "not a provider" (nodecode-ai-usage::run-command "nope" nil)) "a word it does not know is refused")
      (let ((full (nodecode-ai-usage::run-command "" nil)))
        (is (search "zai (Z.AI / GLM)" full) "no argument is the whole report")
        (is (not (search "copilot" full)) "and leaves out the providers with no key"))))))

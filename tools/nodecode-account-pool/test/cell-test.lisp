;;;; cell-test.lisp --- lifecycle, both providers' usage, best, park, wake.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring over stubbed vendors: that
;;;; a start installs the account_pool tool, /account-pool and the manual and
;;;; a stop takes every one of them back, that a section naming no account,
;;;; or no account whose variable holds a token, refuses, that a Claude and a
;;;; Codex answer are parsed into windows, that best picks the account with
;;;; the most headroom and leaves a parked one out, that a park survives in
;;;; the state file, that a wake is one tiny POST, and that an account with
;;;; no token and a label the pool does not hold each say why. Nothing dials:
;;;; every exchange goes through the cell's own *HTTP*.

(require :sb-posix)

(in-package #:nodecode.test)

(define-test-slice "account-pool" "ACCOUNT-POOL-CELL-"
  :start nodecode-account-pool:start-cell)

;;; --- the seam ----------------------------------------------------------------

(defvar *calls* '()
  "Every exchange the stubbed seam was asked for, newest first, as a plist.")

(defvar *answers* '()
  "(URL-SUBSTRING STATUS BODY) rows; the newest row whose substring the url
contains answers it.")

(defun stub-http (method url &rest args)
  (push (list :method method :url (princ-to-string url)
              :content (getf args :content) :headers (getf args :headers))
        *calls*)
  (let ((row (find-if (lambda (row) (search (first row) (princ-to-string url)))
                       *answers* :from-end t)))
    (if row
        (values (third row) (second row) '())
        (values "{}" 404 '()))))

(defun pool-answer (substring status body)
  (push (list substring status body) *answers*))

(defun pool-posted (substring)
  "The newest exchange that POSTed to a url holding SUBSTRING, or NIL."
  (find-if (lambda (call) (and (eql :post (getf call :method))
                               (search substring (getf call :url))))
           *calls*))

(defmacro with-env ((name value) &body body)
  "Run BODY with the environment variable NAME set to VALUE, put back after."
  (let ((saved (gensym "SAVED")))
    `(let ((,saved (uiop:getenv ,name)))
       (unwind-protect
            (progn (sb-posix:setenv ,name ,value 1) ,@body)
         (if ,saved (sb-posix:setenv ,name ,saved 1) (sb-posix:unsetenv ,name))))))

(defparameter +claude-entry+ "claude-main:anthropic:NODECODE_ACCOUNTS_CLAUDE_TOKEN")
(defparameter +codex-entry+ "codex-main:openai-codex:NODECODE_ACCOUNTS_CODEX_TOKEN")

(defmacro with-pool (&body body)
  "Run BODY with both accounts' tokens in the environment and every exchange stubbed."
  `(with-env ("NODECODE_ACCOUNTS_CLAUDE_TOKEN" "test-claude-token")
     (with-env ("NODECODE_ACCOUNTS_CODEX_TOKEN" "test-codex-token")
       (let ((nodecode-account-pool::*http* #'stub-http)
             (*calls* '())
             (*answers* '()))
         ,@body))))

(defun tool-text (call-id json)
  (nle::message-content (execute-wire-call call-id "account_pool" json)))

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "account-pool"
  (:fixture with-pool)
  (:config "accounts" #("claude-main:anthropic:NODECODE_ACCOUNTS_CLAUDE_TOKEN"
                        "codex-main:openai-codex:NODECODE_ACCOUNTS_CODEX_TOKEN")
           "cache_seconds" 0)
  (:help :account-pool)
  (:command "account-pool")
  (:running (is (nle::find-tool "account_pool") "the tool is registered")
            (is (equal '("claude-main" "codex-main") (nodecode-account-pool:account-labels)) "both accounts are read from the section"))
  (:refused ("accounts" #())
            ("accounts" (vector "solo:anthropic:NODECODE_ACCOUNT_POOL_TEST_UNSET")))
  (:stopped (is (null (nle::find-tool "account_pool")) "stop unregisters the tool")
            (is (zerop (hash-table-count nodecode-account-pool::*cache*)) "and forgets what it cached")))

;;; --- the usage both providers answer --------------------------------------------

(deftest account-pool-cell-a-claude-answer-reads-as-windows ()
  (with-pool
    (with-cell-stop ((account-pool-start "accounts" (vector +claude-entry+) "cache_seconds" 0))
      (pool-answer "api/oauth/usage" 200
              "{\"five_hour\":{\"utilization\":42.5,\"resets_at\":\"2026-11-10T22:00:00Z\"},\"seven_day\":{\"utilization\":10}}")
      (let ((text (tool-text "u1" "{\"action\":\"report\"}")))
        (is (search "claude-main (Claude)" text) "the account and its provider are named")
        (is (search "5-hour: 42% used, 58% left" text) "the short window reads as one")
        (is (search "resets 2026-11-10T22:00:00Z" text) "and its reset time rides along")
        (is (search "Weekly: 10% used, 90% left" text) "so does the weekly one")
        (is (search "in rotation" text) "an unparked account is in the rotation")))))

(deftest account-pool-cell-a-codex-answer-reads-as-windows ()
  (with-pool
    (with-cell-stop ((account-pool-start "accounts" (vector +codex-entry+) "cache_seconds" 0))
      (pool-answer "wham/usage" 200
              "{\"rate_limit\":{\"primary_window\":{\"used_percent\":30,\"limit_window_seconds\":3600},\"secondary_window\":{\"used_percent\":80,\"limit_window_seconds\":604800}}}")
      (let ((text (tool-text "u2" "{\"action\":\"report\"}")))
        (is (search "codex-main (Codex)" text))
        (is (search "1h window: 30% used, 70% left" text))
        (is (search "Weekly: 80% used, 20% left" text))
        (is (search "1h window" text) "a window that is not five hours is named by its length")))))

(deftest account-pool-cell-a-reached-limit-reads-as-a-full-window ()
  (with-pool
    (with-cell-stop ((account-pool-start "accounts" (vector +codex-entry+) "cache_seconds" 0))
      (pool-answer "wham/usage" 200
              "{\"rate_limit\":{\"primary_window\":{\"used_percent\":60,\"limit_window_seconds\":18000},\"limit_reached\":true}}")
      (let ((text (tool-text "u3" "{\"action\":\"report\"}")))
        (is (search "5-hour: 100% used, 0% left" text) "a reached limit is a full window")
        (is (search "the limit is reached" text))))))

;;; --- best, park, unpark ----------------------------------------------------------

(deftest account-pool-cell-best-picks-the-most-headroom ()
  (with-pool
    (with-cell-stop ((account-pool-start "accounts" (vector +claude-entry+ +codex-entry+) "cache_seconds" 0))
      (pool-answer "api/oauth/usage" 200 "{\"five_hour\":{\"utilization\":20}}")
      (pool-answer "wham/usage" 200 "{\"rate_limit\":{\"primary_window\":{\"used_percent\":70}}}")
      (let ((text (tool-text "u4" "{\"action\":\"best\"}")))
        (is (search "Use claude-main: 20% used, 80% left" text) "the least used account wins")
        (is (search "next: codex-main at 70% used" text) "and the one behind it is named")
        (is (search "2 accounts in the rotation" text))))))

(deftest account-pool-cell-a-park-leaves-an-account-out ()
  (with-pool
    (with-cell-stop ((account-pool-start "accounts" (vector +claude-entry+ +codex-entry+) "cache_seconds" 0))
      (pool-answer "api/oauth/usage" 200 "{\"five_hour\":{\"utilization\":20}}")
      (pool-answer "wham/usage" 200 "{\"rate_limit\":{\"primary_window\":{\"used_percent\":70}}}")
      (is (search "claude-main is parked" (tool-text "u5" "{\"action\":\"park\",\"account\":\"claude-main\"}")))
      (is (search "Use codex-main" (tool-text "u6" "{\"action\":\"best\"}")) "the parked account is passed over")
      (let ((text (tool-text "u7" "{\"action\":\"report\"}")))
        (is (search "claude-main (Claude) - 20% used, 80% left - parked" text)
            (format nil "and the report says why: ~a" text)))
      (let ((state (nodecode-account-pool::state-path)))
        (is (probe-file state) "the park is written down")
        (is (search "claude-main" (uiop:read-file-string state)) "under its own label"))
      (is (search "claude-main is back in the rotation"
                  (tool-text "u8" "{\"action\":\"unpark\",\"account\":\"claude-main\"}")))
      (is (search "Use claude-main" (tool-text "u9" "{\"action\":\"best\"}")) "and it is back"))))

(deftest account-pool-cell-a-park-survives-a-restart ()
  (with-pool
    (with-cell-stop ((account-pool-start "accounts" (vector +claude-entry+ +codex-entry+) "cache_seconds" 0))
      (nodecode-account-pool:park "codex-main"))
    (with-cell-stop ((account-pool-start "accounts" (vector +claude-entry+ +codex-entry+) "cache_seconds" 0))
      (is (search "codex-main (Codex)" (nodecode-account-pool:report)) "the account is read back")
      (is (search "parked" (nodecode-account-pool:report)) "with its park, from the file"))))

;;; --- one wake, and two refusals ---------------------------------------------------

(deftest account-pool-cell-a-wake-is-one-tiny-request ()
  (with-pool
    (with-cell-stop ((account-pool-start "accounts" (vector +claude-entry+) "cache_seconds" 0))
      (pool-answer "v1/messages" 200 "{\"id\":\"msg_1\"}")
      (let ((text (tool-text "u10" "{\"action\":\"wake\",\"account\":\"claude-main\"}"))
            (call (pool-posted "v1/messages")))
        (is (search "woke claude-main" text))
        (is (search "haiku" text) "the section's wake model is named")
        (let ((content (and call (getf call :content))))
          (is (and content (search "\"max_tokens\":1" content)) "one token, not a conversation")
          (is (and content (search "Claude Code" content)) "and the Claude Code system line"))
        (is (search "Bearer test-claude-token" (cdr (assoc "authorization" (getf call :headers) :test #'equal))) "with the account's own token")))))

(deftest account-pool-cell-an-account-without-a-token-says-what-to-export ()
  (with-pool
    (with-cell-stop ((account-pool-start "accounts"
                                         (vector "solo:anthropic:NODECODE_ACCOUNT_POOL_TEST_UNSET"
                                           +claude-entry+)
                                         "cache_seconds" 0))
      (pool-answer "api/oauth/usage" 200 "{\"five_hour\":{\"utilization\":5}}")
      (let ((text (nodecode-account-pool:report :account "solo")))
        (is (search "solo has no token" text))
        (is (search "export NODECODE_ACCOUNT_POOL_TEST_UNSET" text) "the variable to set is named")))))

(deftest account-pool-cell-a-label-the-pool-does-not-hold-is-refused ()
  (with-pool
    (with-cell-stop ((account-pool-start "accounts" (vector +claude-entry+) "cache_seconds" 0))
      (let ((text (tool-text "u11" "{\"action\":\"park\",\"account\":\"nope\"}")))
        (is (search "ERROR: ACCOUNT-POOL-ERROR" text) "the answer says it failed")
        (is (search "nope is not an account this pool holds" text))
        (is (search "claude-main" text) "and lists the ones it does")))))

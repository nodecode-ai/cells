;;;; cell-test.lisp --- lifecycle, the three tools and /penny, against a stubbed wire.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every request is a stubbed NLK:HTTP, so nothing reaches Penny, needs a
;;;; token or touches the network. What is proved is the cell's own wiring:
;;;; the section (a healthy start, a wrong value and a missing token refused,
;;;; a token read from a file), the MCP exchange it speaks (initialize, the
;;;; notification, then Penny's own tool), the three tools through the seam
;;;; the model calls them by -- their name, arguments, headers and answer --
;;;; and the operator's command.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *reply* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS => (values TEXT STATUS HEADERS).")

(defmacro with-penny-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *REQUESTS* and answered
by *REPLY*, so no test dials Penny."
  `(let ((*requests* '())
         (*reply* (lambda (method url args)
                    (declare (ignore method url args))
                    (error "unexpected penny request"))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *requests*)
                                (funcall *reply* method url args))
       ,@body)))

(defun penny-asked ()
  "The stubbed requests, oldest first."
  (reverse *requests*))

(defun penny-body (request)
  "REQUEST's JSON-RPC body, decoded."
  (let ((content (getf (cddr request) :content)))
    (and (stringp content) (nlk:decode-json content))))

(defun penny-rpc (request)
  "The JSON-RPC method REQUEST carried."
  (nlk:json-value (penny-body request) :text "method"))

(defun penny-calls ()
  "The stubbed tools/call requests, oldest first."
  (remove-if-not (lambda (request) (equal "tools/call" (penny-rpc request))) (penny-asked)))

(defun penny-arguments (request)
  "The arguments object a stubbed tools/call REQUEST carried."
  (nlk:json-value (penny-body request) :object "params" "arguments"))

(defun penny-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun penny-headers (&optional (content-type "application/json") session)
  "A decoded header table, as NLK:HTTP answers one."
  (let ((table (make-hash-table :test 'equal)))
    (when content-type (setf (gethash "content-type" table) content-type))
    (when session (setf (gethash "mcp-session-id" table) session))
    table))

(defun penny-serving (value-text)
  "A Penny that answers the handshake and gives VALUE-TEXT -- a JSON text --
as the value of any tool call."
  (setf *reply*
        (lambda (method url args)
          (declare (ignore method url))
          (let* ((body (nlk:decode-json (getf args :content)))
                 (rpc (nlk:json-value body :text "method"))
                 (id (nlk:json-value body :integer "id")))
            (cond
              ((equal rpc "initialize")
               (values "{\"jsonrpc\": \"2.0\", \"id\": 1, \"result\": {\"protocolVersion\": \"2025-03-26\"}}"
                       200 (penny-headers "application/json" "session-1")))
              ((equal rpc "notifications/initialized")
               (values "" 202 (penny-headers)))
              ((equal rpc "tools/call")
               (values (format nil "{\"jsonrpc\": \"2.0\", \"id\": ~d, \"result\": {\"structuredContent\": {\"value\": ~a}}}"
                               id value-text)
                       200 (penny-headers "application/json" "session-1")))
              (t (error "unexpected JSON-RPC method ~a" rpc)))))))

(defun penny-call (name arguments)
  "NAME called the way the model calls it, as the text the model reads back."
  (nle::message-content (execute-wire-call "penny-call" name arguments)))

(defparameter +core-memory+ "[\"Prefers short answers.\", \"Runs Debian on the laptop.\"]"
  "A core memory answer: Penny's own array of memory lines.")

;;; --- lifecycle -----------------------------------------------------------------

(define-test-slice "penny" "PENNY-CELL-" :start nodecode-penny:start-cell)

(define-cell-lifecycle-tests "penny"
  (:fixture with-penny-wire)
  (:config "token" "penny-token")
  (:help :penny)
  (:command "penny")
  (:running
   (is (nle::find-tool "penny_profile") "profile is registered")
   (is (nle::find-tool "penny_search") "and search")
   (is (nle::find-tool "penny_remember") "and remember")
   (is (= 3 (count-if (lambda (entry) (eql 0 (search "penny_" (first entry)))) nle::*tools*)) "three of them, and no others"))
  (:stopped
   (is (null (nle::find-tool "penny_profile")) "and stop takes every one back")
   (is (null (nle::find-tool "penny_remember"))))
  (:refused ("timeout_seconds" 0)
            ("default_limit" 0)
            ("max_chars" 10)))

(deftest penny-cell-a-start-with-no-token-refuses-and-installs-nothing (with-penny-wire)
  (with-stubbed-fdefinition (uiop:getenv (name) nil)
    (let ((text (refusal-text nlk:config-refusal (penny-start))))
      (is-carrying text "needs a Penny access token" "token_env")
      (is (null (nle::find-tool "penny_profile")) "nothing was installed"))))

(deftest penny-cell-an-api-key-is-refused-as-not-a-token (with-penny-wire)
  (let ((text (refusal-text nlk:config-refusal (penny-start "token" "mpt_live_key"))))
    (is-carrying text "mpt_" "not the access token")))

(deftest penny-cell-the-token-may-come-from-a-file (with-penny-wire)
  (let ((path (format nil "~apenny-auth-~d.json" (uiop:temporary-directory) (random 1000000))))
    (unwind-protect
         (progn
           (with-open-file (stream path :direction :output :if-exists :supersede)
             (write-string "{\"tokens\": {\"access_token\": \"file-token\"}}" stream))
           (penny-serving +core-memory+)
           (with-cell-stop ((penny-start "token_file" path))
             (let ((answer (penny-call "penny_profile" "{}")))
               (is (search "Prefers short answers" answer) "the token file answered")
               (is (not (search "file-token" answer)) "and the token never rides an answer")))
           (is (equal "Bearer file-token" (penny-header (first (penny-asked)) "authorization")) "the token the file held is the one sent"))
      (ignore-errors (delete-file path)))))

;;; --- the tools -------------------------------------------------------------------

(deftest penny-cell-profile-handshakes-then-reads-the-core-memory (with-penny-wire)
  (with-cell-stop ((penny-start "token" "penny-token"))
    (penny-serving +core-memory+)
    (let ((answer (penny-call "penny_profile" "{}"))
          (requests (penny-asked)))
      (is (equal '("initialize" "notifications/initialized" "tools/call")
                 (mapcar #'penny-rpc requests)) "one call is initialize, initialized, tools/call")
      (is (equal "https://app.mypenny.ai/mcp" (second (first requests))) "to Penny's endpoint")
      (is (equal "Bearer penny-token" (penny-header (first requests) "authorization")) "with the section's token")
      (is (equal "2025-03-26" (penny-header (first requests) "mcp-protocol-version")) "at Penny's MCP revision")
      (is (equal "session-1" (penny-header (third requests) "mcp-session-id")) "and the session id initialize issued")
      (is (equal "penny_read" (nlk:json-value (penny-body (first (penny-calls))) :text "params" "name")) "the profile is Penny's own read")
      (is (equal "profile" (nlk:json-value (penny-arguments (first (penny-calls))) :text "target")) "of the profile target")
      (is (search "1. Prefers short answers." answer) "the memory comes back numbered")
      (is (search "2. Runs Debian on the laptop." answer) "every line of it"))))

(deftest penny-cell-search-sends-the-query-and-its-limit (with-penny-wire)
  (with-cell-stop ((penny-start "token" "penny-token"))
    (penny-serving "{\"results\": [{\"content\": \"Deploy is one command.\", \"tags\": [\"deploy\"], \"confidence\": 0.8, \"scope\": {\"kind\": \"private\"}}]}")
    (let ((answer (penny-call "penny_search" "{\"query\": \"how do we deploy\", \"limit\": 3}")))
      (let ((arguments (penny-arguments (first (penny-calls)))))
        (is (equal "search" (nlk:json-value arguments :text "target")))
        (is (equal "how do we deploy" (nlk:json-value arguments :text "query")) "the query is sent")
        (is (= 3 (nlk:json-value arguments :integer "limit")) "and the limit the call named"))
      (is (search "Deploy is one command." answer) "the note comes back")
      (is (search "tags: deploy" answer) "with its tags")
      (is (search "confidence 0.80" answer) "and how sure it is")
      (is (search "[private]" answer) "and that it is a private note")))
  (with-cell-stop ((penny-start "token" "penny-token"))
    (penny-serving "[]")
    (let ((answer (penny-call "penny_search" "{\"query\": \"anything\"}")))
      (is (search "no note matched" answer) "an empty answer says so")
      (is (= 5 (nlk:json-value (penny-arguments (car (last (penny-calls)))) :integer "limit")) "five is the limit a call that names none gets"))))

(deftest penny-cell-remember-sends-a-note-and-reads-its-receipt (with-penny-wire)
  (with-cell-stop ((penny-start "token" "penny-token"))
    (penny-serving "{\"ok\": [\"mem-1\"], \"failed\": [], \"receipts\": {\"mem-1\": {\"status\": \"saved\", \"memoryId\": \"mem-1\"}}}")
    (let ((answer (penny-call "penny_remember"
                              "{\"content\": \"The staging box reboots nightly.\", \"tags\": [\"ops\"], \"confidence\": 0.9, \"sample_questions\": [\"When does staging reboot?\", \"Why is staging down?\", \"Who owns staging?\"]}")))
      (let* ((request (first (penny-calls)))
             (arguments (penny-arguments request))
             (note (aref (nlk:json-value arguments :array "notes") 0)))
        (is (equal "penny_write" (nlk:json-value (penny-body request) :text "params" "name")))
        (is (equal "note" (nlk:json-value arguments :text "entityType")) "the write is a note")
        (is (equal "The staging box reboots nightly." (nlk:json-value note :text "content")))
        (is (= 3 (length (nlk:json-value note :array "sampleQuestions"))) "with its three questions")
        (is (equal "ops" (aref (nlk:json-value note :array "tags") 0)))
        (is (= 0.9 (nlk:json-value note :number "confidence")))
        (is (equal "nodecode" (nlk:json-value note :text "source")) "and where it came from"))
      (is (search "saved to Penny private notes: mem-1" answer) "the receipt is what a save answers"))))

(deftest penny-cell-remember-refuses-what-penny-would (with-penny-wire)
  (with-cell-stop ((penny-start "token" "penny-token"))
    (penny-serving "{\"ok\": [\"mem-1\"], \"failed\": []}")
    (let ((answer (penny-call "penny_remember" "{\"content\": \"x\", \"confidence\": 0.5, \"sample_questions\": [\"one\", \"two\"]}")))
      (is (search "exactly three sample questions" answer) "two questions are refused, not sent")
      (is (null (penny-calls)) "and nothing reached Penny"))
    (penny-serving "{\"ok\": [], \"failed\": [{\"content\": \"x\"}]}")
    (is (search "did not confirm the save" (penny-call "penny_remember" "{\"content\": \"x\", \"confidence\": 0.5, \"sample_questions\": [\"a\", \"b\", \"c\"]}")) "an unconfirmed save says so")))

;;; --- the operator's command --------------------------------------------------------

(deftest penny-cell-the-command-says-the-connection-and-the-memory (with-penny-wire)
  (with-cell-stop ((penny-start "token" "penny-token"))
    (penny-serving +core-memory+)
    (let ((answer (cell-entry "nodecode-penny" "penny" "")))
      (is (search "token set" answer) "the command says the token is there")
      (is (search "https://app.mypenny.ai/mcp" answer) "and which endpoint it talks to")
      (is (search "Prefers short answers" answer) "and lays the core memory out"))
    (let ((answer (cell-entry "nodecode-penny" "penny" "how do we deploy")))
      (is (equal "how do we deploy" (nlk:json-value (penny-arguments (car (last (penny-calls)))) :text "query")) "a query typed after the command is one search"))))


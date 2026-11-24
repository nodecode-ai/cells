;;;; cell-test.lisp --- the bankbridge cell against a stubbed server.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every request is a stubbed NLK:HTTP and the key a stubbed
;;;; NLE::CREDENTIAL-ENV: nothing reaches BankBridge. A call is made the way a
;;;; model's call reaches it -- through the real dispatch and the real tool
;;;; schema -- so the schema the provider is shown and the body that runs are
;;;; one shape.

(in-package #:nodecode.test)

(define-test-slice "bankbridge" "BANKBRIDGE-CELL-" :start nodecode-bankbridge:start-cell
  :defaults (("api_key_env" "BANKBRIDGE_API_KEY")))

;;; --- fixtures --------------------------------------------------------------------

(defparameter *requests* nil
  "Every request the stub answered, oldest first, as (URL HEADERS MESSAGE).")

(defparameter *answers* nil
  "What the stub answers a tools/call with: the text, or (text . :error).")

(defparameter *tool-list* '("list_accounts" "get_spending_summary" "get_spending_summary"))

(defmacro with-bankbridge (&body body)
  "BODY with a key answering and the request log emptied."
  `(let ((saved *requests*))
     (setf *requests* '())
     (nlk:with-cleanup ((setf *requests* saved))
       (with-stubbed-fdefinition (nle::credential-env (name)
                                   (and (equal name "BANKBRIDGE_API_KEY") "bbk-test"))
         ,@body))))

(defun json-text (object) (nlk:encode-json-object object))

(defmacro with-bankbridge-wire (&body body)
  "BODY with NLK:HTTP answering the three messages a call takes."
  `(with-stubbed-fdefinition
       (nlk:http (method target &rest options)
         (declare (ignore method))
         (let* ((message (nlk:decode-json (getf options :content)))
                (rpc (nlk:json-value message :string "method"))
                (headers (let ((table (make-hash-table :test #'equal)))
                           (setf (gethash "content-type" table) "application/json"
                                 (gethash "mcp-session-id" table) "session-1")
                           table)))
           (push (list target (getf options :headers) message) *requests*)
           (cond ((equal rpc "initialize")
                  (values (json-text (nlk:json-object "jsonrpc" "2.0" "id" 1
                                                       "result" (nlk:json-object "protocolVersion" "2025-06-18")))
                          200 headers))
                 ((equal rpc "notifications/initialized") (values "" 202 headers))
                 ((equal rpc "tools/list")
                  (values (json-text (nlk:json-object
                                      "jsonrpc" "2.0" "id" 2
                                      "result" (nlk:json-object
                                                "tools" (coerce (mapcar (lambda (name)
                                                                           (nlk:json-object "name" name))
                                                                         *tool-list*)
                                                                'vector))))
                          200 headers))
                 ((equal rpc "tools/call")
                  (let ((answer (or *answers* "{\"accounts\":[]}")))
                    (values (json-text (nlk:json-object
                                        "jsonrpc" "2.0" "id" 2
                                        "result" (nlk:json-object
                                                  "content" (vector (nlk:json-object "type" "text" "text" answer))
                                                  "isError" (if (consp answer) t nil))))
                            200 headers)))
                 (t (values "{}" 200 headers)))))
     (with-bankbridge ,@body)))

(defun tool-answer (id name arguments)
  "The tool's answer as the model reads it, through the real dispatch."
  (nle::message-content (execute-wire-call id name arguments)))

(defun call-message ()
  "The tools/call message the stub was handed."
  (third (find "tools/call" *requests* :key (lambda (request) (nlk:json-value (third request) :string "method"))
                                :test #'equal)))

;;; --- the lifecycle, from the one declaration ----------------------------------------

(define-cell-lifecycle-tests "bankbridge"
  (:config)
  (:help :bankbridge)
  (:command "bankbridge")
  (:fixture with-bankbridge)
  (:refused ("timeout" 0) ("result_chars" 10)))

(deftest bankbridge-cell-without-a-key-refuses ()
  (with-stubbed-fdefinition (nle::credential-env (name) (declare (ignore name)) nil)
    (let ((condition (signals-error nlk:config-refusal (bankbridge-start))))
      (is (search "BANKBRIDGE_API_KEY" (princ-to-string condition)) "the refusal names the unset variable")
      (is (null (nle::find-tool "bankbridge_list_accounts")) "and installs nothing"))))

(deftest bankbridge-cell-the-19-money-reviews-are-commands-while-it-runs (with-bankbridge)
  (with-cell-stop ((bankbridge-start))
    (is (= 19 (loop for entry in nodecode-bankbridge::*prompts*
                           when (nle::find-registered-command (first entry)) count entry)) "every one of the original's 19 slash commands is registered")
    (is (equal "nodecode-bankbridge"
               (nle::slash-command-owner (nle::find-registered-command "subscriptions")))))
  (is (null (nle::find-registered-command "subscriptions")) "and a stop takes them out"))

;;; --- the tool ---------------------------------------------------------------------

(deftest bankbridge-cell-a-tool-calls-the-server-with-its-arguments (with-bankbridge-wire)
  (with-cell-stop ((bankbridge-start))
    (let ((answer (tool-answer "call-1" "bankbridge_get_spending_summary" "{\"month\":\"2026-03\",\"group_by\":\"category\"}")))
      (is (equal "{\"accounts\":[]}" answer) (format nil "the server's text is the answer: ~a" answer))
      (is (= 3 (length *requests*)) "initialize, initialized, the call")
      (let ((message (call-message)))
        (is (equal "get_spending_summary" (nlk:json-value (nlk:json-value message :object "params") :string "name"))))
      (let ((params (nlk:json-value (call-message) :object "params")))
        (is (equal "2026-03" (nlk:json-value (nlk:json-value params :object "arguments") :string "month")) "the argument reaches the server unchanged"))
      (let ((headers (second (first *requests*))))
        (is (equal "Bearer bbk-test" (cdr (assoc "authorization" headers :test #'equal))) "the key rides the one endpoint's request")
        (is (equal "https://bankbridge.money/api/mcp" (first (first *requests*))))))))

(deftest bankbridge-cell-a-server-error-is-a-failed-call (with-bankbridge-wire)
  (with-cell-stop ((bankbridge-start))
    (setf *answers* '("the bank needs reconnecting" . :error))
    (is (search "ERROR:" (tool-answer "call-2" "bankbridge_list_accounts" "{}")) "an isError result is a failed call, not an answer")))

(deftest bankbridge-cell-a-refused-key-says-which-variable (with-bankbridge)
  (with-cell-stop ((bankbridge-start))
    (with-stubbed-fdefinition (nlk:http (method target &rest options)
                                (declare (ignore method target options))
                                (values "{\"error\":\"invalid token\"}" 401))
      (let ((answer (nodecode-bankbridge:call "list_accounts")))
        (is (search "HTTP 401" answer))
        (is (search "api_key_env" answer) "the refusal names the section's variable")))))

(deftest bankbridge-cell-an-unknown-tool-is-refused-by-the-cell (with-bankbridge)
  (with-cell-stop ((bankbridge-start))
    (is (search "ERROR:" (nodecode-bankbridge:call "get_everything")))
    (with-bankbridge-wire
      (is (null *requests*) "and nothing left the machine"))))

;;; --- the command --------------------------------------------------------------------

(deftest bankbridge-cell-the-command-answers-through-stubbed-io (with-bankbridge-wire)
  (with-cell-stop ((bankbridge-start))
    (let ((line (cell-entry "nodecode-bankbridge" "bankbridge" "")))
      (is (search "bankbridge.money/api/mcp" line) "the status names the endpoint")
      (is (search "BANKBRIDGE_API_KEY" line) "and the variable the key is read from")
      (is (search "/subscriptions" line) "and the money reviews it adds")
      (is (null *requests*) "status sends nothing"))
    (let ((checked (cell-entry "nodecode-bankbridge" "bankbridge" "check")))
      (is (search "tools" checked) (format nil "check proves the key with one listing: ~a" checked)))
    (is (search "not one of its words" (cell-entry "nodecode-bankbridge" "bankbridge" "wobble")))))

(deftest bankbridge-cell-a-money-review-submits-its-prompt (with-bankbridge-wire)
  (with-cell-stop ((bankbridge-start))
    (let ((submitted '()))
      (with-stubbed-fdefinition (nle:submit (session text &rest keys)
                                  (push (list session text keys) submitted)
                                  :started)
        (let ((line (cell-entry "nodecode-bankbridge" "monthly-report" "2026-03")))
          (is (search "monthly-report" line))
          (is (= 1 (length submitted)))
          (is (search "2026-03" (second (first submitted))) "$ARGUMENTS is the argument typed")
          (is (null (search "$ARGUMENTS" (second (first submitted)))) "and the placeholder is gone"))))))

(deftest bankbridge-cell-a-verb-refuses-while-not-running ()
  (with-bankbridge-wire
    (setf nodecode-bankbridge::*bankbridge* nil)
    (is (search "not running" (bankbridge:call "list_accounts")) "the refusal says the cell is not on")
    (is (null *requests*) "and nothing left the machine")))

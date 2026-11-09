;;;; cell-test.lisp --- the cell against a stubbed Adwave MCP server.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every request is a stubbed NLK:HTTP, so nothing reaches Adwave, no key is
;;;; needed and no campaign is touched. What is proved is the cell's own
;;;; wiring: the section (a healthy start, and a missing key refused), the two
;;;; tools through the dispatch the model calls them by -- the MCP method and
;;;; the tool each one names, the bearer key and the session id on the wire,
;;;; the person's yes before anything that spends -- and the /adwave command.

(in-package #:nodecode.test)

(define-test-slice "adwave" "ADWAVE-CELL-" :start nodecode-adwave:start-cell)

;;; --- fixtures --------------------------------------------------------------------

(defparameter *calls* nil
  "Every request the stub answered, newest first, as (METHOD PARAMS HEADERS).")

(defparameter *answer* nil
  "When set, (TEXT STATUS HEADERS): what the stub answers to tools/call.")

(defparameter *offered*
  (vector
   (nlk:json-object "name" "add_business"
                     "description" "Add a business from its website. The analysis is free."
                     "annotations" (nlk:json-object "readOnlyHint" t)
                     "inputSchema" (nlk:json-object
                                    "type" "object"
                                    "properties" (nlk:json-object
                                                  "website_url" (nlk:json-object "type" "string")
                                                  "name" (nlk:json-object "type" "string"))
                                    "required" (vector "website_url")))
   (nlk:json-object "name" "get_campaign_report"
                     "description" "Spend, sessions, conversions, CPA and ROAS by channel."
                     "annotations" (nlk:json-object "readOnlyHint" t))
   (nlk:json-object "name" "launch_campaign"
                     "description" "Launch a draft campaign, setting aside each day's budget from the wallet."
                     "annotations" (nlk:json-object "readOnlyHint" nil))
   (nlk:json-object "name" "set_ad_serving"
                     "description" "Serve or stop serving one ad."))
  "The tools the stub server lists: two read-only, one that spends, one with no hint.")

(defun envelope (id result)
  "One JSON-RPC answer carrying RESULT, as the wire spells it."
  (nlk:encode-json-object
   (nlk:json-object "jsonrpc" "2.0" "id" id "result" result)))

(defun text-result (text &key errorp)
  "One tools/call result carrying TEXT."
  (nlk:json-object "content" (vector (nlk:json-object "type" "text" "text" text))
                   "isError" (and errorp t)))

(defun call-result (params)
  "What the stub's tools/call answers: the tool's name and the arguments back."
  (let ((tool (nlk:json-value params :string "name"))
        (arguments (nlk:json-value params :object "arguments")))
    (text-result (format nil "~a answered~@[ ~a~]" tool
                         (and arguments (nlk:encode-json-object arguments))))))

(defun session-headers ()
  (let ((headers (make-hash-table :test #'equal)))
    (setf (gethash "mcp-session-id" headers) "sess-1")
    headers))

(defun stub-wire (options)
  "NLK:HTTP, answering from the request's own method; nothing leaves the machine."
  (let* ((body (nlk:decode-json (or (getf options :content) "{}")))
         (name (nlk:json-value body :text "method"))
         (params (nlk:json-value body :object "params"))
         (id (nlk:json-value body :integer "id")))
    (push (list name params (getf options :headers)) *calls*)
    (cond ((equal name "initialize")
           (values (envelope id (nlk:json-object "protocolVersion" "2025-06-18"
                                                   "capabilities" (nlk:make-json-object)))
                   200 (session-headers) nil))
          ((equal name "notifications/initialized")
           (values "" 202 nil nil))
          ((equal name "tools/list")
           (values (envelope id (nlk:json-object "tools" *offered*)) 200 nil nil))
          ((equal name "tools/call")
           (if *answer*
               (values (first *answer*) (second *answer*) (third *answer*) nil)
               (values (envelope id (call-result params)) 200 nil nil)))
          (t (values "{}" 404 nil nil)))))

(defmacro with-adwave-wire (&body body)
  "BODY with NLK:HTTP stubbed: every request is logged and answered here."
  `(let ((*calls* '())
         (*answer* nil))
     (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                (stub-wire options))
       ,@body)))

(defmacro with-adwave-running (&body body)
  "BODY with the cell started on a literal section, stopped on unwind."
  `(with-cell-stop ((adwave-start "token" "key-1"))
     ,@body))

(defun adwave-request (method)
  "The newest stubbed request of JSON-RPC METHOD, or NIL."
  (find method *calls* :test #'equal :key #'first))

(defun adwave-request-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (third request) :test #'string-equal)))

(defun adwave-tool-call (&optional (index 0))
  "The INDEX'th newest tools/call the cell sent, as (METHOD PARAMS HEADERS)."
  (nth index (remove-if-not (lambda (call) (equal (first call) "tools/call")) *calls*)))

(defun adwave-sent-tool (&optional (index 0))
  "The MCP tool name the INDEX'th newest tools/call named."
  (let ((call (adwave-tool-call index)))
    (and call (nlk:json-value (second call) :string "name"))))

(defun adwave-sent-arguments (&optional (index 0))
  "The arguments object of the INDEX'th newest tools/call."
  (let ((call (adwave-tool-call index)))
    (and call (nlk:json-value (second call) :object "arguments"))))

(defun adwave-called (tool arguments)
  "TOOL called the way the model calls it, as the text the model reads back."
  (nle::message-content (execute-wire-call "adwave-call" tool arguments)))

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "adwave"
  (:fixture with-adwave-wire)
  (:config "token" "key-1")
  (:help :adwave)
  (:command "adwave")
  (:running
   (is (nle::find-tool "adwave_tools") "the listing tool is registered")
   (is (nle::find-tool "adwave_call") "and the one that runs a tool")
   (is (= 2 (count-if (lambda (entry) (eql 0 (search "adwave_" (first entry))))
                      nle::*tools*)) "two of them, and no others"))
  (:stopped
   (is (null (nle::find-tool "adwave_tools")) "and stop takes every one back")
   (is (null (nle::find-tool "adwave_call"))))
  (:refused ("token" "key-1" "timeout_seconds" 0)
            ("token" "key-1" "max_chars" 10)))

(deftest adwave-cell-a-start-with-no-key-refuses (with-adwave-wire)
  (with-stubbed-fdefinition (nodecode-adwave::mcp-token (url)
                                (declare (ignore url))
                                nil)
    (with-stubbed-fdefinition (nodecode-adwave::environment-token (name)
                                  (declare (ignore name))
                                  nil)
      (let ((text (refusal-text nlk:config-refusal (adwave-start))))
        (is-carrying text "API key" "/mcp")
        (is (null (nle::find-tool "adwave_tools")) "nothing was installed")))))

(deftest adwave-cell-the-mcp-cells-sign-in-is-reused (with-adwave-wire)
  ;; The shipped mcp cell keeps its OAuth sign-in under the home; this cell
  ;; reads that key rather than asking for one of its own.
  (let ((path (nlk:home "mcp/oauth.json")))
    (unwind-protect
         (progn
           (ensure-directories-exist path)
           (with-open-file (out path :direction :output :if-exists :supersede
                                      :if-does-not-exist :create)
             (write-string (nlk:encode-json-object
                            (nlk:json-object "https://waverunner.adwave.com/api/mcp"
                                             (nlk:json-object "tokens"
                                                              (nlk:json-object "access_token" "key-stored"))))
                           out))
           (with-cell-stop ((adwave-start))
             (adwave-called "adwave_call" "{\"name\": \"get_campaign_report\"}")
             (is (equal "Bearer key-stored"
                        (adwave-request-header (adwave-tool-call) "authorization")) "the key the mcp cell kept is the one sent")))
      (when (probe-file path) (ignore-errors (delete-file path))))))

;;; --- the wire ---------------------------------------------------------------------

(deftest adwave-cell-one-session-serves-every-call (with-adwave-wire)
  (with-adwave-running
    (adwave-called "adwave_tools" "{}")
    (adwave-called "adwave_call" "{\"name\": \"get_campaign_report\"}")
    (is (= 1 (count-if (lambda (call) (equal (first call) "initialize")) *calls*)) "initialize happens once and is reused")
    (is (equal "Bearer key-1" (adwave-request-header (adwave-tool-call) "authorization")) "with this cell's own key")
    (is (equal "sess-1" (adwave-request-header (adwave-tool-call) "mcp-session-id")) "and the session id the server handed is echoed")))

(deftest adwave-cell-an-error-the-server-reports-fails-the-call (with-adwave-wire)
  (with-adwave-running
    (setf *answer* (list (envelope 1 (text-result "campaign c-1 is not a draft" :errorp t))
                         200 nil))
    (let ((answer (adwave-called "adwave_call"
                                  "{\"name\": \"get_campaign_report\", \"arguments\": {\"campaignId\": \"c-1\"}}")))
      (is (search "ERROR:" answer) "the call is failed, not answered")
      (is (search "campaign c-1 is not a draft" answer) "and the server's words reach the model"))))

;;; --- the tools say what the skill says ---------------------------------------------

(deftest adwave-cell-the-list-marks-what-needs-a-yes (with-adwave-wire)
  (with-adwave-running
    (let ((answer (adwave-called "adwave_tools" "{}")))
      (is (search "add_business" answer) "every tool the server offers is named")
      (is (search "arguments: name, website_url (required: website_url)" answer) "with the arguments the tool's own schema names")
      (is (search "get_campaign_report [read-only]" answer) "a read-only tool says so")
      (is (search "launch_campaign [needs confirmed true]" answer) "a spending tool says it needs the person's yes")
      (is (search "set_ad_serving [needs confirmed true]" answer) "and a tool with no readOnlyHint is treated as changing the account"))))

(deftest adwave-cell-the-list-takes-a-filter (with-adwave-wire)
  (with-adwave-running
    (let ((answer (adwave-called "adwave_tools" "{\"filter\": \"report\"}")))
      (is (search "get_campaign_report" answer) "the matching tool is listed")
      (is (not (search "add_business" answer)) "and the others are not"))))

(deftest adwave-cell-a-read-only-tool-runs-without-confirmation (with-adwave-wire)
  (with-adwave-running
    (let ((answer (adwave-called "adwave_call" "{\"name\": \"get_campaign_report\"}")))
      (is (equal "get_campaign_report" (adwave-sent-tool)) "the server's own tool is called")
      (is (search "get_campaign_report answered" answer) "and its answer reaches the model"))))

(deftest adwave-cell-a-spending-tool-needs-the-persons-yes (with-adwave-wire)
  (with-adwave-running
    (let ((answer (adwave-called "adwave_call" "{\"name\": \"launch_campaign\", \"arguments\": {\"campaignId\": \"c-1\"}}")))
      (is (search "confirmed true" answer) "the refusal says what the yes is")
      (is (search "dollars" answer) "and that the amount is said first")
      (is (null (adwave-tool-call)) "and nothing was launched"))))

(deftest adwave-cell-a-confirmed-call-runs-with-its-arguments (with-adwave-wire)
  (with-adwave-running
    (adwave-called "adwave_call"
                    "{\"name\": \"launch_campaign\", \"arguments\": {\"campaignId\": \"c-1\"}, \"confirmed\": true}")
    (is (equal "launch_campaign" (adwave-sent-tool)))
    (is (equal "c-1" (nlk:json-value (adwave-sent-arguments) :text "campaignId")) "the tool's own arguments object goes through as the server names it")))

(deftest adwave-cell-an-unknown-name-lists-what-there-is (with-adwave-wire)
  (with-adwave-running
    (adwave-called "adwave_tools" "{}")
    (let ((answer (adwave-called "adwave_call" "{\"name\": \"pause_everything\"}")))
      (is (search "adwave_tools" answer) "the model is told where the names are")
      (is (null (adwave-tool-call)) "and nothing was sent as a call")
      (is (= 2 (count-if (lambda (call) (equal (first call) "tools/list")) *calls*)) "the list is read again before the name is refused"))))

;;; --- the operator's command ---------------------------------------------------------

(deftest adwave-cell-the-command-lists-the-tools (with-adwave-wire)
  (with-adwave-running
    (let ((answer (nodecode-adwave::run-command "" nil)))
      (is (search "add_business" answer) "no argument lists what the server offers")
      (is (search "The key in use comes from" answer) "and says where the key came from"))))

(deftest adwave-cell-the-command-says-where-the-key-came-from (with-adwave-wire)
  (with-adwave-running
    (let ((answer (nodecode-adwave::run-command "login" nil)))
      (is (search "token member" answer) "the operator is told, in words")
      (is (not (search "key-1" answer)) "and never the key itself")
      (is (null *calls*) "with no request to the server"))))

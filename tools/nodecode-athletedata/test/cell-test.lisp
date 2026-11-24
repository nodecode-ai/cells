;;;; cell-test.lisp --- the athletedata cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The key is a stubbed ENVIRONMENT-VALUE and the server a stubbed HTTP
;;;; that answers by JSON-RPC method; the tool list is a fixture. Nothing
;;;; here reaches the network, the environment or the operator's home.

(in-package #:nodecode.test)

(define-test-slice "athletedata" "ATHLETEDATA-CELL-" :start nodecode-athletedata:start-cell)

;;; --- fixtures ------------------------------------------------------------------

(defmacro with-athletedata-key (&body body)
  "BODY with the variable the section names answering a test key."
  `(with-stubbed-fdefinition (nodecode-athletedata::environment-value (name)
                             (if (equal name "ATHLETEDATA_API_KEY") "sk_soma_test" nil))
     ,@body))

(defun athletedata-headers (&key session (type "application/json"))
  "A response's header table, as dexador hands one back: lowercase names."
  (let ((table (make-hash-table :test #'equal)))
    (setf (gethash "content-type" table) type)
    (when session (setf (gethash "mcp-session-id" table) session))
    table))

(defun athletedata-reply (id result &key (status 200) session (type "application/json"))
  "One JSON-RPC result, as the server answers it."
  (values (nlk:encode-json-object (nlk:json-object "jsonrpc" "2.0" "id" id "result" result))
          status (athletedata-headers :session session :type type)))

(defun athletedata-tool (name &key (read-only t) (description "a fixture tool"))
  (nlk:json-object "name" name "description" description
                    "annotations" (nlk:json-object "readOnlyHint" (and read-only t))))

(defparameter +two-tools+
  (vector (athletedata-tool "get_readiness_today")
          (athletedata-tool "push_workout_to_watch" :read-only nil
                             :description "Push a structured workout to the athlete's watch."))
  "What tools/list answers in a test.")

(defparameter +one-workout+ (nlk:json-object "content" (vector (nlk:json-object "type" "text" "text" "pushed 1 workout"))))
(defparameter +readiness+ (nlk:json-object "content" (vector (nlk:json-object "type" "text" "text" "readiness 71"))))

(defvar *asked* '()
  "The stubbed requests of one test, newest first.")

(defmacro with-athletedata-wire ((&rest routes) &body body)
  "BODY with NLK:HTTP stubbed and the cell started on a test key. ROUTES are
RPC-METHOD FORM pairs: the first whose METHOD the request asks answers
FORM's (values TEXT STATUS HEADERS), with ID bound to the request's id. An
initialize and a notification answer a bare success unless a route says
otherwise; a method no route names is an error."
  `(let ((*asked* '()))
     (with-athletedata-key
       (with-stubbed-fdefinition (nodecode-athletedata::http (method url &rest args)
         (push (list* method args) *asked*)
         (let* ((message (nlk:decode-json (getf args :content)))
                (rpc (nlk:json-value message :string "method"))
                (id (nlk:json-value message :integer "id")))
           (cond
             ,@(loop for (name form) on routes by #'cddr
                     collect `((equal rpc ,name) ,form))
             ((equal rpc "initialize")
              (athletedata-reply id (nlk:json-object "protocolVersion" "2025-06-18"
                                                      "capabilities" (nlk:json-object))))
             ((equal rpc "notifications/initialized") (values "" 202 nil))
             (t (error "unexpected request ~a" rpc)))))
       (with-cell-stop ((athletedata-start))
         ,@body)))))

(defun athletedata-entry (name)
  (nle::find-tool name))

(defun athletedata-run (name arguments)
  "Run the registered tool NAME with the JSON object ARGUMENTS, as dispatch does."
  (let ((entry (athletedata-entry name)))
    (assert entry () "no tool named ~a" name)
    (funcall (third entry) (nlk:contract-decode (second entry) arguments))))

(defun athletedata-tools (arguments)
  (athletedata-run "athletedata_tools" arguments))

(defun athletedata-call (arguments)
  (athletedata-run "athletedata_call" arguments))

(defun athletedata-asked (&key (rpc "tools/list"))
  "The stubbed requests whose JSON-RPC method is RPC, oldest first."
  (loop for request in (reverse *asked*)
        for message = (nlk:decode-json (getf (cdr request) :content))
        when (equal rpc (nlk:json-value message :string "method"))
          collect request))

(defun athletedata-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cdr request) :headers) :test #'string-equal)))

(defun athletedata-body (request)
  "REQUEST's JSON-RPC message, decoded."
  (nlk:decode-json (getf (cdr request) :content)))

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "athletedata"
  (:fixture with-athletedata-key)
  (:help :athletedata)
  (:command "athletedata")
  (:running (is (athletedata-entry "athletedata_tools") "athletedata_tools is registered")
            (is (athletedata-entry "athletedata_call") "athletedata_call is registered")
            (is (equal "https://mcp.athletedata.health/mcp" (nodecode-athletedata::setting :url)))
            (is (= 60 (nodecode-athletedata::setting :timeout-seconds)) "a minute a call")
            (is (null (nodecode-athletedata::setting :allow-writes)) "a writing tool asks first"))
  (:stopped (is (null (athletedata-entry "athletedata_tools")) "stop unregisters the tools")
            (is (null (athletedata-entry "athletedata_call"))))
  (:refused ("api_key_env" "ATHLETEDATA_UNSET_FOR_THE_TEST")))

(deftest athletedata-cell-a-key-is-required ()
  (with-stubbed-fdefinition (nodecode-athletedata::environment-value (name) (ignore name))
    (is-carrying (text (refusal-text nlk:config-refusal (athletedata-start)))
      ("no API key" "the refusal names the problem")
      ("ATHLETEDATA_API_KEY" "and the variable to set"))
    (is (null (athletedata-entry "athletedata_tools")) "a refused start installed nothing")))

;;; --- athletedata_tools ------------------------------------------------------------

(deftest athletedata-cell-tools-lists-the-servers-own-tools ()
  (with-athletedata-wire ("tools/list" (athletedata-reply id (nlk:json-object "tools" +two-tools+)))
    (is-carrying (answer (athletedata-tools (nlk:json-object)))
      ("get_readiness_today" "a reading tool is listed")
      ("push_workout_to_watch *" "a writing tool is starred")
      ("Push a structured workout" "with its own description"))
    (let ((request (first (athletedata-asked))))
      (is (= 1 (length (athletedata-asked))) "one tools/list, and the copy is kept")
      (is (equal "Bearer sk_soma_test" (athletedata-header request "authorization")) "the key rides the header")
      (is (equal "application/json" (athletedata-header request "content-type")))
      (is (search "text/event-stream" (athletedata-header request "accept")) "and a stream is accepted as an answer")
      (is (equal "2025-06-18" (athletedata-header request "mcp-protocol-version"))))
    (athletedata-tools (nlk:json-object))
    (is (= 1 (length (athletedata-asked))) "a second listing asks nothing")))

;;; --- athletedata_call ----------------------------------------------------------------

(deftest athletedata-cell-a-writing-tool-needs-confirm ()
  (with-athletedata-wire ("tools/list" (athletedata-reply id (nlk:json-object "tools" +two-tools+))
                          "tools/call" (athletedata-reply id +one-workout+))
    (is-carrying (answer (athletedata-call (nlk:json-object "tool" "push_workout_to_watch")))
      ("writes to the athlete's accounts" "the gate says why it did not run")
      ("confirm true" "and what lets it run"))
    (is (null (athletedata-asked :rpc "tools/call")) "nothing was called")
    (is-carrying (answer (athletedata-call (nlk:json-object "tool" "push_workout_to_watch"
                                                             "confirm" t)))
      ("pushed 1 workout" "with confirm the answer is the server's"))
    (let* ((calls (athletedata-asked :rpc "tools/call"))
           (request (first calls)))
      (is (= 1 (length calls)) "one call")
      (is (equal "push_workout_to_watch" (nlk:json-value (athletedata-body request) :string "params" "name")) "the tool's name rides the params"))))

(deftest athletedata-cell-a-reading-tool-asks-nothing ()
  (with-athletedata-wire ("tools/list" (athletedata-reply id (nlk:json-object "tools" +two-tools+))
                          "tools/call" (athletedata-reply id +readiness+))
    (is-carrying (answer (athletedata-call (nlk:json-object "tool" "get_readiness_today"
                                                             "arguments" (nlk:json-object "date" "2026-11-08"))))
      ("readiness 71" "a reading tool needs no confirm"))
    (let ((request (first (athletedata-asked :rpc "tools/call"))))
      (is (equal "2026-11-08" (nlk:json-value (athletedata-body request)
                                               :string "params" "arguments" "date")) "its arguments ride through untouched"))))

;;; --- the session, and what a server that will not answer says ------------------------

(deftest athletedata-cell-answers-a-stream-and-echoes-the-session ()
  (with-athletedata-wire
      ("initialize"
       (let ((body (nlk:encode-json-object (nlk:json-object "jsonrpc" "2.0" "id" id
                                                             "result" (nlk:json-object "protocolVersion" "2025-06-18")))))
         (values (format nil "event: message~%data: ~a~%~%" body) 200
                 (athletedata-headers :session "s-1" :type "text/event-stream")))
       "tools/list" (athletedata-reply id (nlk:json-object "tools" +two-tools+)))
    (is-carrying (answer (athletedata-tools (nlk:json-object)))
      ("get_readiness_today" "an event stream's reply is read whole"))
    (let ((request (first (athletedata-asked))))
      (is (equal "s-1" (athletedata-header request "mcp-session-id")) "the session the stream issued is echoed on the next request"))))

(deftest athletedata-cell-a-refused-key-says-so ()
  (with-athletedata-wire ("tools/list" (values "" 401 (athletedata-headers)))
    (is-carrying (answer (athletedata-tools (nlk:json-object)))
      ("401" "the status is reported")
      ("check the variable api_key_env names" "with the one thing to check"))))

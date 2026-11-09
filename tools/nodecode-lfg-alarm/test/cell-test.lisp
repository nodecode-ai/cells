;;;; cell-test.lisp --- the LFG Alarm cell against a stubbed MCP server.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every message is a stubbed NLK:HTTP answering JSON-RPC, and the key a
;;;; stubbed NLE::CREDENTIAL-ENV: nothing reaches the server, the environment
;;;; or the operator's files. The tools are called the way a model's call
;;;; reaches them -- through the real dispatch -- because the cell registers
;;;; them from the server's own list, which is the whole of what the original
;;;; plugin's mcp.json pointed at.

(in-package #:nodecode.test)

(define-test-slice "lfg-alarm" "LFG-ALARM-CELL-")

(defun lfg-alarm-config (&rest pairs)
  "A whole shared config holding the `lfg-alarm' section PAIRS make."
  (nlk:json-object "lfg-alarm" (apply #'nlk:make-json-object pairs)))

(defun lfg-alarm-start (&rest pairs)
  "START-CELL against a literal config, the way the gateway calls it."
  (nodecode-lfg-alarm:start-cell (apply #'lfg-alarm-config pairs)))

;;; --- the stubbed server ------------------------------------------------------------

(defparameter +wire-tools+
  '(("set_time_alarm" . "Set an alarm for a time, once or on chosen days.")
    ("list_my_alarms" . "List the alarms in this account.")
    ("ring_alarm" . "Ring an alarm now, on every phone that joined it."))
  "What the stubbed server lists, as (NAME . DESCRIPTION).")

(defvar *wire-asked* '()
  "Every request the stub answered, newest first: (METHOD CONTENT).")

(defvar *wire-call-text* "the alarm is set")
(defvar *wire-call-error* nil)
(defvar *wire-call-status* 200)
(defvar *wire-sse* nil)

(defmacro with-lfg-wire (&body body)
  "BODY with NLK:HTTP a stubbed server and the key a stubbed environment."
  `(let ((*wire-asked* '()) (*wire-call-text* "the alarm is set")
         (*wire-call-error* nil) (*wire-call-status* 200) (*wire-sse* nil))
     (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                 (declare (ignore method url))
                                 (wire-answer options))
       (with-stubbed-fdefinition (nle::credential-env (name)
                                   (and (equal name "LFG_ALARM_TOKEN") "test-key"))
         ,@body))))

(defmacro with-lfg-key (&body body)
  "BODY with a token variable answering, and no server necessarily up."
  `(with-stubbed-fdefinition (nle::credential-env (name)
                              (and (equal name "LFG_ALARM_TOKEN") "test-key"))
     ,@body))

(defun wire-rpc (id result)
  (nlk:json-object "jsonrpc" "2.0" "id" id "result" result))

(defun wire-tool-list (id)
  (wire-rpc id
            (nlk:json-object
             "tools"
             (coerce (loop for (name . description) in +wire-tools+
                           collect (nlk:json-object
                                    "name" name
                                    "description" description
                                    "inputSchema"
                                    (nlk:json-object
                                     "type" "object"
                                     "properties" (nlk:json-object
                                                   "title" (nlk:json-object "type" "string"))
                                     "required" #())))
                     'vector))))

(defun wire-call-result (id)
  (wire-rpc id
            (nlk:json-object
             "content" (vector (nlk:json-object "type" "text"
                                                 "text" (or *wire-call-error* *wire-call-text*)))
             "isError" (and *wire-call-error* t))))

(defun wire-answer (options)
  "The stubbed server's (values BODY STATUS HEADERS) for one request."
  (let* ((content (getf options :content))
         (message (ignore-errors (nlk:decode-json content)))
         (method (and (hash-table-p message) (gethash "method" message)))
         (id (and (hash-table-p message) (gethash "id" message))))
    (push (list method content) *wire-asked*)
    (labels ((answer (object &key (status 200) headers)
               (let ((text (nlk:encode-json-object object)))
                 (values (if *wire-sse*
                             (format nil "event: message~%data: ~a~2%~%" text)
                             text)
                         status headers))))
      (cond ((equal method "initialize")
             (answer (wire-rpc id (nlk:json-object
                                   "protocolVersion" "2025-06-18"
                                   "capabilities" (nlk:json-object)
                                   "serverInfo" (nlk:json-object "name" "lfg-alarm"
                                                                  "version" "1")))
                     :headers (list (cons "mcp-session-id" "session-1"))))
            ((equal method "notifications/initialized") (values "" 202 nil))
            ((equal method "tools/list") (answer (wire-tool-list id)))
            ((equal method "tools/call")
             (answer (wire-call-result id) :status *wire-call-status*))
            (t (values "{}" 404 nil))))))

;;; --- the lifecycle, from the one declaration ----------------------------------------

(define-cell-lifecycle-tests "lfg-alarm"
  (:config "token_env" "LFG_ALARM_TOKEN")
  (:help :lfg-alarm)
  (:command "lfg-alarm")
  (:fixture with-lfg-wire)
  (:running
   (is (nle::find-tool "set_time_alarm") "the start registers what the server lists"))
  (:stopped
   (is (null (nle::find-tool "set_time_alarm")) "and the stop takes it back"))
  (:refused ("url" 5) ("token_env" 5)))

(deftest lfg-alarm-cell-without-a-key-refuses (with-lfg-wire)
  ;; The one setting no tool can work without: a section that names no key is
  ;; refused at start, before anything installs or dials.
  (let ((condition (signals-error nlk:config-refusal (lfg-alarm-start "token_env" ""))))
    (is (search "no key" (princ-to-string condition)) "the refusal says what is missing")
    (is (null (nle::find-tool "set_time_alarm")) "and installs nothing")
    (is (null *wire-asked*) "nor dials the server")))

;;; --- the tools ---------------------------------------------------------------------

(deftest lfg-alarm-cell-registers-the-servers-tools (with-lfg-wire)
  (with-cell-stop ((lfg-alarm-start "token_env" "LFG_ALARM_TOKEN"))
    (let ((entry (nle::find-tool "set_time_alarm")))
      (is entry "the server's own tool name is in the table")
      (is (search "Set an alarm" (getf (second entry) :description))
          "with the server's own description")
      (is (equal "object" (nlk:json-value (getf (second entry) :parameters) :string "type"))
          "and its input schema, as the model is shown it"))
    (is (= 1 (count "initialize" *wire-asked* :key #'first :test #'equal))
        "one handshake, not one per listed page")
    (is (find "notifications/initialized" *wire-asked* :key #'first :test #'equal)
        "and the client's half of it")))

(deftest lfg-alarm-cell-calls-a-tool (with-lfg-wire)
  (with-cell-stop ((lfg-alarm-start "token_env" "LFG_ALARM_TOKEN"))
    (setf *wire-call-text* "Alarm set for 07:00.")
    (let ((answer (nle::message-content
                   (execute-wire-call "lfg-1" "set_time_alarm" "{\"title\":\"Airport\"}"))))
      (is (search "Alarm set for 07:00" answer) "the server's words are the answer"))
    (let* ((sent (find "tools/call" *wire-asked* :key #'first :test #'equal))
           (message (nlk:decode-json (second sent))))
      (is (equal "set_time_alarm" (nlk:json-value message :string "params" "name"))
          "the call carries the server's tool name")
      (is (equal "Airport" (nlk:json-value message :string "params" "arguments" "title"))
          "and the arguments the model passed, unchanged"))))

(deftest lfg-alarm-cell-reads-a-streamed-reply (with-lfg-wire)
  (setf *wire-sse* t)
  (with-cell-stop ((lfg-alarm-start "token_env" "LFG_ALARM_TOKEN"))
    (is (nle::find-tool "ring_alarm") "a handshake and a list over the event stream are read")
    (setf *wire-call-text* "streamed answer")
    (is (search "streamed answer"
                (nle::message-content (execute-wire-call "lfg-2" "ring_alarm" "{}")))
        "and so is a tool's")))

(deftest lfg-alarm-cell-reports-a-tool-error (with-lfg-wire)
  (with-cell-stop ((lfg-alarm-start "token_env" "LFG_ALARM_TOKEN"))
    (setf *wire-call-error* "no phone is signed in to this account")
    (let ((answer (nle::message-content (execute-wire-call "lfg-3" "ring_alarm" "{}"))))
      (is (search "ERROR" answer) "a tool's own error is the call's failure")
      (is (search "no phone is signed in" answer) "and says what the server said"))))

(deftest lfg-alarm-cell-command-lists-the-tools (with-lfg-wire)
  (with-cell-stop ((lfg-alarm-start "token_env" "LFG_ALARM_TOKEN"))
    (let ((text (cell-entry "nodecode-lfg-alarm" "lfg-alarm" nil)))
      (is (search "3 tools" text) "the command counts what the server offers")
      (is (search "set_time_alarm" text) "and names them")
      (is (search "Ring an alarm now"
                  (cell-entry "nodecode-lfg-alarm" "lfg-alarm" "ring_alarm"))
          "naming one answers what it does"))))

(deftest lfg-alarm-cell-a-refused-key-is-said (with-lfg-wire)
  (with-cell-stop ((lfg-alarm-start "token_env" "LFG_ALARM_TOKEN"))
    (setf *wire-call-status* 401)
    (let ((answer (nle::message-content (execute-wire-call "lfg-4" "list_my_alarms" "{}"))))
      (is (search "401" answer) "a refused key is reported, not swallowed")
      (is (search "check the key" answer) "with where to look"))))

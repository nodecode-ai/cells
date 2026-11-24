;;;; cell-test.lisp --- the lifecycle, the wire handshake, the tools and /ambassly.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here: the cell starts and stops cleanly and a malformed
;;;; section refuses; the client introduces itself, takes the session the
;;;; server issues, echoes it, and sends the key as the Bearer and nowhere
;;;; else; the catalog is the server's own; a call passes its arguments and
;;;; answers the tool's text; a missing key and a refused status are named
;;;; rather than thrown at the model; and /ambassly lists, calls and refuses
;;;; a stray argument. Nothing reaches the network: the one HTTP seam is
;;;; stubbed.

(in-package #:nodecode.test)

;;; --- the lifecycle -------------------------------------------------------------

(define-test-slice "ambassly" "AMBASSLY-CELL-" :start nodecode-ambassly:start-cell)

(define-cell-lifecycle-tests "ambassly"
  (:config "api_key" "AMBASSLY_API_KEY")
  (:command "ambassly")
  (:help :ambassly)
  (:running (is (nle::find-tool "ambassly_tools") "the ambassly_tools tool is registered")
            (is (nle::find-tool "ambassly_call") "the ambassly_call tool is registered"))
  (:stopped (is (null (nle::find-tool "ambassly_tools")) "stop unregisters ambassly_tools")
            (is (null (nle::find-tool "ambassly_call")) "and ambassly_call"))
  (:refused ("timeout_seconds" 0))
  (:idle nodecode-ambassly:ambassly-error (nodecode-ambassly:ambassly-tools)))

;;; --- the stubbed server --------------------------------------------------------

(defparameter *ambassly-log* '()
  "Every frame the stub was asked to carry, newest first.")

(defun ambassly-frame-reply (frame result)
  "One JSON-RPC reply to FRAME carrying RESULT, which is JSON text."
  (format nil "{\"jsonrpc\":\"2.0\",\"id\":~a,\"result\":~a}" (gethash "id" frame) result))

(defun ambassly-wire (url headers content timeout)
  "The server's side, scripted: initialize issues a session, tools/list names
two tools, tools/call answers one, and every frame is logged."
  (declare (ignore timeout))
  (let* ((frame (nlk:decode-json content))
         (method (nlk:json-value frame :string "method")))
    (push (list :method method
                :url url
                :headers (loop for (name . value) in headers
                               collect (cons (string-downcase name) value))
                :params (gethash "params" frame))
          *ambassly-log*)
    (cond
      ((equal method "initialize")
       (values (ambassly-frame-reply frame "{\"protocolVersion\":\"2025-06-18\",\"serverInfo\":{\"name\":\"ambassly\"}}")
               200
               (list (cons "content-type" "application/json")
                     (cons "mcp-session-id" "sess-1"))))
      ((equal method "notifications/initialized") (values "" 202 '()))
      ((equal method "tools/list")
       (values (ambassly-frame-reply
                frame
                "{\"tools\":[{\"name\":\"list_programs\",\"description\":\"The merchant's affiliate programs\"},{\"name\":\"list_pending_commissions\",\"description\":\"Commissions waiting for a decision\"}]}")
               200 (list (cons "content-type" "application/json"))))
      ((equal method "tools/call")
       (values (ambassly-frame-reply
                frame
                "{\"content\":[{\"type\":\"text\",\"text\":\"2 programs: spring (20%), autumn (15%)\"}]}")
               200 (list (cons "content-type" "application/json"))))
      (t (values "{}" 404 '())))))

(defun ambassly-frame (method)
  "The one logged frame whose method is METHOD."
  (find method *ambassly-log* :key (lambda (entry) (getf entry :method)) :test #'equal))

(defmacro with-ambassly-wire ((&key (key "test-key")) &body body)
  "BODY with the key variable answering KEY and the endpoint scripted."
  `(with-stubbed-fdefinitions
       ((nle::credential-env (name) (and (equal name "AMBASSLY_API_KEY") ,key))
        (nodecode-ambassly::http-post (url headers content timeout)
          (ambassly-wire url headers content timeout)))
     (setf *ambassly-log* '())
     ,@body))

;;; --- the wire -----------------------------------------------------------------

(deftest ambassly-cell-the-catalog-is-the-servers-own (with-ambassly-wire ()
  (with-cell-stop ((ambassly-start))
    (let ((tools (nodecode-ambassly:ambassly-tools)))
      (is (equal '("list_pending_commissions" "list_programs")
                 (mapcar (lambda (tool) (getf tool :name)) tools)) "every tool the server listed, in name order")
      (is (search "list_programs -- The merchant's affiliate programs"
                  (nodecode-ambassly:ambassly-text)) "and the text names each one with its description"))
    (let ((hello (ambassly-frame "initialize"))
          (listing (ambassly-frame "tools/list")))
      (is (equal "Bearer test-key" (cdr (assoc "authorization" (getf hello :headers) :test #'equal))) "the key rides as the Bearer")
      (is (equal "https://ambassly.com/api/mcp" (getf hello :url)) "to the endpoint the section names")
      (is (null (assoc "mcp-session-id" (getf hello :headers) :test #'equal)) "the first frame carries no session")
      (is (equal "sess-1" (cdr (assoc "mcp-session-id" (getf listing :headers) :test #'equal))) "later frames echo the session the server issued")
      (is (ambassly-frame "notifications/initialized") "the initialized notice is sent")
      (is (= 1 (count "initialize" *ambassly-log* :key (lambda (entry) (getf entry :method))
                      :test #'equal)) "and the handshake happens once per session")))))

(deftest ambassly-cell-a-call-passes-arguments-and-answers-the-tools-text (with-ambassly-wire ()
  (with-cell-stop ((ambassly-start))
    (let* ((call (nodecode-ambassly:decode-ambassly-tool-call
                  (cell-json "{\"tool\":\"list_pending_commissions\",\"arguments\":{\"status\":\"pending\"}}")))
           (text (nodecode-ambassly:tool-call-tool call)))
      (is (search "2 programs: spring (20%), autumn (15%)" text) "the tool's own text")
      (let ((sent (ambassly-frame "tools/call")))
        (is (equal "list_pending_commissions" (nlk:json-value (getf sent :params) :string "name")) "the tool named")
        (is (equal "pending" (nlk:json-value (nlk:json-value (getf sent :params) :object "arguments")
                                              :string "status")) "and the arguments object it was given"))))))

(deftest ambassly-cell-a-missing-key-is-named-not-sent (with-ambassly-wire (:key nil)
  (with-cell-stop ((ambassly-start))
    (let ((text (refusal-text nodecode-ambassly:ambassly-error (nodecode-ambassly:ambassly-tools))))
      (is (search "no key" text) "the refusal says no key")
      (is (search "AMBASSLY_API_KEY" text) "and names the variable to set")
      (is (null *ambassly-log*) "and no frame went out")))))

(deftest ambassly-cell-a-refused-status-is-named-by-its-code (with-ambassly-wire ()
  (with-cell-stop ((ambassly-start))
    (with-stubbed-fdefinition (nodecode-ambassly::http-post (url headers content timeout)
      (declare (ignore url headers content timeout))
      (values "{\"error\":\"invalid_token\"}" 401 '()))
      (let ((text (refusal-text nodecode-ambassly:ambassly-error
                                 (nodecode-ambassly:ambassly-tools))))
        (is (search "HTTP 401" text) (format nil "the status is named: ~s" text))
        (is (null (search "Bearer" text)) "and never the key"))))))

;;; --- /ambassly -----------------------------------------------------------------

(deftest ambassly-cell-the-command-lists-calls-and-refuses-stray-arguments (with-ambassly-wire ()
  (with-cell-stop ((ambassly-start))
    (let ((listing (cell-entry "nodecode-ambassly" "ambassly" "")))
      (is (search "2 tools on the Ambassly server" listing) "no arguments lists the catalog")
      (is (search "list_pending_commissions" listing) "with every name in it"))
    (let ((answer (cell-entry "nodecode-ambassly" "ambassly" "list_programs {}")))
      (is (search "spring (20%)" answer) "a tool name calls it and prints the answer"))
    (let ((stray (cell-entry "nodecode-ambassly" "ambassly" "list_programs window")))
      (is (search "must be one JSON object" stray) "a stray argument is a message, not a signal"))
    (let ((idle (cell-entry "nodecode-ambassly" "ambassly" "list_programs")))
      (is (search "spring (20%)" idle) "no arguments object is a call with none")))))

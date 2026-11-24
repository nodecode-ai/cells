;;;; cell-test.lisp --- the LetsFG cell against a stubbed MCP server.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here: the lifecycle every cell owns; that one call is a
;;;; real JSON-RPC exchange -- a handshake with its notification, then
;;;; tools/call with the bearer token and the session the server answered --
;;;; that a search keeps its own arguments and drops the absent ones; that
;;;; letsfg_results picks the flight or the hotel reader; that an event-stream
;;;; answer parses like a plain one; that a tool's own failure and a refused
;;;; sign-in read as ERROR: and as a refusal; and that a section naming no
;;;; token refuses the start. Nothing here reaches letsfg.co.

(in-package #:nodecode.test)

(define-test-slice "letsfg" "LETSFG-CELL-" :start nodecode-letsfg:start-cell)

;;; --- the stubbed server ------------------------------------------------------------

(defvar *letsfg-calls* nil
  "Every request the cell made, newest first: (:method :url :headers :request).")
(defvar *letsfg-text* "ok" "What a tools/call answers as its one text block.")
(defvar *letsfg-failed* nil "True makes the answered tool call report its own failure.")
(defvar *letsfg-status* 200 "The status a tools/call answers.")
(defvar *letsfg-sse* nil "True answers as an event stream rather than a JSON body.")
(defvar *letsfg-tools* '("search_flights" "search_hotels" "get_flight_results" "book_flight")
  "What tools/list advertises.")

(defun letsfg-token-file (&optional (token "lg_test_token"))
  "A temp file holding TOKEN: the section's token_file."
  (let ((path (merge-pathnames (format nil "letsfg-token-~a.txt" (random 1000000))
                                (uiop:ensure-directory-pathname (uiop:temporary-directory)))))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
      (write-string token out))
    (namestring path)))

(defun letsfg-headers (&key session sse)
  (append (list (cons "content-type" (if sse "text/event-stream" "application/json")))
          (when session (list (cons "mcp-session-id" session)))))

(defun letsfg-body (json)
  (if *letsfg-sse* (format nil "event: message~%data: ~a~%~%" json) json))

(defun letsfg-answer (method url headers content)
  "The cell's one network call, answered from the canned server."
  (declare (ignore headers))
  (let* ((request (and content (plusp (length content)) (nlk:decode-json content)))
         (name (nlk:json-value request :string "method"))
         (id (nlk:json-value request :any "id")))
    (push (list :method method :url url :headers headers :request request) *letsfg-calls*)
    (cond
      ((equal name "initialize")
       (values (letsfg-body (nlk:encode-json-object
                             (nlk:json-object "jsonrpc" "2.0" "id" id
                                              "result" (nlk:json-object
                                                        "protocolVersion" "2025-06-18"
                                                        "serverInfo" (nlk:json-object "name" "letsfg")))))
               200 (letsfg-headers :session "sess-1")))
      ((equal name "notifications/initialized") (values "" 202 (letsfg-headers)))
      ((equal name "tools/list")
       (values (letsfg-body (nlk:encode-json-object
                             (nlk:json-object "jsonrpc" "2.0" "id" id
                                              "result" (nlk:json-object
                                                        "tools" (map 'vector
                                                                     (lambda (one)
                                                                       (nlk:json-object "name" one
                                                                                        "description" "a LetsFG tool"))
                                                                     *letsfg-tools*)))))
               (if (= *letsfg-status* 401) 401 200) (letsfg-headers)))
      ((equal name "tools/call")
       (values (letsfg-body (nlk:encode-json-object
                             (nlk:json-object "jsonrpc" "2.0" "id" id
                                              "result" (nlk:json-object
                                                        "content" (vector (nlk:json-object
                                                                           "type" "text" "text" *letsfg-text*))
                                                        "isError" (and *letsfg-failed* t)))))
               *letsfg-status* (letsfg-headers :sse *letsfg-sse*)))
      (t (values (letsfg-body (nlk:encode-json-object
                               (nlk:json-object "jsonrpc" "2.0" "error"
                                                (nlk:json-object "code" -32601
                                                                 "message" "no such method"))))
                 200 (letsfg-headers))))))

(defmacro with-letsfg-wire (&body body)
  "BODY with the cell's one network call answered from the canned server."
  `(let ((*letsfg-calls* '()) (*letsfg-text* "ok") (*letsfg-failed* nil)
         (*letsfg-status* 200) (*letsfg-sse* nil))
     (with-stubbed-fdefinition (nodecode-letsfg::http (method url &key headers content &allow-other-keys)
       (letsfg-answer method url headers content))
       ,@body)))

;;; --- reading the canned server's traffic -------------------------------------------

(defun letsfg-request (method &optional tool)
  "The newest request of METHOD (a tools/call for TOOL when given)."
  (find-if (lambda (one)
             (and (equal method (nlk:json-value (getf one :request) :string "method"))
                  (or (null tool) (equal tool (nlk:json-value (getf one :request) :string "params" "name")))))
           *letsfg-calls*))

(defun letsfg-tool (name arguments)
  "The registered tool NAME called as the dispatch seam calls it."
  (let ((entry (nle::find-tool name)))
    (assert entry () "~a is not registered" name)
    (funcall (third entry) arguments)))

(defun letsfg-header (call name)
  (cdr (assoc name (getf call :headers) :test #'string-equal)))

;;; --- lifecycle ---------------------------------------------------------------------

(define-cell-lifecycle-tests "letsfg"
  (:fixture with-letsfg-wire)
  (:config "token_file" (letsfg-token-file))
  (:help :letsfg)
  (:command "letsfg")
  (:running (is (nle::find-tool "letsfg_call") "the tools are registered")
            (is (nle::find-tool "letsfg_book") "the write path too")
            (is (equal "lg_test_token" (nodecode-letsfg::token)) "the token is read from the file")
            (is (equal nodecode-letsfg::+default-server+ (nodecode-letsfg::endpoint)) "the hosted endpoint")
            (is (= 60 (nodecode-letsfg::setting :timeout-seconds)))
            (is (null nodecode-letsfg::*session*) "nothing was dialled before a call"))
  (:stopped (is (null (nle::find-tool "letsfg_call")) "stop unregisters the tools"))
  (:refused ("timeout_seconds" 0)
            ("token_env" "NODECODE_LETSFG_TEST_UNSET")
            ("token_env" "HOME" "token_file" (letsfg-token-file)))
  (:idle nodecode-letsfg:letsfg-error (nodecode-letsfg:tools)))

(deftest letsfg-cell-a-section-with-no-token-refuses ()
  (with-letsfg-wire
    (is (signals-error nlk:config-refusal (letsfg-start)))
    (is (search "no LetsFG token yet"
                (handler-case (progn (letsfg-start) "")
                  (nlk:config-refusal (condition) (nlk:config-refusal-detail condition)))))
    (is (null (nle::find-tool "letsfg_call")) "a refused start installed nothing")
    (is (null nodecode-letsfg::*session*) "and nothing was dialled")))

;;; --- one call is a real exchange ----------------------------------------------------

(deftest letsfg-cell-a-call-carries-the-token-and-the-session ()
  (with-letsfg-wire
    (with-cell-stop ((letsfg-start "token_file" (letsfg-token-file)))
      (setf *letsfg-text* "Warsaw is served by WAW and WMI.")
      (is (equal "Warsaw is served by WAW and WMI."
                 (letsfg-tool "letsfg_call" (nlk:json-object
                                              "tool" "find_airports"
                                              "arguments" (nlk:json-object "query" "Warsaw")))))
      (let ((call (letsfg-request "tools/call" "find_airports")))
        (is-present (call call) "a tools/call went out"
          (is (equal "Warsaw" (nlk:json-value (getf call :request) :string "params" "arguments" "query")) "its arguments went with it")
          (is (equal "Bearer lg_test_token" (letsfg-header call "authorization")) "the bearer token the section named")
          (is (equal "sess-1" (letsfg-header call "mcp-session-id")) "the session the handshake answered")))
      (is (= 1 (count "initialize" *letsfg-calls*
                      :key (lambda (one) (nlk:json-value (getf one :request) :string "method"))
                      :test #'equal)) "one handshake for the life of the start")
      (is (letsfg-request "notifications/initialized") "and the notification the protocol requires")
      (is (equal (format nil "~a/mcp" "https://letsfg.co") (getf (first *letsfg-calls*) :url)) "every request goes to the one endpoint"))))

(deftest letsfg-cell-a-search-keeps-what-was-given-and-drops-what-was-not ()
  (with-letsfg-wire
    (with-cell-stop ((letsfg-start "token_file" (letsfg-token-file)))
      (letsfg-tool "letsfg_search_flights"
                   (nlk:json-object "origin" "WAW" "destination" "LHR"
                                    "departure_date" "2026-12-04"
                                    "return_date" nil "adults" 1))
      (let ((call (letsfg-request "tools/call" "search_flights")))
        (is-present (sent (and call (nlk:json-value (getf call :request) :object "params" "arguments"))) "the search went to letsfg's search_flights"
          (is (equal "WAW" (nlk:json-value sent :string "origin")))
          (is (equal "LHR" (nlk:json-value sent :string "destination")))
          (is (= 1 (nlk:json-value sent :integer "adults")))
          (is (null (nlk:json-value sent :any "return_date")) "a date the caller left null is not sent")))
      (letsfg-tool "letsfg_search_hotels"
                   (nlk:json-object "city" "Paris" "check_in" "2026-11-12" "check_out" "2026-11-14"))
      (is (letsfg-request "tools/call" "search_hotels") "a hotel search names its own tool"))))

(deftest letsfg-cell-results-reads-the-kind-it-was-asked-for ()
  (with-letsfg-wire
    (with-cell-stop ((letsfg-start "token_file" (letsfg-token-file)))
      (letsfg-tool "letsfg_results" (nlk:json-object "search_id" "s-1"))
      (is (letsfg-request "tools/call" "get_flight_results") "a flight search_id reads as flights")
      (letsfg-tool "letsfg_results" (nlk:json-object "search_id" "s-2" "kind" "hotel"))
      (let ((call (letsfg-request "tools/call" "get_hotel_results")))
        (is-present (sent (and call (nlk:json-value (getf call :request) :object "params" "arguments"))) "a hotel search_id reads as hotels"
          (is (equal "s-2" (nlk:json-value sent :string "search_id"))))))))

(deftest letsfg-cell-the-booking-path-passes-its-arguments-unchanged ()
  (with-letsfg-wire
    (with-cell-stop ((letsfg-start "token_file" (letsfg-token-file)))
      (setf *letsfg-text* "confirmation_required: press Approve in the e-mail.")
      (is (equal "confirmation_required: press Approve in the e-mail."
                 (letsfg-tool "letsfg_book"
                              (nlk:json-object "tool" "book_flight"
                                               "arguments" (nlk:json-object "booking_handle" "h-1"
                                                                             "cabin_bag" t)))))
      (let ((call (letsfg-request "tools/call" "book_flight")))
        (is-present (sent (and call (nlk:json-value (getf call :request) :object "params" "arguments"))) "book_flight ran with the arguments it was given"
          (is (equal "h-1" (nlk:json-value sent :string "booking_handle")))))
      (is (search "one of" (refusal-text nodecode-letsfg:letsfg-error
                                          (letsfg-tool "letsfg_book" (nlk:json-object "tool" "get_flight_results")))) "a read tool is not the write path"))))

(deftest letsfg-cell-an-event-stream-answer-parses-like-a-plain-one ()
  (with-letsfg-wire
    (with-cell-stop ((letsfg-start "token_file" (letsfg-token-file)))
      (setf *letsfg-sse* t *letsfg-text* "Lisbon in November, four nights, from 212 EUR.")
      (is (equal "Lisbon in November, four nights, from 212 EUR."
                 (letsfg-tool "letsfg_call" (nlk:json-object "tool" "search_hotels"
                                                               "arguments" (nlk:json-object "city" "Lisbon")))) "the data: line carried the answer"))))

(deftest letsfg-cell-failures-read-as-failures ()
  (with-letsfg-wire
    (with-cell-stop ((letsfg-start "token_file" (letsfg-token-file)))
      (setf *letsfg-text* "no offers for that date" *letsfg-failed* t)
      (is (equal "ERROR: no offers for that date"
                 (letsfg-tool "letsfg_call" (nlk:json-object "tool" "get_flight_results"
                                                               "arguments" (nlk:json-object)))) "a tool's own error is the answer, marked ERROR:")
      (setf *letsfg-failed* nil *letsfg-status* 401)
      (is (search "401" (refusal-text nodecode-letsfg:letsfg-error
                                       (letsfg-tool "letsfg_call" (nlk:json-object "tool" "get_preferences")))) "a lapsed sign-in is a refusal naming the status"))))

(deftest letsfg-cell-the-slash-command-answers-without-the-network ()
  (with-letsfg-wire
    (with-cell-stop ((letsfg-start "token_file" (letsfg-token-file)))
      (is-carrying (text (cell-entry "nodecode-letsfg" "letsfg" ""))
        "LetsFG at"
        "https://letsfg.co/mcp"
        "token read")
      (is (null *letsfg-calls*) "the status line dials nothing")
      (is-carrying (text (cell-entry "nodecode-letsfg" "letsfg" "tools"))
        "4 tools"
        "search_flights")
      (is (letsfg-request "tools/list") "/letsfg tools asks the server for its list"))))

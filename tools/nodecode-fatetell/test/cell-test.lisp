;;;; cell-test.lisp --- lifecycle, the eight tools, /fatetell, and the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring: the section (a healthy
;;;; start with no token, a member under its floor, a root without a scheme
;;;; and an access_token_env naming an empty variable refused), the MCP wire
;;;; (initialize, the initialized notification, then one tools/call carrying
;;;; this cell's arguments), each tool's mapping onto the server tool it
;;;; names, the answer read out of result.content[], a 401 and an isError
;;;; answer turned into a repair the model can act on, and /fatetell with and
;;;; without an argument. Every request is a stubbed NLK:HTTP, so nothing
;;;; reaches FateTell or the network.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *fatetell-requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *fatetell-call-answer* nil
  "What a stubbed tools/call answers: a function of (NAME ARGUMENTS) =>
(values BODY STATUS).")

(defun fatetell-config (&rest pairs)
  "A whole shared config with one `fatetell' section built from PAIRS."
  (nlk:json-object "fatetell" (apply #'nlk:make-json-object pairs)))

(defun fatetell-start (&rest pairs)
  "START-CELL against a literal config, the way the gateway calls it."
  (nodecode-fatetell:start-cell (apply #'fatetell-config pairs)))

(defun fatetell-framed (object)
  "OBJECT as the endpoint sends it: one server-sent event."
  (format nil "event: message~%data: ~a" (nlk:encode-json-object object)))

(defun fatetell-result (&optional (text "a chart") &key (is-error :false))
  "A JSON-RPC answer carrying one text block, as tools/call answers."
  (nlk:json-object "jsonrpc" "2.0" "id" 2
                    "result" (nlk:json-object
                              "content" (vector (nlk:json-object "type" "text" "text" text))
                              "isError" is-error)))

(defun fatetell-initialized ()
  "The initialize answer: FateTell's own, cut to the fields a client reads."
  (fatetell-framed
   (nlk:json-object "jsonrpc" "2.0" "id" 1
                     "result" (nlk:json-object
                               "protocolVersion" "2025-06-18"
                               "capabilities" (nlk:json-object
                                               "tools" (nlk:json-object "listChanged" :false))
                               "serverInfo" (nlk:json-object
                                             "name" "fatetell" "title" "FateTell"
                                             "version" "0.1.0")))))

(defun fatetell-request-body (request)
  "REQUEST's JSON body, decoded."
  (nlk:decode-json (getf (cddr request) :content)))

(defun fatetell-request-method (request)
  "REQUEST's JSON-RPC method."
  (nlk:json-value (fatetell-request-body request) :string "method"))

(defun fatetell-request-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun fatetell-calls ()
  "The stubbed tools/call requests, oldest first."
  (remove-if-not (lambda (request) (equal "tools/call" (fatetell-request-method request)))
                 (reverse *fatetell-requests*)))

(defun fatetell-last-call ()
  "The newest stubbed tools/call request."
  (car (last (fatetell-calls))))

(defun fatetell-called-tool (request)
  "REQUEST's tools/call name."
  (nlk:json-value (nlk:json-value (fatetell-request-body request) :object "params")
                  :string "name"))

(defun fatetell-called-arguments (request)
  "REQUEST's tools/call arguments."
  (nlk:json-value (nlk:json-value (fatetell-request-body request) :object "params")
                  :object "arguments"))

(defmacro with-fatetell-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *FATETELL-REQUESTS* and
answered by the MCP dispatcher, so no test dials FateTell. UIOP:GETENV answers
nothing either, so a token in the machine's environment cannot reach a test
that did not ask for one."
  `(let ((*fatetell-requests* '())
         (*fatetell-call-answer* (lambda (name arguments)
                                    (declare (ignore name arguments))
                                    (values (fatetell-framed (fatetell-result)) 200))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *fatetell-requests*)
                                (let* ((request (first *fatetell-requests*))
                                       (body (fatetell-request-body request)))
                                  (cond ((equal "initialize" (nlk:json-value body :string "method"))
                                         (values (fatetell-initialized) 200 nil))
                                        ((equal "notifications/initialized"
                                                (nlk:json-value body :string "method"))
                                         (values "" 202 nil))
                                        (t
                                         (funcall *fatetell-call-answer*
                                                  (nlk:json-value (nlk:json-value body :object "params")
                                                                   :string "name")
                                                  (nlk:json-value (nlk:json-value body :object "params")
                                                                   :object "arguments"))))))
       (with-stubbed-fdefinition (uiop:getenv (name)
                                  (declare (ignore name))
                                  nil)
         ,@body))))

(defun fatetell-tool (name arguments)
  "NAME called through the wire with the JSON object ARGUMENTS, as a provider
sends it -- the contract decoded, the hooks run; => the result text."
  (assert (nle::find-tool name) () "no tool named ~a" name)
  (nle::message-content (execute-wire-call "call-1" name (nlk:encode-json-object arguments))))

;;; --- lifecycle ----------------------------------------------------------------

(define-test-slice "fatetell" "FATETELL-CELL-")

(define-cell-lifecycle-tests "fatetell"
  (:fixture with-fatetell-wire)
  (:help :fatetell)
  (:command "fatetell")
  (:running (is (nle::find-tool "fatetell_chart") "fatetell_chart is registered")
            (is (nle::find-tool "fatetell_create") "fatetell_create is registered")
            (is (equal "https://mcp.fatetell.com/mcp" (nodecode-fatetell::setting :url)) "the section's default endpoint")
            (is (null (nodecode-fatetell::setting :token)) "no token: the anonymous tools answer")
            (is (= 60 (nodecode-fatetell::setting :timeout-seconds)) "one call's ceiling")
            (is (eq t (nodecode-fatetell::setting :initialize)) "the handshake is on"))
  (:stopped (is (null (nle::find-tool "fatetell_chart")) "stop unregisters the tools")
            (is (null (nle::find-tool "fatetell_almanac")) "both of them"))
  (:refused ("timeout_seconds" 0) ("mcp_url" "mcp.fatetell.com")
            ("access_token_env" "FATETELL_TEST_TOKEN"))
  (:idle nodecode-fatetell:fatetell-error (nodecode-fatetell::run-command "" nil)))

(deftest fatetell-cell-a-named-but-empty-token-variable-refuses ()
  (with-fatetell-wire
    (is-carrying (answer (refusal-text nlk:config-refusal
                                        (fatetell-start "access_token_env" "FATETELL_TEST_TOKEN")))
                 ("access_token_env names FATETELL_TEST_TOKEN" "the refusal names the variable")
                 ("need no account" "and says which tools still work"))
    (is (null (nle::find-tool "fatetell_chart")) "a refused start installed nothing")))

(deftest fatetell-cell-a-token-is-sent-as-a-bearer ()
  (with-fatetell-wire
    (with-cell-stop ((fatetell-start "access_token" "t-test"))
      (fatetell-tool "fatetell_saved" (nlk:json-object))
      (is (equal "Bearer t-test" (fatetell-request-header (fatetell-last-call) "authorization")) "the section's token on the tools/call"))))

;;; --- the wire ----------------------------------------------------------------

(deftest fatetell-cell-a-call-handshakes-then-asks ()
  (with-fatetell-wire
    (with-cell-stop ((fatetell-start))
      (fatetell-tool "fatetell_almanac" (nlk:json-object "days" 2))
      (let ((methods (mapcar #'fatetell-request-method (reverse *fatetell-requests*))))
        (is (equal '("initialize" "notifications/initialized" "tools/call") methods) "the handshake, its notification, then the call"))
      (is (equal "https://mcp.fatetell.com/mcp" (second (fatetell-last-call))) "the section's endpoint")
      (is (equal "application/json, text/event-stream" (fatetell-request-header (fatetell-last-call) "accept")) "both answer shapes accepted"))))

(deftest fatetell-cell-the-handshake-can-be-turned-off ()
  (with-fatetell-wire
    (with-cell-stop ((fatetell-start "initialize" nil))
      (fatetell-tool "fatetell_almanac" (nlk:json-object))
      (is (= 1 (length (fatetell-calls))) "one call, no handshake")
      (is (equal "tools/call" (fatetell-request-method (fatetell-last-call)))))))

(deftest fatetell-cell-an-event-stream-answer-reads ()
  (with-fatetell-wire
    (with-cell-stop ((fatetell-start))
      (setf *fatetell-call-answer*
            (lambda (name arguments)
              (declare (ignore name arguments))
              (values (fatetell-framed (fatetell-result "today: 甲子 day")) 200)))
      (is (equal "today: 甲子 day" (fatetell-tool "fatetell_almanac" (nlk:json-object))) "the text block, out of the event stream"))))

;;; --- fatetell_chart --------------------------------------------------------------

(deftest fatetell-cell-a-chart-carries-its-birth-as-an-object ()
  (with-fatetell-wire
    (with-cell-stop ((fatetell-start))
      (fatetell-tool "fatetell_chart"
                      (nlk:json-object "date" "1990-05-17" "timezone" "Asia/Taipei"
                                        "gender" "female" "hemisphere" "north"
                                        "time" "08:30"))
      (let* ((request (fatetell-last-call))
             (arguments (fatetell-called-arguments request))
             (birth (nlk:json-value arguments :object "birth")))
        (is (equal "cast_bazi_chart" (fatetell-called-tool request)))
        (is (equal "1990-05-17" (nlk:json-value birth :string "date")) "the birth date")
        (is (equal "Asia/Taipei" (nlk:json-value birth :string "timezone")) "the time zone")
        (is (equal "female" (nlk:json-value birth :string "gender")) "the gender")
        (is (equal "north" (nlk:json-value birth :string "hemisphere")) "the hemisphere")
        (is (equal "08:30" (nlk:json-value birth :string "time")) "the birth time")
        (is (search "\"use_true_solar_time\":false" (getf (cddr request) :content)) "an untouched switch is sent as false, not left to the server")
        (is (search "\"lunar_leap_month\":false" (getf (cddr request) :content)) "that one too")
        (is (eq t (gethash "include_today" arguments)) "today's pillars, as the default says")))))

(deftest fatetell-cell-a-chart-refuses-without-its-birth-fields ()
  (with-fatetell-wire
    (with-cell-stop ((fatetell-start))
      (is-carrying (answer (fatetell-tool "fatetell_chart" (nlk:json-object "date" "1990-05-17")))
                   ("timezone" "the missing member is named"))
      (is (null (fatetell-calls)) "no call was made"))))

;;; --- the account tools ----------------------------------------------------------

(deftest fatetell-cell-saved-lists-then-reads-one ()
  (with-fatetell-wire
    (with-cell-stop ((fatetell-start))
      (fatetell-tool "fatetell_saved" (nlk:json-object))
      (is (equal "list_saved_charts" (fatetell-called-tool (fatetell-last-call))) "no chart_id lists them")
      (fatetell-tool "fatetell_saved" (nlk:json-object "chart_id" "self"))
      (let* ((request (fatetell-last-call))
             (arguments (fatetell-called-arguments request)))
        (is (equal "get_saved_chart" (fatetell-called-tool request)) "a chart_id reads one")
        (is (equal "self" (nlk:json-value arguments :string "chart_id")))
        (is (eq t (gethash "include_today" arguments)) "today's pillars by default")))))

(deftest fatetell-cell-each-tool-names-its-own-server-tool ()
  (with-fatetell-wire
    (with-cell-stop ((fatetell-start))
      (flet ((called (name arguments)
               (fatetell-tool name arguments)
               (fatetell-called-tool (fatetell-last-call))))
        (is (equal "get_daily_fortune"
                   (called "fatetell_fortune" (nlk:json-object "chart_id" "self" "date" "2026-01-31"))))
        (is (equal "get_chart_calendar"
                   (called "fatetell_calendar" (nlk:json-object "chart_id" "self" "granularity" "month"
                                                                 "count" 3))))
        (is (equal "create_saved_chart"
                   (called "fatetell_create" (nlk:json-object "display_name" "Me" "gender" "male"
                                                               "birth_date" "1990-05-17"
                                                               "birthplace" "Taipei, Taiwan"))))))))

(deftest fatetell-cell-a-judgment-takes-a-chart-id-or-a-birth ()
  (with-fatetell-wire
    (with-cell-stop ((fatetell-start))
      (fatetell-tool "fatetell_judgment" (nlk:json-object "chart_id" "self"))
      (let ((arguments (fatetell-called-arguments (fatetell-last-call))))
        (is (equal "self" (nlk:json-value arguments :string "chart_id")) "a saved chart's verdict")
        (is (null (nlk:json-value arguments :object "birth")) "nothing re-cast"))
      (fatetell-tool "fatetell_judgment"
                      (nlk:json-object "date" "1990-05-17" "timezone" "Asia/Taipei"
                                        "gender" "male" "hemisphere" "north"))
      (let* ((arguments (fatetell-called-arguments (fatetell-last-call)))
             (birth (nlk:json-value arguments :object "birth")))
        (is (equal "1990-05-17" (nlk:json-value birth :string "date")) "birth data cast on the spot")
        (is (null (nlk:json-value arguments :string "chart_id")) "with no chart_id")))
    (with-cell-stop ((fatetell-start))
      (is-carrying (answer (fatetell-tool "fatetell_judgment" (nlk:json-object)))
                   ("needs a chart_id" "neither given is a refusal the model can repair")))))

;;; --- what a failed answer becomes ------------------------------------------------

(deftest fatetell-cell-a-sign-in-challenge-says-what-to-do ()
  (with-fatetell-wire
    (with-cell-stop ((fatetell-start))
      (setf *fatetell-call-answer*
            (lambda (name arguments)
              (declare (ignore name arguments))
              (values "{\"error\":\"invalid_token\"}" 401)))
      (is-carrying (answer (fatetell-tool "fatetell_saved" (nlk:json-object)))
                   ("ERROR: FateTell needs a signed-in account") ("access_token")
                   ("the almanac need no account" "the anonymous tools are named")))))

(deftest fatetell-cell-a-server-error-is-a-repair-not-a-crash ()
  (with-fatetell-wire
    (with-cell-stop ((fatetell-start))
      (setf *fatetell-call-answer*
            (lambda (name arguments)
              (declare (ignore name arguments))
              (values (fatetell-framed (fatetell-result "date must be YYYY-MM-DD" :is-error t)) 200)))
      (is-carrying (answer (fatetell-tool "fatetell_almanac" (nlk:json-object "start_date" "17th")))
                   ("ERROR: get_almanac") ("date must be YYYY-MM-DD")))))

;;; --- /fatetell -------------------------------------------------------------------

(deftest fatetell-cell-the-command-asks-for-the-almanac ()
  (with-fatetell-wire
    (with-cell-stop ((fatetell-start))
      (let ((status (cell-entry "nodecode-fatetell" "fatetell" nil "s1")))
        (is (search "https://mcp.fatetell.com/mcp" status) "no argument says where it points")
        (is (search "without a token" status) "and that no account answers with it"))
      (cell-entry "nodecode-fatetell" "fatetell" "account" "s1")
      (is (equal "get_fatetell_account" (fatetell-called-tool (fatetell-last-call))) "/fatetell account asks who is signed in")
      (cell-entry "nodecode-fatetell" "fatetell" "2026-01-31 3" "s1")
      (let* ((request (fatetell-last-call))
             (arguments (fatetell-called-arguments request)))
        (is (equal "get_almanac" (fatetell-called-tool request)) "the almanac")
        (is (equal "2026-01-31" (nlk:json-value arguments :string "start_date")) "from the date typed")
        (is (= 3 (nlk:json-value arguments :integer "days")) "for the days asked")))))

;;;; cell-test.lisp --- the cell against a stubbed Astro Agents MCP server.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every request is a stubbed NLK:HTTP, so nothing reaches the server, no
;;;; credit is spent and no chart is computed. What is proved is the cell's own
;;;; wiring: the section (a healthy start, a refused missing endpoint), the
;;;; four tools through the dispatch the model calls them by -- the server tool
;;;; each one names and the arguments it sends -- the JSON-RPC wire, and the
;;;; /astro command.

(in-package #:nodecode.test)

(define-test-slice "astro" "ASTRO-CELL-" :start nodecode-astro:start-cell)

;;; --- fixtures --------------------------------------------------------------------

(defparameter *calls* nil
  "Every request the stub answered, as (METHOD PARAMS HEADERS).")

(defparameter *answer* nil
  "When set, (TEXT STATUS HEADERS): what the stub answers to tools/call.")

(defun envelope (id result)
  "One JSON-RPC answer carrying RESULT, as the wire spells it."
  (nlk:encode-json-object
   (nlk:json-object "jsonrpc" "2.0" "id" id "result" result)))

(defun text-result (text &key errorp)
  "One tools/call result carrying TEXT."
  (nlk:json-object "content" (vector (nlk:json-object "type" "text" "text" text))
                   "isError" (and errorp t)))

(defun chart-result (params)
  "What the stub's tools/call answers: a chart-shaped structuredContent naming
the server tool that answered and echoing its arguments."
  (let* ((tool (nlk:json-value params :string "name"))
         (arguments (nlk:json-value params :object "arguments"))
         (structured (nlk:json-object
                      "endpoint" (format nil "western/~a" tool)
                      "result" (nlk:json-object "echo" (or arguments (nlk:make-json-object)))
                      "meta" (nlk:json-object "deterministic" t "llm_used" nil
                                               "result_sha256" "3f2ab9f1"))))
    (nlk:json-object "content" (vector (nlk:json-object "type" "text"
                                                         "text" (nlk:encode-json-object structured)))
                     "structuredContent" structured
                     "isError" nil)))

(defun stub-wire (options)
  "NLK:HTTP, answering from the request's own method; nothing leaves the machine."
  (let* ((body (nlk:decode-json (or (getf options :content) "{}")))
         (name (nlk:json-value body :text "method"))
         (params (nlk:json-value body :object "params"))
         (id (nlk:json-value body :integer "id")))
    (push (list name params (getf options :headers) (getf options :url)) *calls*)
    (cond ((equal name "initialize")
           (values (envelope id (nlk:json-object "protocolVersion" "2025-06-18"
                                                   "capabilities" (nlk:make-json-object)))
                   200 nil nil))
          ((equal name "notifications/initialized")
           (values "" 202 nil nil))
          ((equal name "tools/call")
           (if *answer*
               (values (first *answer*) (second *answer*) (third *answer*) nil)
               (values (envelope id (chart-result params)) 200 nil nil)))
          (t (values "{}" 404 nil nil)))))

(defmacro with-astro-wire (&body body)
  "BODY with NLK:HTTP stubbed: every request is logged and answered here."
  `(let ((*calls* '())
         (*answer* nil))
     (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                (let ((options (list* :url url options)))
                                  (stub-wire options)))
       ,@body)))

(defmacro with-astro-running (&body body)
  "BODY with the cell started on a literal section, stopped on unwind."
  `(with-cell-stop ((astro-start "endpoint" "https://astro-agent.dev/mcp"))
     ,@body))

(defun astro-request (method)
  "The newest stubbed request of JSON-RPC METHOD, or NIL."
  (find method *calls* :test #'equal :key #'first))

(defun astro-request-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (third request) :test #'string-equal)))

(defun astro-tool-call (&optional (index 0))
  "The INDEX'th newest tools/call the cell sent, as (METHOD PARAMS HEADERS URL)."
  (nth index (remove-if-not (lambda (call) (equal (first call) "tools/call")) *calls*)))

(defun astro-sent-tool (&optional (index 0))
  "The server tool name the INDEX'th newest tools/call named."
  (let ((call (astro-tool-call index)))
    (and call (nlk:json-value (second call) :string "name"))))

(defun astro-sent-arguments (&optional (index 0))
  "The arguments object of the INDEX'th newest tools/call."
  (let ((call (astro-tool-call index)))
    (and call (nlk:json-value (second call) :object "arguments"))))

(defun astro-called (name arguments)
  "NAME called the way the model calls it, as the text the model reads back."
  (nle::message-content (execute-wire-call "astro-call" name arguments)))

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "astro"
  (:fixture with-astro-wire)
  (:config "endpoint" "https://astro-agent.dev/mcp")
  (:help :astro)
  (:command "astro")
  (:running
   (is (nle::find-tool "astro_chart") "the chart tool is registered")
   (is (nle::find-tool "astro_sky") "and the sky")
   (is (nle::find-tool "astro_kundli") "and the kundli")
   (is (nle::find-tool "astro_call") "and the one that calls any other tool")
   (is (= 4 (count-if (lambda (entry) (eql 0 (search "astro_" (first entry))))
                      nle::*tools*)) "four of them, and no others"))
  (:stopped
   (is (null (nle::find-tool "astro_chart")) "and stop takes every one back")
   (is (null (nle::find-tool "astro_call"))))
  (:refused ()))

(deftest astro-cell-a-start-with-no-endpoint-refuses ()
  ;; The endpoint is the only required member: without it there is nothing to reach.
  (with-astro-wire
    (let ((text (refusal-text nlk:config-refusal (astro-start))))
      (is-carrying text "endpoint" "astro-agent.dev/mcp")
      (is (null (nle::find-tool "astro_chart")) "nothing was installed"))))

(deftest astro-cell-an-endpoint-that-is-not-https-refuses ()
  (with-astro-wire
    (is-carrying (text (refusal-text nlk:config-refusal (astro-start "endpoint" "http://example.test/mcp")))
                 "https")))

;;; --- the wire ---------------------------------------------------------------------

(deftest astro-cell-one-initialize-serves-every-call (with-astro-wire)
  (with-astro-running
    (astro-called "astro_sky" "{}")
    (astro-called "astro_kundli" "{\"latitude\": 48.85, \"longitude\": 2.35}")
    (is (= 1 (count-if (lambda (call) (equal (first call) "initialize")) *calls*)) "initialize happens once and is reused")
    (let ((call (astro-tool-call)))
      (is (equal "application/json" (astro-request-header call "content-type")) "a JSON-RPC POST")
      (is-carrying (accept (astro-request-header call "accept"))
                   ("text/event-stream" "an answer may arrive as a stream"))
      (is (equal "https://astro-agent.dev/mcp" (fourth call)) "to the endpoint the section names")
      (is (null (astro-request-header call "x-credits-token")) "and with no credit header when none is set"))))

(deftest astro-cell-a-credits-token-rides-every-call ()
  (with-astro-wire
    (with-cell-stop ((astro-start "endpoint" "https://astro-agent.dev/mcp"
                                  "credits_token" "cr-1"))
      (astro-called "astro_sky" "{}")
      (is (equal "cr-1" (astro-request-header (astro-tool-call) "x-credits-token")) "a prepaid token is sent as the header the server pays from"))))

(deftest astro-cell-an-answer-says-what-answered-and-its-hash (with-astro-wire)
  (with-astro-running
    (is-carrying (text (astro-called "astro_sky" "{}"))
                 ("sky_today" "the endpoint names the server tool")
                 ("result_sha256" "the hash that proves the result")
                 "3f2ab9f1")))

(deftest astro-cell-a-paid-tool-says-what-to-do-about-it (with-astro-wire)
  (with-astro-running
    (setf *answer* (list (envelope 1 (text-result
                                      "payment_required: natal_chart costs $0.15, pay at https://astro-agent.dev/v1/western/natal"
                                      :errorp t))
                         200 nil))
    (let ((answer (astro-called "astro_chart"
                                 "{\"datetime\": \"1990-05-15T14:30\", \"latitude\": 48.8566, \"longitude\": 2.3522}")))
      (is-carrying answer "payment_required" "$0.15" "credits_token" "free call"
                    (:absent "retry it twice"))
      (is (= 1 (count-if (lambda (call) (equal (first call) "tools/call")) *calls*)) "the call is not retried"))))

;;; --- the tools say what the skill says ---------------------------------------------

(deftest astro-cell-a-chart-sends-the-birth-data (with-astro-wire)
  (with-astro-running
    (astro-called "astro_chart"
                   "{\"datetime\": \"1990-05-15T14:30\", \"latitude\": 48.8566, \"longitude\": 2.3522}")
    (is (equal "natal_chart" (astro-sent-tool)) "kind natal is the server's natal_chart")
    (let ((sent (astro-sent-arguments)))
      (is (equal "1990-05-15T14:30" (nlk:json-value sent :text "datetime")) "the local clock time, not a UTC one")
      (is (= 48.8566 (nlk:json-value sent :number "latitude")))
      (is (= 2.3522 (nlk:json-value sent :number "longitude"))))))

(deftest astro-cell-a-chart-can-ask-for-positions-or-aspects (with-astro-wire)
  (with-astro-running
    (astro-called "astro_chart" "{\"kind\": \"positions\"}")
    (is (equal "planet_positions" (astro-sent-tool)) "positions is planet_positions")
    (astro-called "astro_chart" "{\"kind\": \"aspects\", \"datetime\": \"1990-05-15T14:30\"}")
    (is (equal "aspects" (astro-sent-tool)) "aspects is aspects")))

(deftest astro-cell-a-chart-of-an-unknown-kind-is-refused (with-astro-wire)
  (with-astro-running
    (is-carrying (text (astro-called "astro_chart" "{\"kind\": \"horoscope\"}"))
                 "natal" "positions" "aspects")
    (is (null (astro-tool-call)) "and nothing was asked of the server")))

(deftest astro-cell-a-natal-chart-without-a-place-is-refused (with-astro-wire)
  (with-astro-running
    (is-carrying (text (astro-called "astro_chart" "{\"datetime\": \"1990-05-15T14:30\"}"))
                 "latitude" "longitude")
    (is (null (astro-tool-call)) "nothing to chart without a place")))

(deftest astro-cell-the-sky-asks-for-the-days-it-was-given (with-astro-wire)
  (with-astro-running
    (astro-called "astro_sky" "{\"days\": 7}")
    (is (equal "sky_today" (astro-sent-tool)) "the sky needs no birth data")
    (is (= 7 (nlk:json-value (astro-sent-arguments) :integer "days")))
    (astro-called "astro_sky" "{}")
    (is (= 30 (nlk:json-value (astro-sent-arguments) :integer "days")) "30 days unless asked otherwise")))

(deftest astro-cell-a-kundli-asks-for-the-vedic-chart (with-astro-wire)
  (with-astro-running
    (astro-called "astro_kundli" "{\"latitude\": 28.6139, \"longitude\": 77.209}")
    (is (equal "kundli" (astro-sent-tool)) "D1 and D9 by default")
    (astro-called "astro_kundli" "{\"latitude\": 28.6139, \"longitude\": 77.209, \"full\": true}")
    (is (equal "kundli_full" (astro-sent-tool)) "full asks for all sixteen divisional charts")))

(deftest astro-cell-any-tool-passes-its-arguments-through (with-astro-wire)
  (with-astro-running
    (astro-called "astro_call"
                   "{\"tool\": \"transits\", \"arguments\": {\"natal\": {\"datetime\": \"1990-05-15T14:30\"}, \"window\": {\"days\": 30}}}")
    (is (equal "transits" (astro-sent-tool)) "the server tool the model named")
    (let ((sent (astro-sent-arguments)))
      (is (hash-table-p (nlk:json-value sent :object "natal")) "its own arguments go through untouched")
      (is (hash-table-p (nlk:json-value sent :object "window")) "and the whole object, not a summary"))))

(deftest astro-cell-any-tool-refuses-a-name-the-server-does-not-have (with-astro-wire)
  (with-astro-running
    (is-carrying (text (astro-called "astro_call" "{\"tool\": \"birth_chart\"}"))
                 "astro_catalog" "not one of the server's tools")
    (is (null (astro-tool-call)) "and nothing was sent")))

;;; --- the operator's command ---------------------------------------------------------

(deftest astro-cell-the-command-reads-todays-sky (with-astro-wire)
  (with-astro-running
    (nodecode-astro::run-command "" nil)
    (is (equal "sky_today" (astro-sent-tool)) "no argument is today's sky")))

(deftest astro-cell-the-command-lists-the-tools (with-astro-wire)
  (with-astro-running
    (is-carrying (text (nodecode-astro::run-command "tools" nil))
                 "sky_today" "transits" "astro_call" "Free call")
    (is (null (astro-tool-call)) "the list costs nothing")))

(deftest astro-cell-the-command-cuts-a-chart-from-a-line (with-astro-wire)
  (with-astro-running
    (nodecode-astro::run-command "1990-05-15T14:30 48.8566 2.3522" nil)
    (is (equal "natal_chart" (astro-sent-tool)))
    (is (< (abs (- 48.8566 (nlk:json-value (astro-sent-arguments) :number "latitude"))) 1d-3) "the place the operator typed"))
  (with-astro-running
    (is-carrying (text (nodecode-astro::run-command "1990-05-15T14:30" nil)) "usage")
    (is (= 1 (count-if (lambda (call) (equal (first call) "tools/call")) *calls*)) "half a line is not a chart")))

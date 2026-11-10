;;;; cell-test.lisp --- the api-indonesia cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The key is a stubbed ENVIRONMENT-VALUE and the server a stubbed HTTP
;;;; answering by JSON-RPC method, or by URL for the REST fallback; the tool
;;;; list and one answer are fixtures. Nothing here reaches the network, the
;;;; environment or the operator's home.

(in-package #:nodecode.test)

(define-test-slice "api-indonesia" "API-INDONESIA-CELL-" :start nodecode-api-indonesia:start-cell)

;;; --- fixtures ------------------------------------------------------------------

(defmacro with-api-indonesia-key (&body body)
  "BODY with the variable the section names answering a test key."
  `(with-stubbed-fdefinition (nodecode-api-indonesia::environment-value (name)
                             (if (equal name "API_INDONESIA_KEY") "aip_live_test" nil))
     ,@body))

(defun api-indonesia-headers (&key session (type "application/json"))
  "A response's header table, as dexador hands one back: lowercase names."
  (let ((table (make-hash-table :test #'equal)))
    (setf (gethash "content-type" table) type)
    (when session (setf (gethash "mcp-session-id" table) session))
    table))

(defun api-indonesia-reply (id result &key (status 200) session (type "application/json"))
  "One JSON-RPC result, as the server answers it."
  (values (nlk:encode-json-object (nlk:json-object "jsonrpc" "2.0" "id" id "result" result))
          status (api-indonesia-headers :session session :type type)))

(defun api-indonesia-tool (name &key (description "a fixture tool"))
  (nlk:json-object "name" name "description" description
                    "inputSchema" (nlk:json-object "type" "object")))

(defparameter +two-tools+
  (vector (api-indonesia-tool "wilayah_provinsi" :description "List the 38 provinces.")
          (api-indonesia-tool "libur_nasional" :description "National holidays for a year."))
  "What tools/list answers in a test.")

(defparameter +provinces+
  "{\"data\":[{\"id\":\"32\",\"name\":\"JAWA BARAT\"}],\"meta\":{\"source\":\"Kemendagri\"}}"
  "A REST answer, as the API writes one.")
(defparameter +one-holiday+ (nlk:json-object "content" (vector (nlk:json-object "type" "text" "text" "1 Januari: Tahun Baru"))))

(defvar *asked* '()
  "The stubbed requests of one test, newest first.")

(defvar *rest* (lambda (url) (declare (ignore url)) (error "no REST stub in this test"))
  "How a :GET is answered in one test: a function of the URL answering (values BODY STATUS HEADERS).")

(defmacro with-api-indonesia-wire ((&rest routes) &body body)
  "BODY with NLK:HTTP stubbed and the cell started on a test key. ROUTES are
RPC-METHOD FORM pairs: the first whose METHOD the request asks answers
FORM's (values TEXT STATUS HEADERS), with ID bound to the request's id. An
initialize and a notification answer a bare success unless a route says
otherwise; a method no route names is an error. A :GET goes to *REST*."
  `(let ((*asked* '()))
     (with-api-indonesia-key
       (with-stubbed-fdefinition (nodecode-api-indonesia::http (method url &rest args)
         (push (list* method url args) *asked*)
         (if (eq method :get)
             (funcall *rest* url)
             (let* ((message (nlk:decode-json (getf args :content)))
                    (rpc (nlk:json-value message :string "method"))
                    (id (nlk:json-value message :integer "id")))
               (cond
                 ,@(loop for (name form) on routes by #'cddr
                         collect `((equal rpc ,name) ,form))
                 ((equal rpc "initialize")
                  (api-indonesia-reply id (nlk:json-object "protocolVersion" "2025-06-18"
                                                            "capabilities" (nlk:json-object))))
                 ((equal rpc "notifications/initialized") (values "" 202 nil))
                 (t (error "unexpected request ~a" rpc))))))
       (with-cell-stop ((api-indonesia-start))
         ,@body)))))

(defun api-indonesia-entry (name)
  (nle::find-tool name))

(defun api-indonesia-run (name arguments)
  "Run the registered tool NAME with the JSON object ARGUMENTS, as dispatch does."
  (let ((entry (api-indonesia-entry name)))
    (assert entry () "no tool named ~a" name)
    (funcall (third entry) (nlk:contract-decode (second entry) arguments))))

(defun api-indonesia-call-tool (arguments)
  (api-indonesia-run "api_indonesia_tools" arguments))

(defun api-indonesia-invoke (arguments)
  (api-indonesia-run "api_indonesia_call" arguments))

(defun api-indonesia-get (arguments)
  (api-indonesia-run "api_indonesia_get" arguments))

(defun api-indonesia-asked (&key (rpc "tools/list"))
  "The stubbed requests whose JSON-RPC method is RPC, oldest first."
  (loop for request in (reverse *asked*)
        for message = (nlk:decode-json (getf (cddr request) :content))
        when (equal rpc (nlk:json-value message :string "method"))
          collect request))

(defun api-indonesia-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun api-indonesia-body (request)
  "REQUEST's JSON-RPC message, decoded."
  (nlk:decode-json (getf (cddr request) :content)))

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "api-indonesia"
  (:fixture with-api-indonesia-key)
  (:help :api-indonesia)
  (:command "api-indonesia")
  (:running (is (api-indonesia-entry "api_indonesia_tools") "api_indonesia_tools is registered")
            (is (api-indonesia-entry "api_indonesia_call") "api_indonesia_call is registered")
            (is (api-indonesia-entry "api_indonesia_get") "api_indonesia_get is registered")
            (is (equal "https://mcp.apiindonesia.id/mcp" (nodecode-api-indonesia::setting :url)))
            (is (equal "https://use.apiindonesia.id/api/v1" (nodecode-api-indonesia::setting :rest-url)))
            (is (= 60 (nodecode-api-indonesia::setting :timeout-seconds)) "a minute a call"))
  (:stopped (is (null (api-indonesia-entry "api_indonesia_tools")) "stop unregisters the tools")
            (is (null (api-indonesia-entry "api_indonesia_call")))
            (is (null (api-indonesia-entry "api_indonesia_get"))))
  (:refused ("api_key_env" "API_INDONESIA_UNSET_FOR_THE_TEST")))

(deftest api-indonesia-cell-a-key-is-required ()
  (with-stubbed-fdefinition (nodecode-api-indonesia::environment-value (name) nil)
    (is-carrying (text (refusal-text nlk:config-refusal (api-indonesia-start)))
      ("no API key" "the refusal names the problem")
      ("API_INDONESIA_KEY" "and the variable to set"))
    (is (null (api-indonesia-entry "api_indonesia_tools")) "a refused start installed nothing")))

;;; --- api_indonesia_tools ------------------------------------------------------------

(deftest api-indonesia-cell-tools-lists-the-servers-own-tools ()
  (with-api-indonesia-wire ("tools/list" (api-indonesia-reply id (nlk:json-object "tools" +two-tools+)))
    (is-carrying (answer (api-indonesia-call-tool (nlk:json-object)))
      ("wilayah_provinsi" "a tool is listed")
      ("List the 38 provinces" "with its own description")
      ("api_indonesia_call" "and the verb that calls one"))
    (let ((request (first (api-indonesia-asked))))
      (is (= 1 (length (api-indonesia-asked))) "one tools/list, and the copy is kept")
      (is (equal "Bearer aip_live_test" (api-indonesia-header request "authorization")) "the key rides the header")
      (is (equal "application/json" (api-indonesia-header request "content-type")))
      (is (search "text/event-stream" (api-indonesia-header request "accept")) "and a stream is accepted as an answer")
      (is (equal "2025-06-18" (api-indonesia-header request "mcp-protocol-version"))))
    (api-indonesia-call-tool (nlk:json-object))
    (is (= 1 (length (api-indonesia-asked))) "a second listing asks nothing")))

;;; --- api_indonesia_call ---------------------------------------------------------------

(deftest api-indonesia-cell-a-call-passes-its-arguments ()
  (with-api-indonesia-wire ("tools/list" (api-indonesia-reply id (nlk:json-object "tools" +two-tools+))
                            "tools/call" (api-indonesia-reply id +one-holiday+))
    (is-carrying (answer (api-indonesia-invoke (nlk:json-object "tool" "libur_nasional"
                                                                 "arguments" (nlk:json-object "tahun" 2026))))
      ("1 Januari" "the answer is the server's text"))
    (let* ((calls (api-indonesia-asked :rpc "tools/call"))
           (request (first calls)))
      (is (= 1 (length calls)) "one call")
      (is (equal "libur_nasional" (nlk:json-value (api-indonesia-body request) :string "params" "name")) "the tool's name rides the params")
      (is (= 2026 (nlk:json-value (api-indonesia-body request) :integer "params" "arguments" "tahun")) "and its arguments ride untouched"))))

(deftest api-indonesia-cell-a-refused-key-says-so ()
  (with-api-indonesia-wire ("tools/list" (values "" 401 (api-indonesia-headers)))
    (is-carrying (answer (api-indonesia-call-tool (nlk:json-object)))
      ("401" "the status is reported")
      ("the variable api_key_env names" "with the one thing to check"))))

;;; --- api_indonesia_get -----------------------------------------------------------------

(deftest api-indonesia-cell-get-reads-a-documented-path ()
  (let ((*rest* (lambda (url)
                  (declare (ignore url))
                  (values +provinces+ 200 (api-indonesia-headers)))))
    (with-api-indonesia-wire ()
      (is-carrying (answer (api-indonesia-get (nlk:json-object "path" "/wilayah/provinsi"
                                                               "query" (nlk:json-object "q" "bandung"))))
        ("Kemendagri" "the JSON the API answered with is the answer")
        ("GET /wilayah/provinsi" "and the path it came from is named"))
      (let ((request (first *asked*)))
        (is (equal "https://use.apiindonesia.id/api/v1/wilayah/provinsi?q=bandung" (second request)) "the documented path under the section's rest_url, its query encoded")
        (is (equal "aip_live_test" (api-indonesia-header request "x-api-key")) "the same key, as the examples send it")))))

(deftest api-indonesia-cell-get-stays-on-the-one-host ()
  (with-api-indonesia-wire ()
    (is (equal "https://use.apiindonesia.id/api/v1/libur/upcoming"
               (nodecode-api-indonesia::rest-target "https://use.apiindonesia.id/api/v1/libur/upcoming" nil)) "a full URL contributes its path")
    (is (equal "https://use.apiindonesia.id/api/v1/x?y=1"
               (nodecode-api-indonesia::rest-target "https://elsewhere.example/x" (nlk:json-object "y" 1))) "and never another host")))

(deftest api-indonesia-cell-get-reports-a-refused-status ()
  (let ((*rest* (lambda (url)
                  (declare (ignore url))
                  (values "{\"error\":\"no credits\"}" 402 (api-indonesia-headers)))))
    (with-api-indonesia-wire ()
      (is-carrying (answer (api-indonesia-get (nlk:json-object "path" "/gempa/terkini")))
        ("402" "the status is reported")
        ("out of credits" "with what it means")))))

;;; --- the command -------------------------------------------------------------------------

(deftest api-indonesia-cell-the-command-names-the-variable ()
  (with-api-indonesia-wire ()
    (is-carrying (answer (cell-entry "nodecode-api-indonesia" "api-indonesia" "status"))
      ("API_INDONESIA_KEY" "the command names the variable holding the key")
      ("key: set" "and says whether one is set"))))

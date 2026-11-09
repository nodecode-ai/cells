;;;; cell-test.lisp --- lifecycle, all 100 tools, the two wires, the command, each refusal.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring, not FXMacroData's data: the
;;;; section (a value out of range refuses the start; no key still calls, with
;;;; no credential header), every one of the 100 tools through the real
;;;; dispatch seam against a stubbed service, the path and query the REST API
;;;; reads, the MCP handshake, the composed brief and version-pinned snapshot,
;;;; and the notices and error bodies the original relayed by hand.

(in-package #:nodecode.test)

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "fxmacrodata"
  (:fixture with-api)
  (:config "api_key" "test-key" "api_key_env" +key-env+)
  (:help :fxmacrodata)
  (:command "fxmacrodata")
  (:running
   (is (= 100 (length (fxmacrodata:tool-names))) "the original's 100 tools, no more and no fewer")
   (dolist (name (fxmacrodata:tool-names))
     (is (nle::find-tool name) (format nil "the ~a tool is registered" name))))
  (:stopped
   (dolist (name '("fxmd_forex" "fxmd_data_catalogue" "fxmd_mcp_ping" "fxmd_usd_macro_brief"))
     (is (null (nle::find-tool name)) (format nil "stop unregisters ~a" name))))
  (:refused
   ("timeout_seconds" 0)))

;;; --- every tool ----------------------------------------------------------------

(deftest fxmacrodata-cell-every-tool-answers-through-the-wire
    (with-fxm ("api_key" "test-key")
      (dolist (name (fxmacrodata:tool-names))
        (setf *calls* '())
        (let ((text (call name (minimal-arguments name))))
          (is (plusp (length text)) (format nil "~a answers text" name))
          (is (null (search "\"error\"" text)) (format nil "~a answers without an error: ~a" name
                                                       (subseq text 0 (min 200 (length text)))))))))

;;; --- the request the REST service reads -------------------------------------------

(deftest fxmacrodata-cell-the-key-rides-a-header-and-never-the-url
    (with-fxm ("api_key" "test-key")
      (call "fxmd_forex" "{\"base\":\"USD\",\"quote\":\"EUR\",\"limit\":5}")
      (is (equal "test-key" (header "X-API-Key")) "the key rides the X-API-Key header")
      (is (null (search "test-key" (newest-url))) "and never the URL")
      (is (uiop:string-prefix-p "https://api.test/fxm/v1/forex/USD/EUR" (newest-url)) "the configured origin, the route, and its path members substituted")
      (is (equal "5" (cdr (assoc "limit" (query-of (newest-url)) :test #'string=))) "and the arguments the operation declares as query members")))

(deftest fxmacrodata-cell-no-key-still-calls-and-carries-no-credential
    (with-fxm ("api_key_env" +no-key-env+)
      (call "fxmd_data_catalogue" "{\"currency\":\"USD\"}")
      (is (null (header "X-API-Key")) "no key set, no header: the public baseline")
      (is (search "/v1/data_catalogue/USD" (newest-url)) "and the public route is still called")))

(deftest fxmacrodata-cell-an-error-body-is-an-error
    (with-fxm ("api_key" "test-key")
      (script "/v1/cot/" (lambda (method url content)
                            (declare (ignore method url content))
                            (values (json-body (nlk:json-object "error" "api_key_required"))
                                    200 (response-headers))))
      (let ((text (call "fxmd_cot" "{\"currency\":\"EUR\"}")))
        (is (search "\"error\"" text) "a successful status with an error body is an error")
        (is (search "api_key_required" text) "naming the service's own code"))))

;;; --- the MCP wire ----------------------------------------------------------------

(deftest fxmacrodata-cell-mcp-handshakes-then-calls-the-tool
    (with-fxm ("api_key" "test-key")
      (setf *calls* '())
      (let ((text (call "fxmd_mcp_ping" "{}"))
            (exchanges nil))
        (setf exchanges (reverse (exchanges-to +mcp+)))
        (is (= 3 (length exchanges)) "initialize, the notification, then the call")
        (is (search "initialize" (getf (third (first exchanges)) :content)) "the session opens first")
        (is (equal "Bearer test-key" (header "Authorization" (getf (third (first exchanges)) :headers))) "the key rides a Bearer header")
        (is (equal "sess-1" (header "Mcp-Session-Id" (getf (third (second exchanges)) :headers))) "the session id the server returned travels on the next request")
        (is (search "tools/call" (getf (third (third exchanges)) :content)) "the call names the tool")
        (is (search "\"structuredContent\"" text) "and the structured result is the answer's data"))))

;;; --- the composed tools ----------------------------------------------------------

(deftest fxmacrodata-cell-the-brief-asks-catalogue-history-and-calendar
    (with-fxm ("api_key" "test-key")
      (setf *calls* '())
      (let ((text (call "fxmd_usd_macro_brief" "{\"indicator\":\"policy_rate\"}")))
        (let ((urls (reverse (mapcar #'second *calls*))))
          (is (= 3 (length urls)) "three sections, three calls")
          (is (search "/v1/data_catalogue/USD" (first urls)) "the catalogue")
          (is (search "/v1/announcements/USD/policy_rate" (second urls)) "ten history rows for the indicator, named in the route")
          (is (search "limit=10" (second urls)))
          (is (search "timezone=UTC" (third urls)) "and a UTC calendar window"))
        (is (search "\"complete\":true" text) "the brief is complete when all three succeed")
        (is (search "The free tier shows the last 12 months." text) "and relays the service's notice"))))

(deftest fxmacrodata-cell-the-snapshot-follows-its-pinned-pages
    (with-fxm ("api_key" "test-key")
      (setf *calls* '())
      (let ((text (call "fxmd_indicator_history_snapshot"
                         "{\"currency\":\"USD\",\"indicator\":\"policy_rate\",\"start_date\":\"2026-01-01\",\"end_date\":\"2026-03-01\"}")))
        (let ((urls (reverse (mapcar #'second *calls*))))
          (is (= 2 (length urls)) "two pages, as the stub pages them")
          (is (search "offset=0" (first urls)) "the first page asks for the window's start")
          (is (search "dataset_version=v1" (second urls)) "the version the first page answered is pinned on the next")
          (is (search "offset=2" (second urls)) "and the continuation offset is the one the page named"))
        (is (search "\"page_count\":2" text) "the snapshot counts its pages")
        (is (search "\"complete\":true" text) "and reports the window complete"))))

(deftest fxmacrodata-cell-a-broken-page-chain-is-an-error
    (with-fxm ("api_key" "test-key")
      (script "offset=2" (lambda (method url content)
                           (declare (ignore method url content))
                           (values (json-body (nlk:json-object
                                               "data" (vector (nlk:json-object "value" 1))
                                               "dataset_version" "v2"
                                               "pagination" (nlk:json-object "has_more" nil
                                                                              "returned_count" 1
                                                                              "offset" 2)))
                                   200 (response-headers))))
      (let ((text (call "fxmd_indicator_history_snapshot"
                         "{\"currency\":\"USD\",\"indicator\":\"policy_rate\"}")))
        (is (search "\"error\"" text) "a version change is refused, not joined")
        (is (search "Dataset version changed" text) "naming what went wrong"))))


;;; --- the command, the manual, and a missing member -------------------------------

(deftest fxmacrodata-cell-the-command-answers-a-status-and-a-brief
    (with-fxm ("api_key" "test-key")
      (let ((status (cell-entry "nodecode-fxmacrodata" "fxmacrodata" "")))
        (is (search "fxmacrodata:" status) "with no argument it says where it calls")
        (is (search "key: set in the section" status) "and what the key state is"))
      (setf *calls* '())
      (let ((answer (cell-entry "nodecode-fxmacrodata" "fxmacrodata" "policy_rate")))
        (let ((calls (length *calls*)))
          (is (search "USD macro research brief" answer) "with an indicator it builds the brief")
          (is (= 3 calls) "from three calls")))))

(deftest fxmacrodata-cell-the-manual-carries-the-guidance
    (with-fxm ("api_key" "test-key")
      (let ((text (cddr (assoc :fxmacrodata nle::*help-topics*))))
        (is (search "fxmd_data_catalogue" text) "the primer names the discovery tool")
        (is (search "never invent data" text) "and the standing instruction rides with it"))))

(deftest fxmacrodata-cell-a-missing-required-member-refuses-first
    (with-fxm ("api_key" "test-key")
      (setf *calls* '())
      (let ((text (call "fxmd_forex" "{}")))
        (let ((calls (length *calls*)))
          (is (search "preflight" text) "the call is refused before the body runs")
          (is (zerop calls) "and no request is made")))))

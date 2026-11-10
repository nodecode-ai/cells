;;;; cell-test.lisp --- the cell against a stubbed Qrblox MCP server.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved is the cell's own wiring, not Qrblox: the lifecycle (a
;;;; start installs what it declares, a stop takes it back, a refused section
;;;; signals), the section (a merchant key, from the member or a variable),
;;;; the seven tools through the dispatch the model calls them by -- the server
;;;; tool each names, the arguments it sends, the bearer key and the session
;;;; id on the wire -- an answer the server reports as an error, and /qrblox.

(in-package #:nodecode.test)

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "qrblox"
  (:config "api_key" +key+)
  (:help :qrblox)
  (:command "qrblox")
  (:running
   (is (nle::find-tool "qrblox_ping") "the health tool is registered")
   (is (nle::find-tool "qrblox_codes") "and the code list")
   (is (nle::find-tool "qrblox_create") "and the code draft")
   (is (nle::find-tool "qrblox_audio") "and the audio code")
   (is (nle::find-tool "qrblox_menu") "and the menus")
   (is (nle::find-tool "qrblox_scans") "and the scan analytics")
   (is (nle::find-tool "qrblox_call") "and the call that reaches the rest")
   (let ((mine (remove-if-not (lambda (entry) (eql 0 (search "qrblox_" (first entry))))
                               nle::*tools*)))
     (is (= 7 (length mine)) "seven of them, and no others"))
   (is (equal +key+ (nodecode-qrblox::setting :api-key)) "the key the section carries"))
  (:stopped
   (is (null (nle::find-tool "qrblox_ping")) "and stop takes every one back")
   (is (null (nle::find-tool "qrblox_call"))))
  (:refused
   ("api_key" "" "api_key_env" "UNSET_QRBLOX_KEY")
   ("api_key" +key+ "timeout_seconds" 0)
   ("api_key" +key+ "max_chars" 10)
   ("api_key" +key+ "server_url" "http://plain.example.test/mcp")))

(deftest qrblox-cell-without-a-key-refuses ()
  ;; The one setting no merchant call works without: a section naming no key,
  ;; over a variable that holds none, is refused at start, as the original
  ;; offered no merchant tool without QRBLOX_API_KEY.
  (with-wire
    (let ((condition (signals-error nlk:config-refusal
                                    (qrblox-start "api_key" ""
                                                  "api_key_env" "UNSET_QRBLOX_KEY"))))
      (is (cl:search "no key" (princ-to-string condition)) "the refusal says what is missing")
      (is (cl:search "create.qrblox.com" (princ-to-string condition)) "and where to mint one")
      (is (null (nle::find-tool "qrblox_codes")) "and installs nothing"))))

(deftest qrblox-cell-the-key-from-the-variable-is-the-one-sent ()
  (with-wire
    (with-stubbed-fdefinition (nle::credential-env (name)
                                (and (string= name "QRBLOX_TEST_KEY") "sk-from-env"))
      (with-cell-stop ((qrblox-start "api_key" "" "api_key_env" "QRBLOX_TEST_KEY"))
        (qrblox-called "qrblox_ping" "{}")
        (let ((sent (qrblox-request-header (qrblox-tool-call) "authorization")))
          (is (equal "Bearer sk-from-env" sent) "a section with no key member reads the variable it names"))
        (let ((said (nodecode-qrblox::run-command "key" nil)))
          (is (cl:search "the variable QRBLOX_TEST_KEY" said) "and says so in words"))))))

;;; --- the wire ---------------------------------------------------------------------

(deftest qrblox-cell-one-session-serves-every-call ()
  (with-qrblox ()
    (qrblox-called "qrblox_codes" "{}")
    (qrblox-called "qrblox_ping" "{}")
    (let ((inits (count-if (lambda (call) (equal "initialize" (nlk:json-value (getf call :body) :text "method")))
                            *calls*)))
      (is (= 1 inits) "initialize happens once and is reused"))
    (let* ((call (qrblox-tool-call))
           (auth (qrblox-request-header call "authorization"))
           (sid (qrblox-request-header call "mcp-session-id")))
      (is (equal "Bearer sk-qrblox-test-key" auth) "with the section's key as a bearer token")
      (is (equal +session+ sid) "and the session id the server handed is echoed")
      (is (equal "https://api.qrblox.com/mcp/v1" (getf call :url)) "to Qrblox's own MCP endpoint, the one the plugin's mcp.json names"))))

(deftest qrblox-cell-a-refused-key-is-one-sentence-naming-the-member ()
  (with-qrblox ()
    (setf *answer*
          (lambda (call)
            (declare (ignore call))
            (values (nlk:encode-json-object
                     (nlk:json-object
                      "jsonrpc" "2.0" "id" 1
                      "error" (nlk:json-object
                               "code" -32001
                               "message" "Missing or invalid key"
                               "data" (nlk:json-object "code" "INVALID_API_KEY"))))
                    401 nil)))
    (let ((answer (qrblox-called "qrblox_codes" "{}")))
      (is (cl:search "HTTP 401" answer) "the status is named")
      (is (cl:search "INVALID_API_KEY" answer) "and the server's own reason")
      (is (cl:search "api_key" answer) "and the member to fix"))))

(deftest qrblox-cell-an-error-the-server-reports-fails-the-call ()
  (with-qrblox ()
    (setf *answer*
          (lambda (call)
            (declare (ignore call))
            (values (envelope 1 (text-result "no code with that id on this account" :errorp t))
                    200 nil)))
    (let ((answer (qrblox-called "qrblox_codes" "{}")))
      (is (cl:search "no code with that id" answer) "the server's words reach the model"))))

;;; --- the seven tools name the server's own tools -------------------------------------

(deftest qrblox-cell-ping-is-the-health-call ()
  (with-qrblox ()
    (qrblox-called "qrblox_ping" "{}")
    (is (equal "qrblox.ping" (qrblox-sent-tool)) "qrblox.ping, as the skill names it")))

(deftest qrblox-cell-codes-lists-the-account-s-codes ()
  (with-qrblox ()
    (qrblox-called "qrblox_codes" "{}")
    (is (equal "qrblox.qr.list" (qrblox-sent-tool)))))

(deftest qrblox-cell-create-drafts-a-code-with-its-destination ()
  (with-qrblox ()
    (qrblox-called "qrblox_create"
                    "{\"title\":\"Spring menu\",\"destination_url\":\"https://example.test/spring\"}")
    (is (equal "qrblox.qr.create" (qrblox-sent-tool)))
    (let ((sent (qrblox-sent-arguments)))
      (is (equal "Spring menu" (nlk:json-value sent :text "title")))
      (is (equal "https://example.test/spring" (nlk:json-value sent :text "destinationUrl")) "the wire's own camelCase member, not the tool's"))))

(deftest qrblox-cell-create-passes-the-members-the-cell-does-not-name ()
  (with-qrblox ()
    (qrblox-called "qrblox_create"
                    "{\"title\":\"Offer\",\"destination_url\":\"https://example.test/o\",\"arguments\":{\"style\":\"rounded\"}}")
    (is (equal "rounded" (nlk:json-value (qrblox-sent-arguments) :text "style")) "an arguments member rides the request as-is")))

(deftest qrblox-cell-create-with-nothing-never-reaches-the-wire ()
  (with-qrblox ()
    (let ((answer (qrblox-called "qrblox_create" "{}")))
      (is (cl:search "destination_url" answer) "the refusal names what to give"))
    (is (null (qrblox-tool-call)) "and nothing was drafted")))

(deftest qrblox-cell-audio-drafts-an-audio-code ()
  (with-qrblox ()
    (qrblox-called "qrblox_audio" "{\"title\":\"Table 4\",\"arguments\":{\"payload\":\"chime-4\"}}")
    (is (equal "qrblox.audio.create" (qrblox-sent-tool)))
    (is (equal "chime-4" (nlk:json-value (qrblox-sent-arguments) :text "payload")))))

(deftest qrblox-cell-menu-names-one-of-three-calls ()
  (with-qrblox ()
    (qrblox-called "qrblox_menu" "{\"op\":\"list\"}")
    (is (equal "qrblox.menu.list" (qrblox-sent-tool)) "op list reads the menus")
    (qrblox-called "qrblox_menu" "{\"op\":\"create\",\"title\":\"Lunch\"}")
    (is (equal "qrblox.menu.create" (qrblox-sent-tool)) "op create drafts one")
    (is (equal "Lunch" (nlk:json-value (qrblox-sent-arguments) :text "title")))
    (qrblox-called "qrblox_menu"
                    "{\"op\":\"add_item\",\"menu_id\":\"m-1\",\"arguments\":{\"name\":\"Soup\",\"price\":\"4.50\"}}")
    (is (equal "qrblox.menu.add_item" (qrblox-sent-tool)) "op add_item extends one")
    (let ((sent (qrblox-sent-arguments)))
      (is (equal "m-1" (nlk:json-value sent :text "menuId")) "the menu under the wire's name")
      (is (equal "Soup" (nlk:json-value sent :text "name")) "and the item as it was given"))))

(deftest qrblox-cell-menu-add-item-without-a-menu-refuses ()
  (with-qrblox ()
    (let ((answer (qrblox-called "qrblox_menu" "{\"op\":\"add_item\",\"title\":\"Soup\"}")))
      (is (cl:search "menu_id" answer) "the refusal names the member"))
    (is (null (qrblox-tool-call)) "and nothing was added")))

(deftest qrblox-cell-scans-reads-the-scan-summary ()
  (with-qrblox ()
    (qrblox-called "qrblox_scans" "{\"arguments\":{\"range\":\"7d\"}}")
    (is (equal "qrblox.analytics.scan_summary" (qrblox-sent-tool)))
    (is (equal "7d" (nlk:json-value (qrblox-sent-arguments) :text "range")) "a range the tools do not name still rides the request")))

(deftest qrblox-cell-call-reaches-a-tool-of-its-own ()
  (with-qrblox ()
    (qrblox-called "qrblox_call"
                    "{\"tool\":\"qrblox.challenge.create\",\"arguments\":{\"kind\":\"streak\",\"days\":7}}")
    (is (equal "qrblox.challenge.create" (qrblox-sent-tool)))
    (is (= 7 (nlk:json-value (qrblox-sent-arguments) :integer "days")))))

(deftest qrblox-cell-call-with-no-tool-answers-the-server-s-tool-list ()
  (with-qrblox ()
    (let ((answer (qrblox-called "qrblox_call" "{}")))
      (is (cl:search "qrblox.challenge.create" answer) "the server's own names")
      (is (cl:search "qrblox.qr.list" answer) "every one it offers"))
    (is (qrblox-method-call "tools/list") "read from tools/list")
    (is (null (qrblox-tool-call)) "and nothing was called for it")))

;;; --- the operator's command ------------------------------------------------------------

(deftest qrblox-cell-the-command-lists-the-codes ()
  (with-qrblox ()
    (let ((answer (cell-entry "nodecode-qrblox" "qrblox" "" "qrblox-cell-1")))
      (is (cl:search "qrblox.qr.list" answer) "no argument lists the codes"))
    (is (equal "qrblox.qr.list" (qrblox-sent-tool)))))

(deftest qrblox-cell-the-command-pings-and-says-where-the-key-came-from ()
  (with-qrblox ()
    (is (cl:search "qrblox.ping" (cell-entry "nodecode-qrblox" "qrblox" "ping" "qrblox-cell-1")) "/qrblox ping is the health call")
    (let ((answer (cell-entry "nodecode-qrblox" "qrblox" "key" "qrblox-cell-1")))
      (is (cl:search "the key in use comes from" answer) "the operator is told, in words")
      (is (not (cl:search +key+ answer)) "and never the key itself"))))

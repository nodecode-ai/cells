;;;; cell-test.lisp --- lifecycle, the checks, the one call, the gate.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring, not Frihet: the config
;;;; surface (a section naming no key refuses the start), a status that dials
;;;; nothing, a doctor that keeps reachable and authenticated apart, one
;;;; operation through the real dispatch seam against a stubbed endpoint, the
;;;; key never echoed, and the gate that waits for a person.

(in-package #:nodecode.test)

;;; --- lifecycle -------------------------------------------------------------

(define-cell-lifecycle-tests "frihet"
  (:config "api_key" +key+)
  (:hooks :tool)
  (:help :frihet)
  (:command "frihet")
  (:running
   (is (fboundp 'nodecode-frihet:doctor) "the checks are defined")
   (is (nle::find-tool "frihet_mcp") "frihet_mcp is registered")
   (is (equal +key+ (nodecode-frihet::setting :api-key)) "the key the section carries")
   (is (equal "api_key" (nodecode-frihet::setting :key-source)) "and where it came from"))
  (:stopped
   (is (null (nle::find-tool "frihet_mcp")) "stop unregisters the tool"))
  (:refused
   ("api_key" "" "api_key_env" "UNSET_FRIHET_KEY")
   ("api_key" +key+ "timeout_seconds" 0)
   ("api_key" +key+ "mcp_url" "ftp://mirror.test/mcp")))

(deftest frihet-cell-without-a-key-refuses ()
  ;; The one setting no call can work without: a section naming no key, over a
  ;; variable that holds none, is refused at start.
  (with-wire
    (let ((condition (signals-error nlk:config-refusal (frihet-run-start "api_key" ""))))
      (is (cl:search "no key" (princ-to-string condition)) "the refusal says what is missing")
      (is (cl:search "api_key" (princ-to-string condition)) "and names the member")
      (is (null (nle::find-tool "frihet_mcp")) "and installs nothing"))))

;;; --- the checks ------------------------------------------------------------

(deftest frihet-cell-status-dials-nothing (with-frihet ()
  (let ((text (nodecode-frihet:status)))
    (is (cl:search "https://mcp.frihet.io/mcp" text) "the endpoint")
    (is (cl:search "(canonical)" text) "which is the canonical one")
    (is (cl:search "key: set" text) "that a key is set")
    (is (cl:search "timeout: 15s" text) "and the timeout"))
  (is (null *calls*) "a local snapshot never reaches the wire")))

(deftest frihet-cell-doctor-keeps-reachable-apart-from-authenticated (with-frihet ()
  (let ((text (nodecode-frihet:doctor)))
    (is (cl:search "endpoint_reachable: true" text) "the handshake landed")
    (is (cl:search "server frihet 1.4.0" text) "and named itself")
    (is (cl:search "authenticated: true" text) "the key was accepted")
    (is (cl:search "tools/list answered 3 operations" text) "and three operations answered")
    (is (cl:search "tools_available: true" text)))
  (let* ((handshakes (calls-named "initialize"))
         (probe (first handshakes))
         (session (second handshakes)))
    (is (= 2 (length handshakes)) "the reachability probe, then the authenticated session")
    (is (null (header probe "authorization")) "the probe carries no credential")
    (is (equal (format nil "Bearer ~a" +key+) (header session "authorization")) "the session carries the section's key")
    (is (equal +session-id+ (header (call-named "tools/list") "mcp-session-id")) "and the handshake's session rides the next call"))))

(deftest frihet-cell-doctor-reports-a-refused-key-as-false (with-frihet ()
  (setf *answer* (lambda (name request)
                   (declare (ignore request))
                   (if (equal name "tools/list") (values "forbidden" 403) nil)))
  (let ((text (nodecode-frihet:doctor)))
    (is (cl:search "endpoint_reachable: true" text) "the endpoint still answers")
    (is (cl:search "authenticated: false" text) "but the key was refused")
    (is (cl:search "HTTP 403" text) "and the reason says so"))))

(deftest frihet-cell-operations-are-listed (with-frihet ()
  (let ((text (nodecode-frihet:operations)))
    (is (cl:search "3 operations" text) "every advertised operation")
    (is (cl:search "send_invoice" text) "a write among them")
    (is (cl:search "get_invoice" text) "and a read"))))

;;; --- the one call ---------------------------------------------------------

(deftest frihet-cell-the-tool-calls-an-operation (with-frihet ()
  (let ((text (frihet-call-tool "frihet_mcp" "{\"operation\":\"list_invoices\"}")))
    (is (cl:search "INV-1 draft, 120.00 EUR" text) "the operation's own text reaches the model"))
  (let ((call (call-named "tools/call")))
    (is (equal "list_invoices" (nlk:json-value (getf call :request) :string "params" "name")) "the operation the call named")
    (is (equal "POST" (getf call :method)) "one POST per exchange")
    (is (equal +session-id+ (header call "mcp-session-id")) "on the handshake's session")
    (is (equal (format nil "Bearer ~a" +key+) (header call "authorization")) "with the key"))))

(deftest frihet-cell-a-refused-key-is-a-refusal-and-not-a-leak (with-frihet ()
  (setf *answer* (lambda (name request)
                   (declare (ignore request))
                   (if (equal name "tools/call")
                        (values "{\"error\":{\"message\":\"bad key fri_abcdefgh12345678\"}}" 401)
                        nil)))
  (let ((condition (signals-error nodecode-frihet:frihet-error
                                  (nodecode-frihet:call-operation "list_invoices"))))
    (let ((text (princ-to-string condition)))
      (is (cl:search "HTTP 401" text) "the status is named")
      (is (null (cl:search "fri_abcdefgh12345678" text)) "the key in the answer is masked")
      (is (cl:search "[REDACTED" text) "with the mask the original used")))))

(deftest frihet-cell-a-call-with-no-operation-never-reaches-the-wire (with-frihet ()
  (let ((condition (signals-error nodecode-frihet:frihet-error
                                  (nodecode-frihet:call-operation "  "))))
    (is (cl:search "needs an operation" (princ-to-string condition))))
  (is (null *calls*) "and nothing was dialled")))

;;; --- the gate -------------------------------------------------------------

(deftest frihet-cell-a-read-a-draft-and-a-foreign-tool-pass-the-gate-unasked (with-frihet ()
  (let ((*asked* '())
        (nodecode-frihet::*ask-operator* (answer-with nodecode-frihet::+allow-once+)))
    (is (eq :allowed (gate-verdict "list_invoices")) "a read flows through")
    (is (eq :allowed (gate-verdict "createInvoice")) "a draft-write flows through")
    (is (eq :allowed (hook-verdict "eval" (nlk:json-object "form" "(+ 1 1)"))) "the core's own tool is let through")
    (is (eq :allowed (hook-verdict "mcp__frihet__list_invoices" (nlk:json-object))) "and so is a Frihet read another client serves")
    (is (null *asked*) "nobody was asked for any of them")
    (is (eq :allowed (hook-verdict "mcp__frihet__send_invoice" (nlk:json-object))) "a Frihet write through it waits for a person")
    (is (= 1 (length *asked*)) "which is one question"))))

(deftest frihet-cell-an-irreversible-operation-waits-for-a-person (with-frihet ()
  (let ((*asked* '())
        (nodecode-frihet::*ask-operator* (answer-with nodecode-frihet::+allow-once+)))
    (is (eq :allowed (gate-verdict "send_invoice")) "Allow once lets it run")
    (is (= 1 (length *asked*)) "after one question")
    (is (cl:search "send_invoice" (first *asked*)) "that names the operation"))
  (let ((nodecode-frihet::*ask-operator* (answer-with nodecode-frihet::+deny+)))
    (let ((verdict (gate-verdict "markInvoicePaid")))
      (is (stringp verdict) "Deny is the call's failure")
      (is (cl:search "chose Deny" verdict) "and says the operator chose it")
      (is (cl:search "markInvoicePaid" verdict) "naming the operation")))))

(deftest frihet-cell-with-nobody-to-ask-it-refuses-and-names-the-way-through (with-frihet ("ask" nil)
  (let ((verdict (gate-verdict "registerVerifactu")))
    (is (stringp verdict) "nobody was asked, so the call does not run")
    (is (cl:search "nobody was asked" verdict))
    (is (cl:search "confirm" verdict) "and the way through is named"))
  (is (eq :allowed (gate-verdict "registerVerifactu" :confirm t)) "which the operator's own yes is")))

(deftest frihet-cell-an-unclassified-operation-fails-closed (with-frihet ("ask" nil)
  (is (eq :unknown (nodecode-frihet:classify-operation "frobnicate")))
  (is (eq :read (nodecode-frihet:classify-operation "mcp__frihet__list_invoices")) "a namespaced read is still a read")
  (is (eq :irreversible-write (nodecode-frihet:classify-operation "markInvoicePaid")))
  (let ((verdict (gate-verdict "frobnicate")))
    (is (stringp verdict) "an operation the cell cannot classify asks a person too"))))

;;; --- the command -----------------------------------------------------------

(deftest frihet-cell-the-command-answers-each-subcommand (with-frihet ()
  (let ((status (cell-entry "nodecode-frihet" "frihet" "status" "frihet-cell-1")))
    (is (cl:search "endpoint: https://mcp.frihet.io/mcp" status) "/frihet status reads locally"))
  (let ((doctor (cell-entry "nodecode-frihet" "frihet" "doctor" "frihet-cell-1")))
    (is (cl:search "endpoint_reachable: true" doctor) "/frihet doctor probes"))
  (let ((setup (cell-entry "nodecode-frihet" "frihet" "setup" "frihet-cell-1")))
    (is (cl:search "api_key" setup) "/frihet setup says where the key goes"))
  (let ((help (cell-entry "nodecode-frihet" "frihet" "help" "frihet-cell-1")))
    (is (cl:search "Read before write" help) "/frihet help carries the operating contract"))))

(deftest frihet-cell-the-command-never-echoes-what-it-was-handled (with-frihet ()
  (let ((text (cell-entry "nodecode-frihet" "frihet" "status fri_livekey_123456" "frihet-cell-1")))
    (is (cl:search "not 1 more" text) "a second word is refused")
    (is (null (cl:search "fri_livekey_123456" text)) "and never echoed into the transcript"))))

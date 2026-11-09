;;;; cell-test.lisp --- the six tools against a stubbed endpoint, and the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every exchange is a stubbed NODECODE-CROSODO::HTTP: nothing reaches the
;;;; network. Each tool is called the way a model's call reaches it -- through
;;;; the real dispatch, contract and all -- so the schema the provider is
;;;; shown and the body that runs are one shape, and the JSON-RPC body the
;;;; cell really posted is what the assertions read.

(in-package #:nodecode.test)

;;; --- words the tests read ------------------------------------------------------------

(defparameter +county-call+
  "{\"product\":\"permit_guide\",\"state\":\"VA\",\"county\":\"Fairfax\",\"email\":\"baker@example.test\",\"idempotencyKey\":\"k-1\"}"
  "One start_checkout call carrying every member the endpoint declares.")

(defparameter +sale-call+
  "{\"tool\":\"log_sale\",\"arguments\":{\"amountUsd\":42.5,\"soldOn\":\"2026-05-01\"}}"
  "One crosodo_call, forwarding a tool this cell has no schema of its own for.")

;;; --- the lifecycle, from the one declaration ---------------------------------------

(define-cell-lifecycle-tests "crosodo"
  (:help :crosodo)
  (:command "crosodo")
  (:running
   (is (nle::find-tool "lookup_state_law") "lookup_state_law is registered")
   (is (nle::find-tool "lookup_county") "lookup_county is registered")
   (is (nle::find-tool "start_checkout") "start_checkout is registered")
   (is (nle::find-tool "crosodo_call") "crosodo_call is registered")
   (is (equal "https://crosodo.com/api/mcp" (nodecode-crosodo::setting :base-url)) "the endpoint defaults to Crosodo's"))
  (:stopped
   (is (null (nle::find-tool "lookup_state_law")) "stop unregisters the tools"))
  (:refused
   ("base_url" "crosodo.com/api/mcp")
   ("timeout_seconds" 0)
   ("answer_chars" 100))
  (:idle crosodo:crosodo-error (crosodo:lookup-state-law "VA")))

;;; --- a state, a county, the catalog ------------------------------------------------

(deftest crosodo-cell-a-state-lookup-answers-the-state (with-crosodo ()
  (let ((text (crosodo-call "lookup_state_law" "{\"state\":\"VA\"}")))
    (is (search "Virginia" text) "the state's row comes through")
    (is (search "Tiered" text) "with its sales cap")
    (is (search "3.2-5130" text) "and its statute")
    (is (equal "VA" (crosodo-parameter "lookup_state_law" "state")) "the state is sent under the endpoint's own member name")
    (is (equal '("initialize" "notifications/initialized" "tools/call") (posted-methods)) "the handshake goes out once, before the call"))))

(deftest crosodo-cell-a-county-lookup-sends-both-places (with-crosodo ()
  (let ((text (crosodo-call "lookup_county" "{\"state\":\"VA\",\"county\":\"Fairfax\"}")))
    (is (search "Fairfax" text) "the county comes back")
    (is (equal "VA" (crosodo-parameter "lookup_county" "state")) "both places are sent")
    (is (equal "Fairfax" (crosodo-parameter "lookup_county" "county")) "the county under its own member name"))))

(deftest crosodo-cell-the-catalog-needs-no-arguments (with-crosodo ()
  (let ((text (crosodo-call "list_products" "{}")))
    (is (search "permit_guide" text) "the products come through")
    (is (search "$26" text) "with their prices")
    (is (zerop (hash-table-count (posted-arguments "list_products"))) "an empty arguments object is sent, as the endpoint's schema says"))))

(deftest crosodo-cell-the-handshake-goes-out-once (with-crosodo ()
  (crosodo-call "list_products" "{}")
  (crosodo-call "list_products" "{}")
  (is (= 1 (count "initialize" (posted-methods) :test #'equal)) "one handshake for three POSTs")
  (is (= 2 (count "tools/call" (posted-methods) :test #'equal)) "and one call each")))

;;; --- a checkout, a credential ------------------------------------------------------

(deftest crosodo-cell-a-checkout-answers-its-url-and-sends-every-member (with-crosodo ()
  (let ((text (crosodo-call "start_checkout" +county-call+)))
    (is (search "checkout.example.test" text) "the checkout url comes back for the operator to open")
    (is (equal "permit_guide" (crosodo-parameter "start_checkout" "product")) "the product is sent")
    (is (equal "Fairfax" (crosodo-parameter "start_checkout" "county")) "the county is sent")
    (is (equal "k-1" (crosodo-parameter "start_checkout" "idempotencyKey")) "the idempotency key keeps its camelCase wire name"))))

(deftest crosodo-cell-a-county-product-needs-a-place (with-crosodo ()
  (is (search "county-specific" (refusal-text crosodo:crosodo-error (crosodo:start-checkout "permit_guide"))) "the county-specific products say what they need")))

(deftest crosodo-cell-a-credential-answers-whether-it-stands (with-crosodo ()
  (is (search "active" (crosodo-call "verify_certification" "{\"certId\":\"CC-VA-26-1042\"}")) "a real credential answers its status")
  (is (equal "CC-VA-26-1042" (crosodo-parameter "verify_certification" "certId")) "the id is sent under the endpoint's own name")
  (is (search "ERROR: Credential not found" (crosodo-call "verify_certification" "{\"certId\":\"CC-XX-26-0001\"}")) "an unknown credential is a failed call, in the endpoint's words")))

;;; --- the escape hatch, and a forgotten session -------------------------------------

(deftest crosodo-cell-crosodo-call-forwards-any-tool (with-crosodo ()
  (crosodo-call "crosodo_call" +sale-call+)
  (is (= 42.5 (crosodo-parameter "log_sale" "amountUsd")) "the arguments object reaches the endpoint member for member")
  (is (equal "2026-05-01" (crosodo-parameter "log_sale" "soldOn")) "and keeps the names it carried")))

(deftest crosodo-cell-a-forgotten-session-costs-one-more-handshake (with-crosodo ()
  (setf *session-lost* t)
  (let ((text (crosodo-call "list_products" "{}")))
    (is (search "permit_guide" text) "the retry answers")
    (is (= 2 (count "initialize" (posted-methods) :test #'equal)) "a session the endpoint forgot is said again, once"))))

;;; --- refusals ----------------------------------------------------------------------

(deftest crosodo-cell-a-lookup-without-a-place-refuses (with-crosodo ()
  (is (search "needs a state" (refusal-text crosodo:crosodo-error (crosodo:lookup-state-law " "))) "the refusal names what is missing")
  (is (search "needs both" (refusal-text crosodo:crosodo-error (crosodo:lookup-county "VA" ""))) "a county lookup needs both places")))

(deftest crosodo-cell-the-command-answers-with-its-panel (with-crosodo ()
  (multiple-value-bind (text dialog) (nodecode-crosodo::run-slash "" nil)
    (is (search "cottage-food" text) "bare /crosodo says what it is for")
    (is (= 5 (length (getf dialog :rows))) "and offers a row a lookup")))
  (with-crosodo ()
    (is (search "Tiered" (nodecode-crosodo::run-slash "CA" nil)) "a word is read as a state")))

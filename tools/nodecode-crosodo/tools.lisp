;;;; tools.lisp --- the endpoint's five public lookups and the escape hatch.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every tool the original pointed an MCP client at, with the argument names
;;;; the endpoint's own list uses -- lookup_state_law {state}, lookup_county
;;;; {state, county}, list_products {}, start_checkout {product, state,
;;;; county, email, idempotencyKey}, verify_certification {certId} -- and one
;;;; more, crosodo_call, for a tool the endpoint grows after this cell was
;;;; written: the original read its tool list from the server every time, and
;;;; that reach is what crosodo_call keeps.
;;;;
;;;; Each answers the endpoint's text. A result the server marks isError is
;;;; NLE:FAILURE, so the fact records the call failed; a refusal of this
;;;; cell's own is CROSODO-ERROR, as every verb's is.

(in-package #:nodecode-crosodo)

;;; --- words ---------------------------------------------------------------------

(defun nonblank (value)
  "VALUE as a trimmed non-empty string, or NIL."
  (let ((text (and (stringp value) (nlk:trimmed value))))
    (and text (plusp (length text)) text)))

(defun member-key (key)
  "KEY as a JSON member name: a string as it is, a symbol downcased."
  (etypecase key
    (string key)
    (symbol (string-downcase (symbol-name key)))))

(defun arguments-of (arguments)
  "ARGUMENTS -- an object, or a plist of member names and values -- as the
object a tools/call carries."
  (cond ((null arguments) (nlk:json-object))
        ((hash-table-p arguments) arguments)
        ((listp arguments)
         (let ((object (nlk:json-object)))
           (loop for (key value) on arguments by #'cddr
                 when value do (setf (gethash (member-key key) object) value))
           object))
        (t (fail "arguments must be an object or a plist of names and values, not ~s" arguments))))

(defmacro answering (&body body)
  "BODY's text; a refusal becomes that text, never an unhandled error."
  `(handler-case (progn ,@body)
     (crosodo-error (condition)
       (nle:failure (format nil "ERROR: ~a" (crosodo-error-detail condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: Crosodo: ~a" (nlk:one-line (princ-to-string condition)))))))

(defun ask (name arguments)
  "Tool NAME called with ARGUMENTS: the endpoint's answer as the model reads
it, marked a failure when the endpoint says the call failed."
  (let* ((result (mcp-call name arguments))
         (text (result-text result)))
    (if (result-failed-p result)
        (nle:failure (format nil "ERROR: ~a" text))
        text)))

;;; --- the boundary: what a model call carries -------------------------------------
;;; The endpoint's own input schemas, member for member, in the same names:
;;; it refuses a member it does not declare (additionalProperties false), so a
;;; name this cell invents would be a 400.

(nlk:define-contract state-law-request
  (:doc "Cottage-food and home-bakery requirements for one US state, from
Crosodo's hand-maintained dataset: sales cap, registration, allowed products,
statute and agency links. An unknown state answers an error, never invented data.")
  ("state" :text :required t
   :doc "a two-letter USPS code (VA) or a state slug (virginia)"))

(nlk:define-contract county-request
  (:doc "County-level cottage-food requirements for one state and county:
contacts, verification status and local links. An unknown county answers an
error, never invented data.")
  ("state" :text :required t
   :doc "a two-letter USPS code (PA) or a state slug (pennsylvania)")
  ("county" :text :required t
   :doc "a county name or slug (lancaster or Lancaster County)"))

(nlk:define-contract checkout-request
  (:doc "Create a Stripe Checkout session for one Crosodo product and answer its
url: the buyer pays on Stripe's hosted page, and this call never charges. Ask
the user before starting one, and hand them the url.")
  ("product" :text :required t
   :options '("permit_guide" "certified_exam" "unlimited_pass")
   :doc "the purchasable product id, as list_products names it")
  ("state" :text
   :doc "required for permit_guide and unlimited_pass: a two-letter code or a slug")
  ("county" :text
   :doc "required for permit_guide and unlimited_pass: a county name or slug")
  ("email" :text
   :doc "the buyer's email, prefilled at Stripe Checkout")
  ("idempotencyKey" :text :slot idempotency-key
   :doc "a client-generated key: a retry with the same key reuses the Stripe session instead of making a second one"))

(nlk:define-contract certification-request
  (:doc "Public lookup of a Crosodo Certified credential. Answers the public
fields only: holder display name, dates and status. Unknown, expired or revoked
credentials answer an error.")
  ("certId" :text :required t :slot cert-id
   :doc "the credential id, format CC-{ST}-{YY}-{XXXX}; CC-VA-26-1042 only illustrates the format"))

(nlk:define-contract generic-call-request
  (:doc "Call any tool the Crosodo endpoint lists, by its own name, for a lookup
this cell has no verb for or one the endpoint added after this cell was
written.")
  ("tool" :text :required t
   :doc "the endpoint's tool name, e.g. lookup_state_law")
  ("arguments" :object
   :doc "the arguments object that tool takes, in its own member names; absent for a tool that takes none"))

(defparameter +products-schema+
  (list :description "The Crosodo product catalog: the free state-law report and the agent checkout products, each with its id, price and url. It does not create a checkout session; start_checkout does."
        :parameters (nlk:json-object "type" "object" "properties" (nlk:json-object)))
  "The list_products tool's schema: an empty object, as the endpoint's is.")

(defparameter +place-products+ '("permit_guide" "unlimited_pass")
  "The products the endpoint sells per county, which need a state and a county.")

;;; --- the verbs -------------------------------------------------------------------

(define-verb lookup-state-law (state)
  "Cottage-food and home-bakery law for one US state: sales cap, registration,
allowed products, statute and the agency links, as Crosodo's dataset has them.
STATE is a two-letter code (VA) or a slug (virginia). => the endpoint's answer
as text."
  (let ((state (nonblank state)))
    (unless state
      (fail "lookup-state-law needs a state: a two-letter code (VA) or a slug (virginia)"))
    (answering (ask "lookup_state_law" (nlk:json-object "state" state)))))

(define-verb lookup-county (state county)
  "County-level requirements for one US state and county: contacts,
verification status and local links. => the endpoint's answer as text."
  (let ((state (nonblank state))
        (county (nonblank county)))
    (unless (and state county)
      (fail "lookup-county needs both: a state (VA, or virginia) and a county (Fairfax, or Fairfax County)"))
    (answering (ask "lookup_county" (nlk:json-object "state" state "county" county)))))

(define-verb products ()
  "The Crosodo product catalog: the free state-law report and the products a
checkout can sell, with prices and urls. => the endpoint's answer as text."
  (answering (ask "list_products" (nlk:json-object))))

(define-verb start-checkout (product &key state county email idempotency-key)
  "Create a Stripe Checkout session for PRODUCT and answer its url; it never
charges. PRODUCT is permit_guide, certified_exam or unlimited_pass; the first
and last are county-specific and need STATE and COUNTY. EMAIL is prefilled at
Checkout. IDEMPOTENCY-KEY makes a retry reuse the session it made. => the
endpoint's answer as text."
  (let ((product (nonblank product))
        (state (nonblank state))
        (county (nonblank county)))
    (unless product
      (fail "start-checkout needs a product: permit_guide, certified_exam or unlimited_pass, as list_products names them"))
    (unless (member product '("permit_guide" "certified_exam" "unlimited_pass") :test #'string=)
      (fail "~s is no Crosodo product: permit_guide, certified_exam or unlimited_pass" product))
    (when (and (member product +place-products+ :test #'string=) (or (null state) (null county)))
      (fail "~a is county-specific: give both state and county, as in (crosodo:start-checkout \"~a\" :state \"VA\" :county \"Fairfax\")"
            product product))
    (answering (ask "start_checkout"
                    (arguments-of (list "product" product
                                        "state" state
                                        "county" county
                                        "email" (nonblank email)
                                        "idempotencyKey" (nonblank idempotency-key)))))))

(define-verb verify-certification (cert-id)
  "Verify a Crosodo Certified credential and answer its public fields: holder
display name, dates and status. CERT-ID is CC-{ST}-{YY}-{XXXX}. => the
endpoint's answer as text."
  (let ((cert-id (nonblank cert-id)))
    (unless cert-id
      (fail "verify-certification needs a credential id, format CC-{ST}-{YY}-{XXXX}"))
    (answering (ask "verify_certification" (nlk:json-object "certId" cert-id)))))

(define-verb call-tool (name &optional arguments)
  "Call the endpoint's tool NAME with ARGUMENTS -- a plist of member names and
values, or an object -- for a lookup this cell has no verb for. => the
endpoint's answer as text."
  (let ((name (nonblank name)))
    (unless name
      (fail "call-tool needs the endpoint's tool name, e.g. \"lookup_state_law\""))
    (answering (ask name (arguments-of arguments)))))

;;; --- what the model calls ---------------------------------------------------------

(defun state-law-tool (request)
  "The lookup_state_law tool: one state's cottage-food law."
  (answering (lookup-state-law (state-law-request-state request))))

(defun county-tool (request)
  "The lookup_county tool: one county's requirements, under its state's law."
  (answering (lookup-county (county-request-state request)
                             (county-request-county request))))

(defun products-tool (call)
  "The list_products tool: what a checkout can sell, and the free report."
  (declare (ignore call))
  (answering (products)))

(defun checkout-tool (request)
  "The start_checkout tool: a Stripe Checkout url for one product."
  (answering (start-checkout (checkout-request-product request)
                              :state (checkout-request-state request)
                              :county (checkout-request-county request)
                              :email (checkout-request-email request)
                              :idempotency-key (checkout-request-idempotency-key request))))

(defun certification-tool (request)
  "The verify_certification tool: one credential's public fields."
  (answering (verify-certification (certification-request-cert-id request))))

(defun generic-call-tool (request)
  "The crosodo_call tool: any tool the endpoint lists, by its own name."
  (answering (call-tool (generic-call-request-tool request)
                         (generic-call-request-arguments request))))

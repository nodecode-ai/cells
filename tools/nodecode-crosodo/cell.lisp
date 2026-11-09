;;;; cell.lisp --- the manual, the section, /crosodo, and the one declaration.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Config, a sibling top-level key next to `websearch':
;;;;   "crosodo": {"base_url": "https://crosodo.com/api/mcp",
;;;;               "timeout_seconds": 40, "answer_chars": 12000}
;;;; Nothing here needs a key: the endpoint answers its lookups unauthenticated,
;;;; as the original's manifest said (Auth: none needed for lookups). A section
;;;; left out entirely runs on the defaults; one whose base_url is empty or
;;;; has no scheme refuses the start, and then no tool is offered.

(in-package #:nodecode-crosodo)

;;; --- the manual ---------------------------------------------------------------
;;; (help :crosodo) answers it while the cell runs; a request carries the one
;;; line the :HELP clause below gives, never this text.

(defparameter *primer*
  "Crosodo answers US cottage-food and home-bakery law by state and county, through the
nodecode-crosodo cell: six model tools, which a person can also call from eval in the
crosodo package.
  lookup_state_law  {state}
      Sales cap, registration, allowed products, the statute and the agency links, for one
      state: a two-letter code (VA) or a slug (virginia).
  lookup_county  {state, county}
      County requirements: contacts, verification status and local links. A state's rules are
      not a county's permits, so ask the county whenever the user names a place.
  list_products  {}
      The catalog: the free state-law report, and the products a checkout can sell.
  start_checkout  {product, state, county, email, idempotencyKey}
      A Stripe Checkout url, which never charges: the buyer pays on Stripe's page, and the
      url may ask for a Crosodo sign-in. permit_guide and unlimited_pass are county-specific
      and need both state and county. Start one only when the user clearly asks to buy, and
      hand them the url. Pass an idempotencyKey so a retry reuses the same session.
  verify_certification  {certId}
      The public fields of a Crosodo Certified credential, format CC-{ST}-{YY}-{XXXX}:
      holder display name, dates and status.
  crosodo_call  {tool, arguments}
      Any tool the endpoint lists that the five above do not cover, by its own name.
Say which state an answer covers, and the county when one was asked: the law differs by
place. Never invent statute text. When an answer says a place is unknown, say so and point
at the official state site. This is legal information, not legal advice, and cottage-food
law changes: say both plainly when the user asks for counsel, and give the answer's own
verification date when it carries one. An answer the endpoint marks a failure arrives as
ERROR:, in the endpoint's words. ERROR: CROSODO-ERROR is a refusal of this cell's own."
  "What (help :crosodo) answers while the cell runs.")

;;; --- reading the section ---------------------------------------------------------

(defun read-settings (values table)
  "The declared VALUES, with base_url trimmed and refused when it names no
scheme: the one member every call reads. An empty base_url reads as absent,
so the section's default answers for it."
  (declare (ignore table))
  (let* ((raw (or (getf values :base-url) ""))
         (url (string-right-trim "/" (nlk:trimmed raw)))
         (plist (copy-list values)))
    (unless (plusp (length url))
      (nlk:config-error
       "base_url is empty: set it to the Crosodo endpoint, https://crosodo.com/api/mcp"))
    (unless (or (uiop:string-prefix-p "https://" url) (uiop:string-prefix-p "http://" url))
      (nlk:config-error
       "base_url ~s has no http:// or https:// scheme: give the endpoint whole, https://crosodo.com/api/mcp"
       raw))
    (setf (getf plist :base-url) url)
    plist))

;;; --- /crosodo --------------------------------------------------------------------

(defun words-of (text)
  "TEXT as its non-empty whitespace-separated words."
  (remove-if (lambda (word) (zerop (length word)))
             (uiop:split-string (or text "") :separator '(#\Space #\Tab #\Newline #\Return))))

(defun joined (words)
  "WORDS as one phrase, single spaces between."
  (format nil "~{~a~^ ~}" words))

(defun panel ()
  "The dialog bare /crosodo answers: a row a lookup, each running this command
with what the operator types after it."
  (nle:list-dialog
   :title "Crosodo"
   :summary "US cottage-food law by state and county, from Crosodo's dataset"
   :rows (list (nle:ask-row "Look up a state" "State" "/crosodo state"
                             :placeholder "VA or virginia" :verb "look up"
                             :detail "sales cap, registration, allowed products, statute")
               (nle:ask-row "Look up a county" "State and county" "/crosodo county"
                             :placeholder "VA Fairfax" :verb "look up"
                             :detail "county contacts, verification status, local links")
               (nle:run-row "The product catalog" "/crosodo products"
                            :detail "the free state-law report, and what a checkout can sell")
               (nle:ask-row "Verify a credential" "Credential" "/crosodo verify"
                             :placeholder "CC-VA-26-1042" :verb "verify"
                             :detail "public fields of a Crosodo Certified credential")
               (nle:ask-row "Buy a product" "Product, state and county" "/crosodo buy"
                             :placeholder "permit_guide VA Fairfax" :verb "start a checkout"
                             :detail "a Stripe Checkout url; it never charges"))))

(defun run-slash (args session-id)
  "/crosodo on its own: the panel above. With words, one lookup: STATE a
state's law, county STATE COUNTY a county's, products the catalog, verify ID a
credential, buy PRODUCT [STATE COUNTY] a checkout."
  (declare (ignore session-id))
  (let* ((text (nlk:trimmed (or args "")))
         (words (words-of text))
         (head (string-downcase (or (first words) "")))
         (rest (rest words)))
    (cond
      ((zerop (length text))
       (values "crosodo: cottage-food law by state and county. Name a state, e.g. /crosodo CA, or pick a row."
               (panel)))
      ((member head '("products" "product" "catalog") :test #'string=)
       (answering (products)))
      ((member head '("verify" "cert" "certificate" "certification") :test #'string=)
       (answering (verify-certification (joined rest))))
      ((string= head "county")
       (answering (lookup-county (first rest) (joined (rest rest)))))
      ((member head '("buy" "checkout" "buy-product") :test #'string=)
       (answering (start-checkout (first rest)
                                   :state (second rest)
                                   :county (joined (rest (rest rest))))))
      ((string= head "state")
       (answering (lookup-state-law (joined rest))))
      (t (answering (lookup-state-law text))))))

;;; --- the declaration ---------------------------------------------------------------

(nle:define-cell crosodo
  (:section ("crosodo")
    (:guide "nothing is required: the endpoint answers its lookups without a key. base_url points the cell at the endpoint (or at a copy of it), answer_chars bounds what one answer gives the model, and timeout_seconds bounds one call")
    ("base_url" :string :default "https://crosodo.com/api/mcp"
                :doc "the MCP endpoint's streamable-HTTP url; it must carry http:// or https://")
    ("timeout_seconds" :integer :default 40 :min 1
                        :doc "seconds one lookup or checkout may take")
    ("answer_chars" :integer :default 12000 :min 500
                    :doc "the longest answer the model reads from one call; a longer one is clipped, its structured content and all"))
  (:settings #'read-settings)
  (:tool "lookup_state_law" (nlk:contract 'state-law-request) #'state-law-tool)
  (:tool "lookup_county" (nlk:contract 'county-request) #'county-tool)
  (:tool "list_products" +products-schema+ #'products-tool)
  (:tool "start_checkout" (nlk:contract 'checkout-request) #'checkout-tool)
  (:tool "verify_certification" (nlk:contract 'certification-request) #'certification-tool)
  (:tool "crosodo_call" (nlk:contract 'generic-call-request) #'generic-call-tool)
  (:command "crosodo" 'run-slash
            :catches 'crosodo-error
            :description "US cottage-food law by state and county, from Crosodo")
  (:help :crosodo
   "crosodo: US cottage-food and home-bakery law by state and county, from Crosodo - lookup_state_law, lookup_county, list_products, start_checkout, verify_certification"
   *primer*)
  (:start (lambda ()
            ;; A start begins a new connection, and a stop forgets it.
            (reset-wire)
            (nle:on-stop #'reset-wire))))

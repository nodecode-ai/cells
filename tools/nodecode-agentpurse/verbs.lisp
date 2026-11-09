;;;; verbs.lisp --- the plugin's twelve tools, as twelve verbs called through eval.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Eight of them read and never move money. Two of them are governed --
;;;; request-purchase and watch-price -- and reach the server only after a
;;;; person answered this cell's question, or after the operator typed
;;;; /purse approve for the same merchant, amount and quote. The two left are
;;;; the allowance and the receipts.
;;;;
;;;; Every verb answers a string: the server's own JSON text for a read, a
;;;; sentence for a governed purchase. A refusal is AGENTPURSE-ERROR, whose
;;;; detail says what was and was not sent.

(in-package #:nodecode-agentpurse)

(defun chat ()
  "The session this evaluation belongs to, or `operator'."
  (or (and (boundp 'nlk:*scribe-session-id*) nlk:*scribe-session-id*)
      "operator"))

(defun arguments (&rest pairs)
  "A JSON argument object from PAIRS (wire name, value): empties are left out,
a string is trimmed, an integer is an integer, and anything else refuses."
  (let ((object (nlk:make-json-object)))
    (loop for (key value) on pairs by #'cddr
          do (cond ((null value))
                   ((stringp value)
                    (let ((text (nlk:trimmed value)))
                      (unless (zerop (length text))
                        (setf (gethash key object) text))))
                   ((integerp value) (setf (gethash key object) value))
                   (t (fail "~a must be an integer number of cents, not ~s" key value))))
    object))

(defun wire-arguments (&rest pairs)
  "ARGUMENTS over PAIRS, for a call site that spells them out."
  (apply #'arguments pairs))

(defun cents-argument (key value)
  "VALUE as integer cents for the wire, or a refusal that says what it wanted."
  (cond ((null value) nil)
        ((as-cents value) value)
        ((and (stringp value)
              (every #'digit-char-p (nlk:trimmed value))
              (plusp (length (nlk:trimmed value))))
         (parse-integer (nlk:trimmed value)))
        (t (fail "~a is integer cents, never a dollar amount or a float, and ~s is neither" key value))))

(defun tool-text (tool &rest pairs)
  "TOOL's result, the server's own JSON text, over one client of this call."
  (call-tool (open-client) tool :arguments (apply #'arguments pairs) :retry t))

;;; --- the eight that do not move money ------------------------------------------------

(define-verb search-merchants (&key (query ""))
  "Find merchants AgentPurse can pay, each saying whether it is trusted or
blocked. A blocked merchant cannot be paid. Does not move money."
  (tool-text "search_merchants" "query" query))

(define-verb search-products (query &key max-price-cents)
  "Search product offers by QUERY, in the user's own words. Prices are integer
cents. Does not move money; no retailer order is placed."
  (tool-text "search_products" "query" query "maxPriceCents" (cents-argument "max_price_cents" max-price-cents)))

(define-verb get-product (&key product-id url)
  "One product and its offers, by product id or pasted URL. Does not move money."
  (tool-text "get_product" "productId" product-id "url" url))

(define-verb check-price (product-id)
  "Today's price against 90 days of history: low, typical or high. Does not
move money."
  (tool-text "check_price" "productId" product-id))

(define-verb find-alternatives (product-id &key max-price-cents)
  "Similar products under a price. Does not move money."
  (tool-text "find_alternatives" "productId" product-id
        "maxPriceCents" (cents-argument "max_price_cents" max-price-cents)))

(define-verb list-watches ()
  "The price watches and how they ended. Does not move money."
  (tool-text "list_watches"))

(define-verb cancel-watch (watch-id)
  "Stop a price watch. It refunds nothing."
  (with-state (let ((answer (tool-text "cancel_watch" "watchId" watch-id))) answer)))

(define-verb get-budget ()
  "The AgentPurse allowances left, in integer cents. Does not move money."
  (tool-text "get_budget"))

(define-verb list-receipts ()
  "The PayPal sandbox receipts for this purse. A receipt means the sandbox
captured a payment; it does not mean a retailer order was placed."
  (tool-text "list_receipts"))

(define-verb get-quote (&key product-id domain merchant amount-cents description original-task)
  "Quote a purchase before paying. For a catalog product pass product_id and
domain and no price: the server reads it. For a known merchant pass merchant,
amount_cents and description, and original_task in the user's own words.
Answers quoteId, merchant, amountCents and the guard's verdict. If the guard
blocked the quote, stop and say so. Does not move money."
  (let* ((client (open-client))
         (arguments (wire-arguments
                           "productId" product-id
                           "domain" domain
                           "merchant" merchant
                           "amountCents" (cents-argument "amount_cents" amount-cents)
                           "description" description
                           "originalTask" (or original-task ""))))
    (multiple-value-bind (text data) (call-tool client "get_quote" :arguments arguments :retry t)
      (let ((quote (and (hash-table-p data) (quote-from-payload data))))
        (when quote (with-state (remember-quote quote))))
      text)))

;;; --- the money ---------------------------------------------------------------------

(defun note-capture (session data)
  "Add a captured payment to SESSION's ledger, once."
  (let* ((intent (nlk:json-value data :object "intent"))
         (status (nlk:json-value intent :string "status"))
         (amount (as-cents (nlk:json-value intent :integer "amountCents") :positive t))
         (intent-id (nlk:json-value intent :string "id"))
         (order (nlk:json-value data :object "order")))
    (when (and (equal status "captured") amount intent-id)
      (with-state
        (record-purchase session :amount-cents amount
                                  :merchant (nlk:json-value intent :string "merchantName")
                                  :intent-id intent-id
                                  :order-id (nlk:json-value order :string "id"))))))

(defun dashboard-url (owner intent-id)
  (format nil "~a/purses/~a?hold=~a" (normalize-base-url (setting :base-url))
          (quri:url-encode owner) (quri:url-encode intent-id)))

(defun polled-status (data)
  "DATA's payment status, the string the clerk reports."
  (or (nlk:json-value data :string "intent" "status") ""))

(defun waiting-p (status)
  (member status '("pending_approval" "awaiting_paypal") :test #'string=))

(defun poll-hold (client data)
  "GET the payment's status until it settles, the kill switch comes on, or
approval_timeout_s runs out. Never waits past it, never approves anything.
=> (values DATA TIMED-OUT STOPPED-REASON)"
  (let* ((intent-id (nlk:json-value data :string "intent" "id"))
         (deadline (+ (get-internal-real-time)
                      (* (settings-integer :approval-timeout-seconds 120)
                         internal-time-units-per-second))))
    (loop
      (when (killed-p)
        (return (values data nil "the kill switch came on while waiting")))
      (let ((outcome (handler-case (get-spend client intent-id)
                       (agentpurse-error (condition)
                         (return (values data nil (agentpurse-error-detail condition)))))))
        (let ((status (nlk:json-value outcome :string "intent" "status")))
          (unless (waiting-p status)
            (return (values outcome nil nil)))
          (when (>= (get-internal-real-time) deadline)
            (return (values outcome t nil)))
          (sleep (min 2 (max 0 (/ (- deadline (get-internal-real-time))
                                  internal-time-units-per-second)))))))))

(defun purchase-answer (data session stopped timed-out)
  "DATA, a purchase result, as the sentence the model says to the person."
  (let* ((intent (nlk:json-value data :object "intent"))
         (status (or (nlk:json-value intent :string "status") ""))
         (amount (as-cents (nlk:json-value intent :integer "amountCents")))
         (merchant (or (nlk:json-value intent :string "merchantName") "the merchant"))
         (owner (nlk:json-value intent :string "ownerId"))
         (intent-id (nlk:json-value intent :string "id"))
         (order (nlk:json-value data :string "order" "id"))
         (url (nlk:json-value data :string "approvalUrl"))
         (explanation (nlk:json-value data :string "explanation")))
    (cond
      ((equal status "captured")
       (format nil "captured: ~@[~a to ~]~a~@[ (PayPal sandbox order ~a)~]. ~a~@[~%~a~]"
               (and amount (format-cents amount)) merchant order
               (or explanation "The sandbox moved the money; no retailer order was placed.")
               (and (nlk:json-value data :string "fulfillment")
                    (nlk:json-value data :string "fulfillment"))))
      ((waiting-p status)
       (format nil "the cheque is holding for a signature~@[ at ~a~]~@[ (~a)~]. Nothing has been \
captured yet: the owner signs it, then read the payment again.~@[~%~a~]"
               (or url (and owner intent-id (dashboard-url owner intent-id)))
               (cond (timed-out "the wait ended") (stopped) (t "not settled yet"))
               explanation))
      (t (format nil "AgentPurse answered ~a~@[: ~a~]. The sandbox only; no retailer order."
                 (if (plusp (length status)) status "without a status") explanation)))))

(defun explain-pending (session)
  "What a payment that nobody answered leaves behind, in words."
  (format nil "no one answered the approval question, so nothing was sent yet. This session now \
holds a proposed ~a. The operator can answer it with /purse approve (or /purse deny), and then the \
same call sends the payment; it waits ~d seconds."
          (or (first (pending-lines session)) "payment") +grant-ttl-seconds+))

(defun pending-lines (session)
  "The grants waiting for SESSION, one line each, newest first."
  (loop for (key . row) in (pending-grants session)
        collect (format nil "~a ~a to ~a (~d cents), ~a"
                        (if (equal "yes" (nlk:json-value row :string "human"))
                            "approved:" "waiting:")
                        (if (equal "watch_price" (nlk:json-value row :string "tool"))
                            "a price watch for" "a payment of")
                        (or (nlk:json-value row :string "merchant") "a merchant")
                        (or (nlk:json-value row :integer "amount_cents") 0)
                        (if (equal "yes" (nlk:json-value row :string "human"))
                            "call it again to send it"
                            "waiting for /purse approve"))))

(defun ask-and-grant (preview session tool)
  "Ask the person, then record what they said: a proposed grant either way, one
marked human when they approved. A pending one waits for /purse approve."
  (let ((answer (ask-approval preview tool session)))
    (when (eq answer :denied)
      (with-agentpurse-lock (clear-grants session))
      (fail "denied at the approval question: nothing was sent"))
    (with-agentpurse-lock
      (issue-grant preview session tool)
      (when (eq answer :approved) (mark-human session)))
    (when (eq answer :pending)
      (fail "~a" (with-agentpurse-lock (explain-pending session))))
    answer))

(defun grant-for (preview session tool)
  "Consume the one-time grant this payment needs, or refuse."
  (or (with-agentpurse-lock (take-grant session (getf preview :fingerprint) tool))
      (fail "no person's approval matches this payment, so it was not sent")))

(define-verb request-purchase (&key quote-id text original-task page-excerpt)
  "Ask the clerk to pay a quote. Pass quote_id from get_quote. A person is
asked before this sends anything; the clerk can capture, hold the cheque for a
passkey, or deny. A quote pays once. PayPal sandbox only: never say a retailer
shipped the item. If the guard blocked the quote, do not call this."
  (let* ((session (chat))
         (client (open-client))
         (preview (with-agentpurse-lock
                    (shopping-preview client *state* nil :quote-id quote-id :text text))))
    (let ((reason (gate-reason preview session)))
      (when reason
        (with-agentpurse-lock (clear-grants session))
        (fail "~a" reason)))
    (unless (with-agentpurse-lock
             (approved-grant-p session (getf preview :fingerprint) "request_purchase"))
      (ask-and-grant preview session "request_purchase"))
    (grant-for preview session "request_purchase")
    (let ((data (multiple-value-bind (answer data)
                    (call-tool client "request_purchase"
                                :arguments (wire-arguments
                                            "quoteId" (getf preview :quote-id)
                                            "text" text
                                            "originalTask" original-task
                                            "pageExcerpt" page-excerpt))
                  (declare (ignore answer))
                  data)))
      (when (hash-table-p data) (note-capture session data))
      (multiple-value-bind (settled timed-out stopped)
          (if (hash-table-p data) (poll-hold client data) (values data nil nil))
        (when (hash-table-p settled) (note-capture session settled))
        (purchase-answer settled session stopped timed-out)))))

(define-verb watch-price (product-id domain target-cents original-task)
  "Wait for a product to reach target_cents, then AgentPurse files a normal
purchase, which its clerk decides again. A person approves creating the watch;
a later hit shows no second question here. Captures nothing now."
  (let* ((session (chat))
         (client (open-client))
         (preview (with-agentpurse-lock
                    (watch-preview client nil :product-id product-id :domain domain
                                              :target-cents target-cents
                                              :original-task original-task))))
    (let ((reason (gate-reason preview session)))
      (when reason
        (with-agentpurse-lock (clear-grants session))
        (fail "~a" reason)))
    (unless (with-agentpurse-lock
             (approved-grant-p session (getf preview :fingerprint) "watch_price"))
      (ask-and-grant preview session "watch_price"))
    (grant-for preview session "watch_price")
    (let ((data (multiple-value-bind (answer data)
                    (call-tool client "watch_price"
                                :arguments (wire-arguments
                                            "productId" product-id
                                            "domain" domain
                                            "targetCents" (cents-argument "target_cents" target-cents)
                                            "originalTask" original-task))
                  (declare (ignore answer))
                  data)))
      (if (hash-table-p data)
          (format nil "watching ~a to ~a: ~a. It files a purchase when the target is hit, and ~
/purse kill cancels open watches.~%~a"
                  product-id (format-cents (getf preview :amount-cents))
                  (or (nlk:json-value data :string "id") "recorded")
                  (nlk:encode-json-object data))
          (format nil "~a" data)))))

;;; --- the person's side, also what /purse runs ----------------------------------------

(define-verb pending ()
  "What this session is waiting to have approved, one line each. Does not move money."
  (let ((lines (with-agentpurse-lock (pending-lines (chat)))))
    (if lines
        (format nil "~{~a~^~%~}" lines)
        "nothing waits for approval in this session")))

(define-verb approve ()
  "The person's yes: approve what this session is waiting on, so the same call
sends it. A model must not run this for its own payment."
  (let ((count (with-agentpurse-lock (mark-human (chat)))))
    (if (plusp count)
        (format nil "approved ~d waiting purchase~:p: call it again with the same quote to send ~
it. Nothing is sent automatically." count)
        "nothing waits for approval in this session")))

(define-verb deny ()
  "The person's no: drop what this session is waiting on. Nothing is sent."
  (let ((count (with-agentpurse-lock (clear-grants (chat)))))
    (if (plusp count)
        (format nil "denied ~d waiting purchase~:p: nothing will be sent" count)
        "nothing waited for approval in this session")))

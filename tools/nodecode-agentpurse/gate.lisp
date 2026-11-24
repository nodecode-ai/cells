;;;; gate.lisp --- the purchase gate: what a person is shown, and what blocks.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Pure but for the two reads it makes (a merchant's standing, the
;;;; allowance left): no approval of its own, no money. GATE-REASON answers
;;;; NIL when a person may be asked, or the sentence that says why nothing
;;;; will be sent. The plugin's gate.py decided exactly these, in this order.

(in-package #:nodecode-agentpurse)

;;; --- money ---------------------------------------------------------------------

(defun as-cents (value &key positive)
  "VALUE as integer cents, or NIL: a JSON number, never a float, never a boolean."
  (when (and (integerp value) (not (typep value 'bit)))
    (when (and (or (null positive) (plusp value))
               (<= (abs value) 100000000))
      value)))

(defun format-cents (cents)
  "CENTS as dollars: 255 => \"$2.55\". Integer arithmetic, never a float."
  (let* ((sign (if (minusp cents) "-" ""))
         (amount (abs cents)))
    (format nil "~a$~d.~2,'0d" sign (floor amount 100) (mod amount 100))))

;;; --- what the model wrote, read, never trusted -------------------------------------

(defun text-arg (value) (if (stringp value) (nlk:trimmed value) ""))

(defun guard-parts (value)
  "VALUE (the server's guard verdict) => (values DECISION DETAIL): pass,
block, or unknown, and the sentences it gave."
  (cond ((stringp value)
         (values (let ((decision (string-downcase (nlk:trimmed value))))
                   (if (member decision '("pass" "block") :test #'string=) decision "unknown"))
                 ""))
        ((hash-table-p value)
         (let ((decision (string-downcase (or (nlk:json-value value :string "decision") "")))
               (sentences '()))
           (loop for reason across (or (nlk:json-value value :array "reasons") #())
                 do (let ((sentence (if (hash-table-p reason)
                                        (nlk:json-value reason :string "sentence")
                                        (and (stringp reason) reason))))
                      (when sentence (push sentence sentences))))
           (values (if (member decision '("pass" "block") :test #'string=) decision "unknown")
                   (subseq (format nil "~{~a~^ ~}" (nreverse sentences)) 0
                           (min 400 (length (format nil "~{~a~^ ~}" (nreverse sentences))))))))
        (t (values "unknown" ""))))

(defun quote-from-payload (object)
  "OBJECT, a get_quote result, as a quote plist, or NIL when it lacks what a
payment needs."
  (let* ((quote-id (nlk:json-value object :string "quoteId"))
         (amount (as-cents (nlk:json-value object :integer "amountCents") :positive t))
         (merchant (text-arg (nlk:json-value object :string "merchant"))))
    (when (and quote-id (plusp (length quote-id)) amount (plusp (length merchant)))
      (multiple-value-bind (decision detail) (guard-parts (nlk:json-value object :any "guard"))
        (list :quote-id quote-id :merchant merchant :amount-cents amount
              :guard decision :guard-detail detail
              :description (subseq (or (nlk:json-value object :string "description")
                                       (nlk:json-value object :string "product") "")
                                   0 (min 180 (length (or (nlk:json-value object :string "description")
                                                           (nlk:json-value object :string "product") "")))))))))

(defun parse-payment-sentence (text)
  "A payment sentence (\"pay $2.55 from Green Grocer for oat milk\") as
(values MERCHANT CENTS), or NIL. Only when no quote names them."
  (let* ((text (text-arg text))
         (amount-at (search "$" text))
         (from-at (search "from " text :test #'char-equal))
         (for-at (and from-at (search " for " text :start2 from-at :test #'char-equal))))
    (when (and amount-at from-at for-at)
      (let* ((digits (parse-integer text :start (1+ amount-at) :junk-allowed t))
             (dot (position #\. text :start (1+ amount-at))))
        (when (and digits (<= 0 digits 10000000))
          (let ((cents (if (and dot (< dot for-at) (digit-char-p (char text (1+ dot))))
                           (+ (* digits 100)
                              (let* ((fraction (subseq text (1+ dot) (min (+ dot 3) (length text))))
                                     (whole (subseq fraction 0 (or (position-if-not #'digit-char-p fraction)
                                                                    (length fraction)))))
                                (parse-integer (concatenate 'string whole "0") :junk-allowed t)))
                           (* digits 100)))
                (merchant (nlk:trimmed (subseq text (+ from-at 5) for-at))))
            (when (and (plusp cents) (plusp (length merchant)) (<= (length merchant) 80))
              (values merchant cents))))))))

;;; --- what the server says about this payment -----------------------------------------

(defun exact-merchant (rows name)
  "ROWS' row whose name is NAME, folded, or NIL."
  (let ((folded (string-downcase (nlk:trimmed name))))
    (loop for row across (if (vectorp rows) rows #())
          when (and (hash-table-p row)
                    (equal folded (string-downcase (or (nlk:json-value row :string "name") ""))))
            return row)))

(defun merchant-blocked (client name)
  "Whether NAME is blocked, asked of the server, else of the cache. Refuses
when neither can say: an unknown merchant is not a licence to pay."
  (multiple-value-bind (text data)
      (handler-case
          (call-tool client "search_merchants"
                      :arguments (nlk:make-json-object "query" name) :retry t)
        (agentpurse-error () (values nil nil)))
    (declare (ignore text))
    (if (vectorp data)
        (progn (remember-merchants data)
               (let ((row (exact-merchant data name)))
                 (if row
                     (nlk:json-value row :any "blocked")
                     (merchant-blocked-p (cached-merchant name)))))
        (let ((cached (cached-merchant name)))
          (when (null cached)
            (fail "could not re-check ~a against the merchant list, so the payment was not sent" name))
          (merchant-blocked-p cached)))))

(defun remaining-cents (client)
  "The smallest remaining allowance the server reports, in integer cents."
  (let ((rows (multiple-value-bind (text data)
                  (call-tool client "get_budget" :arguments (nlk:make-json-object) :retry t)
                (declare (ignore text))
                data)))
    (unless (vectorp rows)
      (fail "AgentPurse did not return a budget"))
    (let ((cents (loop for row across rows
                       for value = (and (hash-table-p row)
                                        (nlk:json-value row :integer "remainingCents"))
                       when (as-cents value) collect value)))
      (when (null cents)
        (fail "AgentPurse's budget named no remaining cents"))
      (reduce #'min cents))))

(defun quoted-merchant (quote-id)
  "The stored quote QUOTE-ID, or a refusal that says what to do."
  (let ((quote (cached-quote quote-id)))
    (unless quote
      (fail "unknown quote ~a: call get_quote first, so the approval can name the merchant, the \
amount and the guard's verdict" quote-id))
    (when (or (null (getf quote :amount-cents)) (null (getf quote :merchant)))
      (fail "the stored quote is missing a merchant or an amount in cents"))
    quote))

(defun shopping-preview (client state args &key quote-id text)
  "The preview of the payment ARGS name: which merchant, how many cents, the
guard's verdict, the allowance left, and whether the merchant is blocked."
  (multiple-value-bind (quote-id text) (values (text-arg quote-id) (text-arg text))
    (multiple-value-bind (merchant amount guard detail description quote)
        (if (plusp (length quote-id))
            (let ((quote (quoted-merchant quote-id)))
              (values (getf quote :merchant) (getf quote :amount-cents)
                      (getf quote :guard) (getf quote :guard-detail)
                      (getf quote :description) quote-id))
            (multiple-value-bind (merchant cents) (parse-payment-sentence text)
              (unless merchant
                (fail "call get_quote first: a payment sentence needs a merchant and a dollar \
amount (\"pay $2.55 from Green Grocer for oat milk\") before anyone can approve it"))
              (values merchant cents "pending" "" (subseq text 0 (min 180 (length text))) "")))
      (declare (ignore quote))
      (let ((blocked (merchant-blocked client merchant)))
        (list :merchant merchant :amount-cents amount :guard guard :guard-detail detail
              :remaining-cents (remaining-cents client) :quote-id quote-id
              :description (or description "") :merchant-blocked blocked
              :fingerprint (nlk:encode-json-object
                            (nlk:make-json-object "tool" "request_purchase"
                                                   "quoteId" quote-id
                                                   "text" text
                                                   "merchant" merchant
                                                   "amount_cents" amount)))))))

(defun watch-preview (client args &key product-id domain target-cents original-task)
  "The preview of a price watch: the offer's domain, the target, the task."
  (let ((product-id (text-arg product-id)) (domain (text-arg domain))
        (task (text-arg original-task))
        (target (as-cents target-cents :positive t)))
    (unless (and (plusp (length product-id)) (plusp (length domain)) target (plusp (length task)))
      (fail "a price watch needs product_id, domain, target_cents and original_task"))
    (list :merchant domain :amount-cents target :guard "pending" :guard-detail ""
          :remaining-cents (remaining-cents client) :quote-id ""
          :description (subseq task 0 (min 180 (length task)))
          :merchant-blocked (merchant-blocked client domain)
          :fingerprint (nlk:encode-json-object
                        (nlk:make-json-object "tool" "watch_price"
                                               "productId" product-id
                                               "domain" domain
                                               "targetCents" target
                                               "originalTask" task
                                               "merchant" domain
                                               "amount_cents" target)))))

;;; --- the blocks --------------------------------------------------------------------

(defun gate-reason (preview session)
  "Why this payment must not be asked about, or NIL when a person may be."
  (let ((limit (settings-integer :spend-limit-cents 5000))
        (spent (spent-cents session))
        (amount (getf preview :amount-cents)))
    (cond ((killed-p)
           "the AgentPurse kill switch is on: /purse resume turns it off, and this payment was \
not sent")
          ((null preview) "AgentPurse could not prepare this payment, so it was not sent")
          ((getf preview :merchant-blocked)
           (format nil "~a is blocked, so the payment was not sent" (getf preview :merchant)))
          ((equal "block" (getf preview :guard))
           (format nil "the intent guard blocked this purchase~@[. ~a~]. It was not sent"
                   (let ((detail (getf preview :guard-detail)))
                     (and detail (plusp (length detail)) detail))))
          ((not (member (getf preview :guard) '("pass" "pending") :test #'string=))
           "the quote carries no guard verdict, so the payment was not sent")
          ((null spent) "the local spend ledger is unreadable, so the payment was not sent")
          ((> amount (getf preview :remaining-cents))
           (format nil "~a is over the ~a left in this AgentPurse allowance, so the payment was \
not sent" (format-cents amount) (format-cents (getf preview :remaining-cents))))
          ((> (+ spent amount) limit)
           (format nil "this session has spent ~a of its ~a limit: ~a more would pass it, so the \
payment was not sent" (format-cents spent) (format-cents limit) (format-cents amount)))
          (t nil))))

;;; --- the person's question ---------------------------------------------------------

(defun approval-detail (preview tool session)
  "The question's body: what the person is approving, in the plugin's own order."
  (let ((spent (or (spent-cents session) 0))
        (limit (settings-integer :spend-limit-cents 5000))
        (amount (getf preview :amount-cents)))
    (format nil "Merchant: ~a~%Amount: ~a (~d cents)~%Remaining allowance: ~a (~d cents)~%\
Guard: ~a~@[ (~a)~]~%This session after this payment: ~a of ~a (~d cents spent before)~@[~%Quote: ~a~]\
~@[~%Description: ~a~]~%~a~%Sandbox money only; no retailer order is placed."
            (getf preview :merchant)
            (format-cents amount) amount
            (format-cents (getf preview :remaining-cents)) (getf preview :remaining-cents)
            (getf preview :guard)
            (let ((detail (getf preview :guard-detail)))
              (and detail (plusp (length detail)) detail))
            (format-cents (+ spent amount)) (format-cents limit) spent
            (let ((quote (getf preview :quote-id))) (and quote (plusp (length quote)) quote))
            (let ((description (getf preview :description)))
              (and description (plusp (length description)) description))
            (if (equal tool "watch_price")
                "Approving creates the watch. If the target is hit, AgentPurse files a normal \
purchase and its clerk decides again, with no second question here; /purse kill cancels open watches."
                "Approving lets the clerk allow, hold for a passkey, or deny; it does not place a \
retailer order."))))

(defun ask-approval (preview tool session)
  "Put the purchase to the person watching SESSION and wait ASK-OPERATOR's
bounded time. => :approved, :denied or :pending (nobody answered)."
  (let ((title (format nil "AgentPurse: pay ~a to ~a?"
                        (format-cents (getf preview :amount-cents))
                        (getf preview :merchant))))
    (multiple-value-bind (choice how)
        (nle:ask-operator title
                           :choices '("Approve once" "Deny")
                           :detail (approval-detail preview tool session)
                           :seconds (settings-integer :approval-timeout-seconds 120)
                           :session-id session)
      (cond ((and (eq how :answered) (equal choice "Approve once")) :approved)
            ((eq how :answered) :denied)
            (t :pending)))))

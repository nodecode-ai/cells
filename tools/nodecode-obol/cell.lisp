;;;; cell.lisp --- invoices and Stripe payment links, from the conversation.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What the original plugin gave a Hermes user is here: seven invoice tools
;;;; talking to the user's own Stripe account, and a local ledger of what was
;;;; billed. Every write is a Stripe REST call made from this machine with the
;;;; user's key; nothing else is contacted. The ledger is a JSON file under the
;;;; Nodecode home, so a fresh chat still shows what is outstanding.

(in-package #:nodecode-obol)

(defparameter +api-base+ "https://api.stripe.com"
  "Stripe's API. The one host this cell talks to.")

(defparameter +api-version+ "2024-06-20"
  "The Stripe API version the original pinned.")

(defparameter +user-agent+ "nodecode-obol/0.1"
  "This cell names itself; it impersonates no other client.")

(defparameter +duplicate-days+ 30
  "How far back a same-client same-amount invoice is called a duplicate.")

(defparameter +amount-multiplier+ 3
  "How many times a client's average invoice counts as unusual.")

(defparameter +amount-min-history+ 3
  "How many past invoices a client needs before the average means anything.")

;;; --- the network seam ---------------------------------------------------------------

(defun http-call (method url &key headers content timeout)
  "URL asked METHOD by the image's own HTTP exchange. => (values BODY STATUS
HEADERS TRUNCATED-P). Every test stubs this name; nothing else does."
  (nlk:http method url :headers headers :content content :timeout timeout))

(defvar *http* #'http-call
  "The cell's one network call, a DEFVAR so a reload keeps it and a test stubs
it: (METHOD URL &key HEADERS CONTENT TIMEOUT) => (values BODY STATUS HEADERS TRUNCATED-P).")

;;; --- small things -----------------------------------------------------------------

(defun nonblank (text)
  "TEXT without its surrounding whitespace, or NIL when nothing is left."
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (and text (plusp (length text)) text)))

(defun url-escape (text)
  "TEXT as one form value: everything but the unreserved characters escaped."
  (with-output-to-string (out)
    (loop for char across text
          do (if (or (alphanumericp char) (find char "-._~" :test #'char=))
                 (write-char char out)
                 (format out "%~2,'0X" (char-code char))))))

(defun form-encode (pairs)
  "PAIRS, an alist of (name . value), as an application/x-www-form-urlencoded body."
  (format nil "~{~a~^&~}"
          (loop for (name . value) in pairs
                when (and (stringp value) (plusp (length value)))
                  collect (format nil "~a=~a" name (url-escape value)))))

(defun redacted (text)
  "TEXT with anything shaped like a key blanked out, so a log line never holds one."
  (cl-ppcre:regex-replace-all "(sk|rk|pk)_(test|live)_[A-Za-z0-9]+" (or text "") "[redacted key]"))

;;; --- Stripe ------------------------------------------------------------------------

(defun api-key ()
  "The section's key, or a refusal naming where it goes."
  (or (nonblank (setting :api-key))
      (fail "no Stripe key set: put one in the obol section (\"api_key\"), or set STRIPE_API_KEY and name it there")))

(defun key-mode ()
  "TEST, LIVE, MISSING or INVALID, from the shape of the key."
  (let ((key (nonblank (setting :api-key))))
    (cond ((null key) :missing)
          ((or (uiop:string-prefix-p "sk_test_" key) (uiop:string-prefix-p "rk_test_" key)) :test)
          ((or (uiop:string-prefix-p "sk_live_" key) (uiop:string-prefix-p "rk_live_" key)) :live)
          (t :invalid))))

(defun stripe (method path &key params data)
  "PATH on Stripe, asked METHOD with the user's key. => the decoded object.
A refused status signals OBOL-ERROR carrying Stripe's own message."
  (let* ((key (api-key))
         (mode (key-mode)))
    (when (and (eq mode :live) (not (setting :allow-live)))
      (fail "this is a LIVE key and live mode is off, so nothing was sent; turn on allow_live to move real money"))
    (let* ((query (and params (form-encode params)))
           (body (and data (form-encode data)))
           (url (format nil "~a~a~@[?~a~]" +api-base+ path query))
           (headers (list (cons "authorization" (format nil "Bearer ~a" key))
                          (cons "stripe-version" +api-version+)
                          (cons "user-agent" +user-agent+)
                          (cons "content-type" "application/x-www-form-urlencoded"))))
      (multiple-value-bind (text status)
          (funcall *http* method url :headers headers :content body
                             :timeout (setting :timeout-seconds))
        (let ((object (and (stringp text) (plusp (length text))
                           (ignore-errors (nlk:decode-json text)))))
          (if (and (integerp status) (<= 200 status 299) (hash-table-p object))
              object
              (fail "Stripe answered ~a: ~a" status
                    (redacted (or (and (hash-table-p object) (nlk:json-value object :string "error" "message"))
                                  "no message")))))))))

(defun stripe-money (amount-minor currency)
  "AMOUNT-MINOR as a readable amount in CURRENCY."
  (format nil "~,2f ~a" (/ amount-minor 100.0) (string-upcase currency)))

;;; --- the ledger --------------------------------------------------------------------

(defun ledger-path ()
  "This home's Obol ledger."
  (nlk:home "obol/ledger.json"))

(defun json-text (value)
  "VALUE, a string or an integer, as a JSON literal."
  (etypecase value
    (integer (princ-to-string value))
    (string (with-output-to-string (out)
              (write-char #\" out)
              (loop for char across value
                    do (case char
                         (#\" (write-string "\\\"" out))
                         (#\\ (write-string "\\\\" out))
                         (t (if (< (char-code char) 32)
                                (format out "\\u~4,'0X" (char-code char))
                                (write-char char out)))))
              (write-char #\" out)))))

(defun record-json (record)
  "One ledger RECORD, a plist, as a JSON object."
  (format nil "{~{~a~^,~}}"
          (loop for (key . value) in (list (cons "id" (getf record :id))
                                            (cons "number" (getf record :number))
                                            (cons "email" (getf record :email))
                                            (cons "name" (getf record :name))
                                            (cons "currency" (getf record :currency))
                                            (cons "amount" (getf record :amount))
                                            (cons "status" (getf record :status))
                                            (cons "link" (getf record :link))
                                            (cons "created" (getf record :created))
                                            (cons "note" (or (getf record :note) "")))
                collect (format nil "~a:~a" (json-text key) (json-text value)))))

(defun ledger-save (records)
  "RECORDS written whole, atomically, under this home."
  (let ((path (ledger-path)))
    (ensure-directories-exist path)
    (let ((tmp (concatenate 'string (namestring path) ".tmp")))
      (with-open-file (out tmp :direction :output :if-exists :supersede :if-does-not-exist :create)
        (format out "{\"invoices\":[~{~a~^,~}]}" (mapcar #'record-json records)))
      (rename-file tmp path))
    records))

(defun ledger-load ()
  "This home's ledger as a list of plists, newest first."
  (let ((path (ledger-path)))
    (sort (if (probe-file path)
              (let ((store (ignore-errors (nlk:decode-json (uiop:read-file-string path)))))
                (loop for item across (or (nlk:json-value store :array "invoices") #())
                      collect (list :id (nlk:json-value item :string "id")
                            :number (nlk:json-value item :string "number")
                            :email (nlk:json-value item :string "email")
                            :name (nlk:json-value item :string "name")
                            :currency (nlk:json-value item :string "currency")
                            :amount (nlk:json-value item :integer "amount")
                            :status (nlk:json-value item :string "status")
                            :link (nlk:json-value item :string "link")
                            :created (nlk:json-value item :integer "created")
                            :note (nlk:json-value item :string "note"))))
              '())
          #'> :key (lambda (record) (or (getf record :created) 0)))))

(defun ledger-put (record)
  "RECORD saved over any record with its id."
  (let ((records (remove (getf record :id) (ledger-load)
                         :test #'equal :key (lambda (item) (getf item :id)))))
    (ledger-save (cons record records))))

(defun ledger-find (id)
  (find id (ledger-load) :test #'equal :key (lambda (record) (getf record :id))))

;;; --- risk checks -------------------------------------------------------------------

(defun amount-minor (amount currency)
  "AMOUNT, in currency units, as Stripe's minor units for CURRENCY."
  (declare (ignore currency))
  (let ((value (and amount (round (* (coerce amount 'double-float) 100)))))
    (unless (and (integerp value) (plusp value))
      (fail "amount must be a positive number, got ~s" amount))
    value))

(defun risk-flags (email name minor)
  "The deterministic warnings one proposed invoice earns, as sentences."
  (let* ((records (ledger-load))
         (same (remove-if-not (lambda (record)
                                 (and (getf record :email) email
                                      (string-equal (getf record :email) email)
                                      (not (equal (getf record :status) "void"))))
                               records))
         (window (* +duplicate-days+ 24 3600))
         (now (get-universal-time))
         (fine 2208988800)
         (duplicate (find-if (lambda (record)
                               (and (= (or (getf record :amount) 0) minor)
                                    (< (- now (or (getf record :created) 0)) window)))
                             same))
         (average (and (>= (length same) +amount-min-history+)
                       (/ (reduce #'+ same :key (lambda (record) (or (getf record :amount) 0)))
                          (length same))))
         flags)
    (when duplicate
      (push (format nil "possible duplicate: ~a for ~a was already billed ~d days ago (~a)"
                    (stripe-money minor "usd") (or email "this client")
                    (max 0 (round (- now (or (getf duplicate :created) now)) 86400))
                    (or (getf duplicate :id) "no id"))
            flags))
    (when (and average (plusp average) (> minor (* +amount-multiplier+ average)))
      (push (format nil "unusual amount: ~a is more than ~d times this client's average invoice (~a)"
                    (stripe-money minor "usd") +amount-multiplier+ (stripe-money (round average) "usd"))
            flags))
    (when (and name (find-if (lambda (record)
                               (and (getf record :name) (string-equal (getf record :name) name)
                                    (getf record :email) email
                                    (not (string-equal (getf record :email) email))))
                             records))
      (push (format nil "known client, new address: ~a was billed at a different email before" name) flags))
    (nreverse flags)))

;;; --- the operations ----------------------------------------------------------------

(defun customer-id (email name)
  "The Stripe customer for EMAIL, made when this account has none."
  (let* ((found (stripe :get "/v1/customers" :params (list (cons "email" email)
                                                            (cons "limit" "1"))))
         (data (nlk:json-value found :array "data")))
    (or (and (> (length data) 0) (nlk:json-value (aref data 0) :string "id"))
        (nlk:json-value (stripe :post "/v1/customers"
                                 :data (list (cons "email" email)
                                             (cons "name" name)))
                         :string "id"))))

(defun line-item-data (invoice-id customer currency item)
  "One invoice item's form body."
  (list (cons "invoice" invoice-id)
        (cons "customer" customer)
        (cons "currency" currency)
        (cons "description" (obol-line-item-description item))
        (cons "unit_amount" (princ-to-string (amount-minor (obol-line-item-amount item) currency)))
        (cons "quantity" (princ-to-string (obol-line-item-quantity item)))))

(defun finish-invoice (invoice-id send)
  "INVOICE-ID finalized, and sent when SEND."
  (let ((final (stripe :post (format nil "/v1/invoices/~a/finalize" invoice-id))))
    (if send
        (stripe :post (format nil "/v1/invoices/~a/send" invoice-id))
        final)))

(defun invoice-record (invoice &key email name amount link)
  "The ledger record for a Stripe INVOICE."
  (list :id (nlk:json-value invoice :string "id")
        :number (nlk:json-value invoice :string "number")
        :email email
        :name name
        :currency (or (nlk:json-value invoice :string "currency") "usd")
        :amount amount
        :status (or (nlk:json-value invoice :string "status") "draft")
        :link (or link (nlk:json-value invoice :string "hosted_invoice_url") "")
        :created (get-universal-time)
        :note ""))

(defun create-invoice (call)
  "The obol_create_invoice tool: a Stripe invoice, drafted or sent, and recorded here."
  (let* ((email (obol-create-customer-email call))
         (name (obol-create-customer-name call))
         (currency (string-downcase (or (nonblank (obol-create-currency call))
                                        (setting :default-currency))))
         (items (obol-create-line-items call))
         (days (let ((asked (obol-create-days-until-due call))) (if (and asked (plusp asked)) asked (setting :days-until-due))))
         (total (loop for item across items
                       sum (* (amount-minor (obol-line-item-amount item) currency)
                              (obol-line-item-quantity item)))))
    (unless (plusp (length items)) (fail "line_items needs at least one item"))
    (unless (nonblank email) 
      (fail "customer_email is required: Obol never guesses an address"))
    (let* ((flags (risk-flags email name total))
           (customer (customer-id email name))
           (invoice (stripe :post "/v1/invoices"
                            :data (list (cons "customer" customer)
                                        (cons "currency" currency)
                                        (cons "collection_method" "send_invoice")
                                        (cons "days_until_due" (princ-to-string days))
                                        (cons "auto_advance" "false"))))
           (id (nlk:json-value invoice :string "id")))
      (loop for item across items
            do (stripe :post "/v1/invoiceitems" :data (line-item-data id customer currency item)))
      (let* ((final (finish-invoice id (obol-create-send call)))
             (record (invoice-record final :email email :name name :amount total))
             (link (getf record :link)))
        (ledger-put record)
        (format nil "~@[~a -- ~]~a for ~a, ~a, ~a~@[ due in ~d days~].~@[ Payment link: ~a~]~%~
                     ~@[~%Risk checks:~%  - ~{~a~^~%  - ~}~]"
                (getf record :id) (getf record :number)
                (or name email) (stripe-money total currency)
                (string-capitalize (getf record :status))
                (and (equal (getf record :status) "draft") days)
                link flags)))))

(defun send-invoice (id)
  "The obol_send_invoice tool: a draft finalized and emailed by Stripe."
  (let* ((result (finish-invoice id t))
         (record (ledger-find id))
         (link (nlk:json-value result :string "hosted_invoice_url")))
    (when record
      (setf (getf record :status) "open" (getf record :link) (or link (getf record :link)))
      (ledger-put record))
    (format nil "~a sent to ~a~@[ -- payment link: ~a~]"
            id (or (getf record :email) "the customer") link)))

(defun send-reminder (id)
  "The obol_send_reminder tool: Stripe's payment email again, for an open invoice."
  (let ((record (ledger-find id)))
    (when (and record (not (equal (getf record :status) "open")))
      (fail "~a is ~a, so a reminder makes no sense" id (getf record :status)))
    (stripe :post (format nil "/v1/invoices/~a/send" id))
    (format nil "reminder sent for ~a to ~a" id (or (and record (getf record :email)) "the customer"))))

(defun void-invoice (id)
  "The obol_void_invoice tool: an open invoice voided, a draft deleted."
  (let* ((record (ledger-find id))
         (draft (equal (and record (getf record :status)) "draft"))
         (result (if draft
                     (stripe :delete (format nil "/v1/invoices/~a" id))
                     (stripe :post (format nil "/v1/invoices/~a/void" id)))))
    (when record
      (setf (getf record :status) (if draft "deleted" "void"))
      (ledger-put record))
    (format nil "~a ~a" id (nlk:json-value result :string "status"))))

(defun mark-paid (id note)
  "The obol_mark_paid tool: a payment recorded. Stripe is told too, when the
invoice lives there; in bank-transfer mode the ledger is the only record."
  (let ((record (ledger-find id)))
    (unless record (fail "no invoice ~a in this home's ledger" id))
    (when (and (ignore-errors (api-key)) (uiop:string-prefix-p "in_" id))
      (ignore-errors (stripe :post (format nil "/v1/invoices/~a/pay" id)
                             :data (list (cons "paid_out_of_band" "true")))))
    (setf (getf record :status) "paid" (getf record :note) (or note ""))
    (ledger-put record)
    (format nil "~a marked paid~@[ (~a)~]" id (and note (nonblank note)))))

(defun invoice-line (record)
  "One ledger RECORD as a line a person reads."
  (format nil "~a  ~a  ~a  ~a~@[  ~a~]"
          (or (getf record :id) "?")
          (if (and (getf record :name) (nonblank (getf record :email))) (format nil "~a <~a>" (getf record :name) (getf record :email)) (or (getf record :name) (getf record :email) "?"))
          (stripe-money (or (getf record :amount) 0) (or (getf record :currency) "usd"))
          (string-capitalize (or (getf record :status) "?"))
          (nonblank (getf record :link))))

(defun list-invoices (&key status)
  "The ledger's invoices, newest first, optionally only those in STATUS."
  (running-settings)
  (let ((records (if status
                     (remove-if-not (lambda (record) (equal (getf record :status) status))
                                    (ledger-load))
                     (ledger-load))))
    (if records
        (format nil "~{~a~^~%~}" (mapcar #'invoice-line records))
        (format nil "no invoices~@[ in status ~a~] yet" status))))

(defun get-invoice (id)
  "One invoice by ID: the ledger's copy, or Stripe's when this home has none."
  (running-settings)
  (let ((record (ledger-find id)))
    (unless record
      (let ((invoice (stripe :get (format nil "/v1/invoices/~a" id))))
        (setf record (invoice-record invoice
                                     :amount (nlk:json-value invoice :integer "amount_due")))))
    (format nil "~a~%status: ~a~%amount: ~a~@[~%link: ~a~]"
            (or (getf record :id) id)
            (or (getf record :status) "unknown")
            (stripe-money (or (getf record :amount) 0) (or (getf record :currency) "usd"))
            (nonblank (getf record :link)))))

;;; --- the tool boundaries ------------------------------------------------------------

(nlk:define-contract obol-line-item
  (:doc "One line of an invoice.")
  ("description" :text :required t :doc "what is being billed, e.g. October consulting")
  ("amount" :number :required t :doc "unit price in currency units: 1200 is 1,200.00")
  ("quantity" :integer :default 1 :doc "units billed, at least one"))

(nlk:define-contract obol-create
  (:doc "Draft a Stripe invoice for a client: their exact billing email, the lines, and send=true only when the user asked to send it now. Returns the invoice id, the total and any risk warnings. Live keys are refused unless allow_live is on, and a draft emails nobody until it is sent.")
  ("customer_email" :text :required t :doc "the client's billing email, exactly as the user gave it")
  ("customer_name" :string :default "" :doc "the client or company name")
  ("line_items" :array :of obol-line-item :required t :doc "what is billed")
  ("currency" :string :default "" :doc "ISO code such as usd; empty means the section's default")
  ("days_until_due" :integer :default 0 :doc "payment terms in days; 0 means the section's default")
  ("send" :boolean :default nil :doc "finalize and email the payment link now; the default is a draft"))

(nlk:define-contract obol-id
  (:doc "One invoice, by its Stripe id (in_...).")
  ("invoice_id" :text :required t :doc "the invoice id, e.g. in_1Q2w3E"))

(nlk:define-contract obol-mark-paid
  (:doc "Record that a client paid an invoice. Obol's own ledger is always updated; when the invoice is a Stripe one, Stripe is told it was paid out of band.")
  ("invoice_id" :text :required t :doc "the invoice id")
  ("note" :string :default "" :doc "how it was paid, e.g. 'bank transfer, ref 0004'"))

(nlk:define-contract obol-list
  (:doc "List the invoices this home has billed, newest first, with each one's amount and status (draft, open, paid, void).")
  ("status" :string :options '("" "draft" "open" "paid" "void") :default ""
            :doc "only invoices in this status; empty lists every one"))

(defun create-tool (call) (create-invoice call))
(defun send-tool (call) (send-invoice (obol-id-invoice-id call)))
(defun get-tool (call) (get-invoice (obol-id-invoice-id call)))
(defun reminder-tool (call) (send-reminder (obol-id-invoice-id call)))
(defun void-tool (call) (void-invoice (obol-id-invoice-id call)))
(defun paid-tool (call) (mark-paid (obol-mark-paid-invoice-id call)
                                    (obol-mark-paid-note call)))
(defun list-tool (call) (list-invoices :status (nonblank (obol-list-status call))))

;;; --- /obol -------------------------------------------------------------------------

(defun run-slash (args session-id)
  "/obol [overdue|paid|draft|open|sync]: the operator's own receivables view."
  (declare (ignore session-id))
  (running-settings)
  (let ((word (string-downcase (or (nonblank args) ""))))
    (cond ((member word '("overdue" "paid" "draft" "open" "void") :test #'string=)
           (list-invoices :status word))
          ((string= word "")
           (let* ((records (ledger-load))
                  (open (remove-if-not (lambda (record)
                                          (member (getf record :status) '("open" "overdue")
                                                  :test #'string=))
                                        records)))
             (format nil "~d invoice~:p billed here, ~d still unpaid, ~a outstanding.~
                          ~%~@[~%~{~a~^~%~}~]~%~
                          ~%/obol overdue, /obol paid, /obol sync"
                     (length records) (length open)
                     (stripe-money (reduce #'+ open :key (lambda (record) (or (getf record :amount) 0))
                                                  :initial-value 0)
                                   (setting :default-currency))
                     (and open (mapcar #'invoice-line open)))))
          (t (fail "obol takes overdue, paid, draft, open or sync, not ~s" word)))))

(defun sync ()
  "This home's ledger rewritten from Stripe's own list, newest 20."
  (let* ((found (stripe :get "/v1/invoices" :params (list (cons "limit" "20"))))
         (data (nlk:json-value found :array "data")))
    (loop for invoice across data
          do (ledger-put (invoice-record invoice
                                         :amount (nlk:json-value invoice :integer "amount_due")
                                         :email (nlk:json-value invoice :string "customer_email"))))
    (format nil "synced ~d invoices from Stripe" (length data))))

;;; --- the declaration ---------------------------------------------------------------

(defparameter *primer*
  "Obol bills clients from the conversation: obol_create_invoice drafts a Stripe
invoice (send=true emails the payment link), obol_send_invoice finalizes and sends a
draft, obol_list_invoices and obol_get_invoice say who has paid, obol_send_reminder
re-sends the payment email, obol_void_invoice cancels, and obol_mark_paid records a
payment that arrived another way. Amounts are in currency units: 1200 is 1,200.00.
Every invoice is a real email to the user's client, so send only when asked. The key
is the user's own Stripe secret or restricted key; with a live key and allow_live off,
Obol refuses to send anything."
  "What (help :obol) answers and what the model reads.")

(nle:define-cell obol
  (:section ("obol")
    (:guide "a Stripe secret or restricted key goes in api_key; a test key (sk_test_...) is safest; allow_live lets a live key bill real clients")
    ("api_key" :secret :doc "Stripe secret or restricted key")
    ("allow_live" :boolean :default nil :doc "let a live key send real invoices")
    ("default_currency" :string :default "usd" :doc "the ISO code an invoice uses when it names none")
    ("days_until_due" :integer :default 14 :min 1 :doc "payment terms in days when a call names none")
    ("timeout_seconds" :integer :default 20 :min 1 :doc "how long one Stripe exchange may take"))
  (:tool "obol_create_invoice" (nlk:contract 'obol-create) 'create-tool)
  (:tool "obol_send_invoice" (nlk:contract 'obol-id) 'send-tool)
  (:tool "obol_get_invoice" (nlk:contract 'obol-id) 'get-tool)
  (:tool "obol_list_invoices" (nlk:contract 'obol-list) 'list-tool)
  (:tool "obol_send_reminder" (nlk:contract 'obol-id) 'reminder-tool)
  (:tool "obol_void_invoice" (nlk:contract 'obol-id) 'void-tool)
  (:tool "obol_mark_paid" (nlk:contract 'obol-mark-paid) 'paid-tool)
  (:command "obol" 'run-slash
            :catches 'obol-error
            :description "Who owes what: invoices and payment links"
            :argument-hint "overdue | paid | sync")
  (:help :obol "obol: invoices and Stripe payment links - (obol:create-invoice ...), (obol:list-invoices)" *primer*))

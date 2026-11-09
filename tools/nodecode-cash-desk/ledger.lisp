;;;; ledger.lisp --- the ledger's file, what an export brings in, and which
;;;; payment pays which invoice.
;;;;
;;;; The ledger is one JSON file under the home, mode 0600: customers,
;;;; invoices, bank payments, the matches, the receipts still in review, the
;;;; drafts and what was sent. Amounts are whole cents; dates are YYYY-MM-DD.
;;;;
;;;; A payment is applied by itself only when its reference names exactly one
;;;; invoice, in that invoice's currency, for exactly its open balance, from a
;;;; payer that fits the customer, dated no earlier than three days before the
;;;; invoice was issued. Everything else becomes a review line with its
;;;; candidate invoices, and a receipt in review pauses reminders for the
;;;; invoices it may pay.

(in-package #:nodecode-cash-desk)

;;; --- the file's shape ------------------------------------------------------------

(defparameter +customer-fields+
  '((:id "id" :string "") (:name "name" :string "") (:language "language" :string "en")
    (:email "email" :string "") (:contact-name "contact_name" :string "")
    (:on-hold "on_hold" :integer 0) (:hold-reason "hold_reason" :string "")
    (:reply-kind "reply_kind" :string "") (:promised-date "promised_date" :string "")
    (:note "note" :string ""))
  "A customer: who they are, where a reminder goes, and why a chase is paused.")

(defparameter +invoice-fields+
  '((:number "number" :string "") (:customer-id "customer_id" :string "")
    (:issue-date "issue_date" :string "") (:due-date "due_date" :string "")
    (:amount "amount" :integer 0) (:currency "currency" :string "")
    (:balance "balance" :integer 0) (:status "status" :string "open")
    (:reference "reference" :string "") (:iban "iban" :string "")
    (:paid-date "paid_date" :string "") (:note "note" :string "")
    (:source "source" :string ""))
  "An invoice: the gross, and the balance still owed, in whole cents.")

(defparameter +payment-fields+
  '((:id "id" :string "") (:date "date" :string "") (:amount "amount" :integer 0)
    (:currency "currency" :string "") (:reference "reference" :string "")
    (:counterparty "counterparty" :string "") (:memo "memo" :string "")
    (:source "source" :string "bank"))
  "One bank line: the amount signed as the account saw it.")

(defparameter +match-fields+
  '((:payment-id "payment_id" :string "") (:invoice-number "invoice_number" :string "")
    (:amount "amount" :integer 0) (:currency "currency" :string "")
    (:method "method" :string "") (:confidence "confidence" :string "")
    (:at "at" :string "") (:thanked "thanked" :integer 0))
  "A payment applied to an invoice, and how sure the rule was.")

(defparameter +review-fields+
  '((:payment-id "payment_id" :string "") (:kind "kind" :string "")
    (:detail "detail" :string "") (:holds "holds" :list ()) (:at "at" :string ""))
  "A payment a person has to look at, and the invoices it pauses.")

(defparameter +draft-fields+
  '((:id "id" :string "") (:kind "kind" :string "") (:customer-id "customer_id" :string "")
    (:invoice-numbers "invoice_numbers" :list ()) (:language "language" :string "en")
    (:subject "subject" :string "") (:body "body" :string "")
    (:recipients "recipients" :list ()) (:status "status" :string "pending")
    (:hash "hash" :string "") (:figures "figures" :list ())
    (:as-of "as_of" :string "") (:created "created" :string "")
    (:sender "sender" :string ""))
  "A drafted reminder: the exact text, its figures, and the hash a person approves.")

(defparameter +chase-fields+
  '((:invoice-number "invoice_number" :string "") (:stage "stage" :string "")
    (:at "at" :string ""))
  "The highest reminder stage already drafted for one invoice.")

(defparameter +import-fields+
  '((:fingerprint "fingerprint" :string "") (:kind "kind" :string "")
    (:file "file" :string "") (:rows "rows" :integer 0) (:at "at" :string ""))
  "One import, by the fingerprint of the file it read, so it is never done twice.")

(defun record-to-json (record fields)
  "RECORD as a JSON object of FIELDS."
  (apply #'nlk:make-json-object
         (loop for (key wire type default) in fields
               append (list wire
                             (case type
                               (:integer (or (getf record key) 0))
                               (:list (coerce (or (getf record key) '()) 'vector))
                               (t (or (getf record key) "")))))))

(defun json-to-record (object fields)
  "OBJECT as a plist of FIELDS, every field defaulted."
  (let ((record '()))
    (loop for (key wire type default) in fields
          for value = (case type
                        (:integer (nlk:json-value object :integer wire))
                        (:list (let ((array (nlk:json-value object :array wire)))
                                 (if array (coerce array 'list) '())))
                        (t (nlk:json-value object :string wire)))
          do (setf (getf record key) (if (null value) default value)))
    record))

(defun array-of (records fields)
  "RECORDS as the JSON array FIELDS describes."
  (coerce (mapcar (lambda (record) (record-to-json record fields)) records) 'vector))

;;; --- reading and writing the ledger ---------------------------------------------

(defun ledger-path ()
  "The ledger's one file."
  (nlk:home "cash-desk/ledger.json"))

(defun ledger-dir ()
  "The folder the ledger and its outbox live in."
  (nlk:home "cash-desk/"))

(defun empty-ledger ()
  "A ledger with nothing in it yet."
  (list :customers '() :invoices '() :payments '() :matches '() :reviews '()
        :drafts '() :approvals '() :sends '() :chase '() :imports '()
        :next-draft 1 :kv '()))

(defun read-ledger ()
  "The ledger. A missing or torn file is an empty ledger, never an error."
  (let* ((text (nlk:read-text (ledger-path)))
         (object (and text (ignore-errors (nlk:decode-json text)))))
    (if (null object)
        (empty-ledger)
        (list :customers (loop for raw across (or (nlk:json-value object :array "customers") #())
                               when (hash-table-p raw) collect (json-to-record raw +customer-fields+))
              :invoices (loop for raw across (or (nlk:json-value object :array "invoices") #())
                              when (hash-table-p raw) collect (json-to-record raw +invoice-fields+))
              :payments (loop for raw across (or (nlk:json-value object :array "payments") #())
                              when (hash-table-p raw) collect (json-to-record raw +payment-fields+))
              :matches (loop for raw across (or (nlk:json-value object :array "matches") #())
                             when (hash-table-p raw) collect (json-to-record raw +match-fields+))
              :reviews (loop for raw across (or (nlk:json-value object :array "reviews") #())
                             when (hash-table-p raw) collect (json-to-record raw +review-fields+))
              :drafts (loop for raw across (or (nlk:json-value object :array "drafts") #())
                            when (hash-table-p raw) collect (json-to-record raw +draft-fields+))
              :approvals (loop for raw across (or (nlk:json-value object :array "approvals") #())
                               when (hash-table-p raw) collect (json-to-record raw +approval-fields+))
              :sends (loop for raw across (or (nlk:json-value object :array "sends") #())
                           when (hash-table-p raw) collect (json-to-record raw +send-fields+))
              :chase (loop for raw across (or (nlk:json-value object :array "chase") #())
                           when (hash-table-p raw) collect (json-to-record raw +chase-fields+))
              :imports (loop for raw across (or (nlk:json-value object :array "imports") #())
                             when (hash-table-p raw) collect (json-to-record raw +import-fields+))
              :next-draft (or (nlk:json-value object :integer "next_draft") 1)
              :kv (let ((object (nlk:json-value object :object "kv")) (pairs '()))
                    (when object
                      (maphash (lambda (key value) (push (cons key value) pairs)) object))
                    (nreverse pairs))))))

(defun write-ledger (board)
  "The ledger as its file: atomic, 0600, its folder 0700."
  (nlk:write-file-atomically
   (ledger-path)
   (nlk:encode-json-object
    (apply #'nlk:make-json-object
           (append
            (list "version" 1
                  "next_draft" (getf board :next-draft 1)
                  "customers" (array-of (getf board :customers) +customer-fields+)
                  "invoices" (array-of (getf board :invoices) +invoice-fields+)
                  "payments" (array-of (getf board :payments) +payment-fields+)
                  "matches" (array-of (getf board :matches) +match-fields+)
                  "reviews" (array-of (getf board :reviews) +review-fields+)
                  "drafts" (array-of (getf board :drafts) +draft-fields+)
                  "approvals" (array-of (getf board :approvals) +approval-fields+)
                  "sends" (array-of (getf board :sends) +send-fields+)
                  "chase" (array-of (getf board :chase) +chase-fields+)
                  "imports" (array-of (getf board :imports) +import-fields+))
            (list "kv" (apply #'nlk:make-json-object
                               (loop for (key . value) in (getf board :kv)
                                     append (list key value)))))))
   :mode #o600 :directory-mode #o700)
  board)

(defmacro with-ledger ((board) &body body)
  "BODY with BOARD bound, read and written back under the cell's lock."
  `(with-cash-desk-lock
     (let ((,board (read-ledger)))
       (prog1 (progn ,@body)
         (write-ledger ,board)))))

(defun ledger-kv (board key)
  "BOARD's KV entry KEY, or 0."
  (let ((pair (assoc key (getf board :kv) :test #'string=)))
    (if pair (cdr pair) 0)))

;;; --- looking things up ------------------------------------------------------------

(defun customer-by-id (board id)
  "BOARD's customer ID, or NIL."
  (find id (getf board :customers) :key (lambda (c) (getf c :id)) :test #'string=))

(defun invoice-by-number (board number)
  "BOARD's invoice NUMBER, or NIL."
  (find number (getf board :invoices) :key (lambda (i) (getf i :number)) :test #'string=))

(defun customer-invoices (board customer-id)
  "The invoices of CUSTOMER-ID."
  (remove-if-not (lambda (invoice) (string= (getf invoice :customer-id) customer-id))
                  (getf board :invoices)))

(defun open-p (invoice)
  "Whether INVOICE may still be paid."
  (and (member (getf invoice :status) '("open" "disputed") :test #'string=)
       (plusp (getf invoice :balance))))

(defun void-p (invoice)
  (string= "void" (getf invoice :status)))

(defun customer-name (board id)
  (or (let ((customer (customer-by-id board id))) (and customer (getf customer :name)))
      id))

(defun slug (text)
  "TEXT as a lowercase id: letters and digits, one dash between words."
  (let ((out (with-output-to-string (s)
               (let ((dash nil))
                 (loop for char across (string-downcase (trim-text text))
                       do (if (alphanumericp char)
                              (progn (when dash (write-char #\- s)) (setf dash nil)
                                     (write-char char s))
                              (setf dash t)))))))
    (if (plusp (length out)) (string-right-trim "-" out) "customer")))

;;; --- the file an import reads ------------------------------------------------------

(defun resolve-import (path)
  "PATH as a truename inside the ledger folder or one of import_dirs, or a
refusal. Nothing outside them is opened."
  (let* ((path (trim-text path))
         (candidate (if (or (uiop:absolute-pathname-p (uiop:parse-native-namestring path))
                            (prefix-p "/" path))
                        (uiop:parse-native-namestring path)
                        (merge-pathnames path (ledger-dir))))
         (probe (probe-file candidate)))
    (when (zerop (length path))
      (fail "an import needs a CSV path"))
    (unless probe
      (fail "no file at ~a" path))
    (let ((allowed (cons (ledger-dir)
                         (loop for dir in (setting :import-dirs)
                               when (plusp (length (trim-text dir)))
                                 collect (uiop:parse-native-namestring (trim-text dir))))))
      (unless (loop for dir in allowed
                    for base = (probe-file dir)
                    when (and base (uiop:subpathp (truename probe) (truename base)))
                      return t)
        (fail "~a is outside the ledger folder and import_dirs: add its folder there first" path)))
    (truename probe)))

;;; --- CSV columns -------------------------------------------------------------------

(defparameter +invoice-number-columns+
  '("invoice no" "invoice number" "invoicenumber" "invoice id" "invoice" "number" "num" "no" "id"))
(defparameter +customer-id-columns+ '("customer id" "customerid" "customer account" "account"))
(defparameter +customer-name-columns+ '("customer full name" "customer name" "customer" "contact name" "contact" "name"))
(defparameter +email-columns+ '("email" "e-mail" "email address" "customer email" "contact email" "emailaddress"))
(defparameter +language-columns+ '("language" "lang" "jazyk"))
(defparameter +issue-columns+ '("issue date" "invoice date" "invoicedate" "issued" "date"))
(defparameter +due-columns+ '("due date" "duedate" "due" "datum splatnosti"))
(defparameter +amount-columns+ '("amount" "total" "gross" "celkem" "invoice amount"))
(defparameter +currency-columns+ '("currency" "curr" "měna"))
(defparameter +balance-columns+ '("balance" "open balance" "outstanding" "amount due" "amountdue" "balance due" "amount remaining" "remaining"))
(defparameter +paid-columns+ '("amount paid" "invoiceamountpaid" "paid amount" "paid"))
(defparameter +invoice-status-columns+ '("status" "stav"))
(defparameter +reference-columns+ '("reference" "variable symbol" "payment reference" "vs" "variabilní symbol"))
(defparameter +iban-columns+ '("iban" "bank account" "account iban"))
(defparameter +note-columns+ '("note" "notes" "poznámka"))

(defparameter +bank-id-columns+ '("id" "line id" "transaction id" "txn id" "payment id" "reference number"))
(defparameter +bank-date-columns+ '("date" "booking date" "value date" "transaction date" "posting date" "datum"))
(defparameter +bank-amount-columns+ '("amount" "value" "betrag" "částka"))
(defparameter +bank-debit-columns+ '("debit" "money out" "withdrawal" "paid out"))
(defparameter +bank-credit-columns+ '("credit" "money in" "deposit" "paid in"))
(defparameter +bank-direction-columns+ '("direction" "creditdebitindicator" "cdtdbtind" "dr cr"))
(defparameter +bank-status-columns+ '("status" "state"))
(defparameter +bank-counterparty-columns+ '("counterparty" "payer" "payer name" "sender" "payee" "name"))
(defparameter +bank-memo-columns+ '("memo" "description" "message" "note" "details"))

(defparameter +skipped-invoice-statuses+ '("draft" "submitted" "awaiting approval" "deleted")
  "A row in one of these has not been issued, so it is not a chaseable invoice.")

(defun status-of (text)
  "The ledger status a file's status word means, or :skip."
  (let ((text (string-downcase (trim-text text))))
    (cond ((member text '("" "open" "overdue" "authorised" "authorized" "awaiting payment"
                          "sent" "partially paid" "part paid")
                 :test #'string=) "open")
          ((member text '("paid" "closed" "deposited" "applied" "settled") :test #'string=) "paid")
          ((member text '("void" "voided" "deleted" "uncollectible" "written off") :test #'string=) "void")
          ((string= text "disputed") "disputed")
          ((member text +skipped-invoice-statuses+ :test #'string=) :skip)
          (t "open"))))

(defun slash-date-parts (text)
  "TEXT's three numeric parts when it is a slashed date."
  (let* ((text (trim-text text))
         (parts (split-text text #\/)))
    (when (= 3 (length parts))
      (let ((a (number-text (first parts))) (b (number-text (second parts)))
            (c (number-text (third parts))))
        (when (and a b c) (list a b c))))))

(defun ambiguous-slash-p (text)
  "Whether TEXT is a slashed date whose day and month could swap."
  (let ((parts (slash-date-parts text)))
    (and parts (< (first parts) 13) (< (second parts) 13) (/= (first parts) (second parts)))))

;;; --- importing --------------------------------------------------------------------

(defun parse-row-date (text order lang what)
  "TEXT as a day number for WHAT, refusing an ambiguous slashed date."
  (when (and (string= order "auto") (ambiguous-slash-p text))
    (fail "~s reads both ways (day first or month first): set date_order to dmy or mdy" text))
  (or (parse-date text :order order :lang lang)
      (fail "~s is not a date in ~a" text what)))

(defun ledger-customer (board id name lang)
  "The customer ID, created under NAME when absent."
  (or (customer-by-id board id)
      (let ((customer (list :id id :name (if (plusp (length (trim-text name))) name id)
                            :language lang :email "" :contact-name "" :on-hold 0
                            :hold-reason "" :reply-kind "" :promised-date "" :note "")))
        (setf (getf board :customers) (append (getf board :customers) (list customer)))
        customer)))

(defun import-customers (board rows header options)
  "Read customer ROWS into BOARD. => (VALUES ADDED UPDATED PROBLEMS)."
  (let* ((lang (getf options :lang "en"))
         (id-at (find-column header +customer-id-columns+))
         (name-at (required-column header +customer-name-columns+ "name"))
         (email-at (find-column header +email-columns+))
         (language-at (find-column header +language-columns+))
         (contact-at (find-column header '("contact name" "contact")))
         (hold-at (find-column header '("on hold" "on_hold" "hold")))
         (reason-at (find-column header '("hold reason" "hold_reason")))
         (added 0) (updated 0) (problems '()))
    (loop for row in rows
          for line from 2
          do (let* ((name (cell row name-at))
                    (id (or (let ((given (cell row id-at))) (and (plusp (length given)) given))
                            (slug name)))
                    (text-language (string-downcase (cell row language-at)))
                    (customer (customer-by-id board id)))
               (cond
                 ((zerop (length name))
                  (push (format nil "line ~d: a customer needs a name" line) problems))
                 (customer
                  (setf (getf customer :name) name)
                  (when (plusp (length (cell row email-at)))
                    (setf (getf customer :email) (cell row email-at)))
                  (when (member text-language '("en" "cs") :test #'string=)
                    (setf (getf customer :language) text-language))
                  (when (plusp (length (cell row contact-at)))
                    (setf (getf customer :contact-name) (cell row contact-at)))
                  (when hold-at
                    (setf (getf customer :on-hold)
                          (if (member (string-downcase (cell row hold-at))
                                      '("yes" "true" "1" "y") :test #'string=) 1 0)))
                  (when (plusp (length (cell row reason-at)))
                    (setf (getf customer :hold-reason) (cell row reason-at)))
                  (incf updated))
                 (t
                  (let ((customer (ledger-customer board id name lang)))
                    (when (plusp (length (cell row email-at)))
                      (setf (getf customer :email) (cell row email-at)))
                    (when (member text-language '("en" "cs") :test #'string=)
                      (setf (getf customer :language) text-language))
                    (when (plusp (length (cell row contact-at)))
                      (setf (getf customer :contact-name) (cell row contact-at)))
                    (when hold-at
                      (setf (getf customer :on-hold)
                            (if (member (string-downcase (cell row hold-at))
                                        '("yes" "true" "1" "y") :test #'string=) 1 0)))
                    (when (plusp (length (cell row reason-at)))
                      (setf (getf customer :hold-reason) (cell row reason-at)))
                    (incf added))))))
    (values added updated problems)))

(defun header-currency (header)
  "The currency a header cell like \"amount (czk)\" names, or NIL."
  (loop for name in header
        for open = (position #\( name)
        for close = (position #\) name)
        when (and open close (> close (1+ open)))
          do (let ((code (string-upcase (subseq name (1+ open) close))))
               (when (member code +currency-codes+ :test #'string=) (return code)))))

(defun import-invoices (board rows header options)
  "Read invoice ROWS into BOARD. => (VALUES ADDED UPDATED SKIPPED PROBLEMS)."
  (let* ((lang (getf options :lang "en"))
         (order (getf options :date-order "auto"))
         (source (getf options :source "generic"))
         (default-currency (or (getf options :default-currency) ""))
         (number-at (required-column header +invoice-number-columns+ "invoice number"))
         (id-at (find-column header +customer-id-columns+))
         (name-at (find-column header +customer-name-columns+))
         (email-at (find-column header +email-columns+))
         (language-at (find-column header +language-columns+))
         (issue-at (required-column header +issue-columns+ "issue date"))
         (due-at (find-column header +due-columns+))
         (amount-at (required-column header +amount-columns+ "amount"))
         (currency-at (find-column header +currency-columns+))
         (balance-at (find-column header +balance-columns+))
         (paid-at (find-column header +paid-columns+))
         (status-at (find-column header +invoice-status-columns+))
         (reference-at (find-column header +reference-columns+))
         (iban-at (find-column header +iban-columns+))
         (note-at (find-column header +note-columns+))
         (file-currency (or (header-currency header) default-currency (setting :currency)))
         (added 0) (updated 0) (skipped 0) (problems '()))
    (unless (or id-at name-at)
      (fail "the customer column is missing: one of ~{~a~^, ~}" +customer-id-columns+))
    (loop for row in rows
          for line from 2
          do (handler-case
                 (let* ((number (cell row number-at))
                        (status (if status-at (status-of (cell row status-at)) "open"))
                        (issue-text (cell row issue-at))
                        (due-text (if due-at (cell row due-at) ""))
                        (currency (string-upcase
                                   (or (let ((given (if currency-at (cell row currency-at) "")))
                                         (and (plusp (length given)) given))
                                       file-currency)))
                        (name (if name-at (cell row name-at) ""))
                        (given-id (if id-at (cell row id-at) ""))
                        (customer-id (if (plusp (length given-id)) given-id (slug name))))
                   (cond
                     ((zerop (length number))
                      (incf skipped) (push (format nil "line ~d: no invoice number" line) problems))
                     ((eq status :skip)
                      (incf skipped))
                     ((zerop (length due-text))
                      (incf skipped) (push (format nil "line ~d: ~a has no due date" line number) problems))
                     (t
                      (let* ((issue (parse-row-date issue-text order lang (format nil "invoice ~a's issue date" number)))
                             (due (parse-row-date due-text order lang (format nil "invoice ~a's due date" number)))
                             (amount (parse-amount (cell row amount-at) :lang lang :what (format nil "invoice ~a's amount" number)))
                             (paid (if (and paid-at (plusp (length (cell row paid-at))))
                                       (parse-amount (cell row paid-at) :lang lang :what "the paid amount")
                                       0))
                             (balance (cond ((and balance-at (plusp (length (cell row balance-at))))
                                             (parse-amount (cell row balance-at) :lang lang :what "the balance"))
                                            ((string= status "paid") 0)
                                            (t (max 0 (- amount paid)))))
                             (invoice (invoice-by-number board number)))
                        (when (plusp (length name))
                          (ledger-customer board customer-id name (setting :default-language)))
                        (if invoice
                            (progn
                              (setf (getf invoice :balance) balance
                                    (getf invoice :status) status
                                    (getf invoice :due-date) (iso-date due)
                                    (getf invoice :issue-date) (iso-date issue))
                              (when (plusp (length (cell row reference-at)))
                                (setf (getf invoice :reference) (cell row reference-at)))
                              (when (plusp (length (cell row iban-at)))
                                (setf (getf invoice :iban) (cell row iban-at)))
                              (when (string= status "paid")
                                (setf (getf invoice :paid-date) (iso-date (or (ignore-errors
                                                                               (parse-row-date
                                                                                (cell row (find-column header '("paid date" "payment date")))
                                                                                order lang "the paid date"))
                                                                             due))))
                              (incf updated))
                            (let ((invoice (list :number number :customer-id customer-id
                                                 :issue-date (iso-date issue) :due-date (iso-date due)
                                                 :amount amount :currency currency :balance balance
                                                 :status status
                                                 :reference (if reference-at (cell row reference-at) "")
                                                 :iban (if iban-at (cell row iban-at) "")
                                                 :paid-date "" :source source
                                                 :note (if note-at (cell row note-at) ""))))
                              (setf (getf board :invoices) (append (getf board :invoices) (list invoice)))
                              (incf added)))))))
              (cash-desk-error (condition)
                (incf skipped)
                (push (format nil "line ~d: ~a" line (cash-desk-error-detail condition)) problems))))
    (values added updated skipped (nreverse problems))))

(defun bank-sign (direction)
  "The sign a direction word gives an unsigned amount, or NIL when unknown."
  (let ((direction (string-upcase (trim-text direction))))
    (cond ((member direction '("CRDT" "CR" "C" "CREDIT" "HABEN" "H" "IN" "BIJ") :test #'string=) 1)
          ((member direction '("DBIT" "DR" "D" "DEBIT" "SOLL" "S" "OUT" "AF") :test #'string=) -1)
          (t nil))))

(defun import-bank (board rows header options)
  "Read bank ROWS into BOARD. => (VALUES ADDED SKIPPED PROBLEMS)."
  (let* ((lang (getf options :lang "en"))
         (order (getf options :date-order "auto"))
         (default-currency (getf options :default-currency))
         (id-at (find-column header +bank-id-columns+))
         (date-at (required-column header +bank-date-columns+ "date"))
         (amount-at (find-column header +bank-amount-columns+))
         (debit-at (find-column header +bank-debit-columns+))
         (credit-at (find-column header +bank-credit-columns+))
         (direction-at (find-column header +bank-direction-columns+))
         (status-at (find-column header +bank-status-columns+))
         (currency-at (find-column header +currency-columns+))
         (counterparty-at (find-column header +bank-counterparty-columns+))
         (reference-at (find-column header +reference-columns+))
         (memo-at (find-column header +bank-memo-columns+))
         (file-currency (or (header-currency header) default-currency (setting :currency)))
         (added 0) (skipped 0) (problems '()))
    (unless (or amount-at debit-at credit-at)
      (fail "the amount column is missing: one of ~{~a~^, ~}" +bank-amount-columns+))
    (loop for row in rows
          for line from 2
          do (handler-case
                 (let* ((status (if status-at (string-downcase (cell row status-at)) "")))
                   (cond
                     ((member status '("pending" "failed" "scheduled") :test #'string=) (incf skipped))
                     ((zerop (length (cell row date-at)))
                      (incf skipped) (push (format nil "line ~d: a bank line needs a date" line) problems))
                     (t
                      (let* ((date (parse-row-date (cell row date-at) order lang "a bank line's date"))
                             (currency (string-upcase (or (let ((given (if currency-at (cell row currency-at) "")))
                                                            (and (plusp (length given)) given))
                                                          file-currency)))
                             (signed (cond
                                       (amount-at
                                        (let ((value (parse-amount (cell row amount-at) :lang lang :what "a bank amount")))
                                          (if direction-at
                                              (let ((sign (bank-sign (cell row direction-at))))
                                                (unless sign (fail "~s is not a direction" (cell row direction-at)))
                                                (if (minusp value) value (* sign (abs value))))
                                              value)))
                                       (t
                                        (let ((debit (if (plusp (length (cell row debit-at)))
                                                         (parse-amount (cell row debit-at) :lang lang :what "a debit")
                                                         0))
                                              (credit (if (plusp (length (cell row credit-at)))
                                                          (parse-amount (cell row credit-at) :lang lang :what "a credit")
                                                          0)))
                                          (when (and (plusp debit) (plusp credit))
                                            (fail "a line carries both a debit and a credit"))
                                          (- credit debit)))))
                             (id (or (let ((given (if id-at (cell row id-at) "")))
                                       (and (plusp (length given)) given))
                                     (format nil "~a-L~d" (subseq (getf options :fingerprint "") 0 8) line))))
                        (if (find id (getf board :payments) :key (lambda (p) (getf p :id)) :test #'string=)
                            (incf skipped)
                            (progn
                              (setf (getf board :payments)
                                    (append (getf board :payments)
                                            (list (list :id id :date (iso-date date) :amount signed
                                                        :currency currency
                                                        :reference (if reference-at (cell row reference-at) "")
                                                        :counterparty (if counterparty-at (cell row counterparty-at) "")
                                                        :memo (if memo-at (cell row memo-at) "")
                                                        :source "bank"))))
                              (incf added)))))))
              (cash-desk-error (condition)
                (incf skipped)
                (push (format nil "line ~d: ~a" line (cash-desk-error-detail condition)) problems))))
    (values added skipped (nreverse problems))))

(defun import-file (kind path &key (source "generic") default-currency date-order allow-errors)
  "Import a CSV export into the ledger. KIND is customers, invoices or bank.
The same file is never imported twice. Reads only PATH, inside the ledger
folder or import_dirs."
  (let* ((kind (string-downcase (trim-text kind)))
         (order (if (member date-order '("dmy" "mdy") :test #'string=) date-order "auto"))
         (file (resolve-import (if (pathnamep path) (namestring path) path)))
         (text (nlk:read-text file))
         (fingerprint (nlk:sha256-text text)))
    (unless (member kind '("customers" "invoices" "bank") :test #'string=)
      (fail "~s is not an import kind: customers, invoices or bank" kind))
    (when (zerop (length (trim-text (or text ""))))
      (fail "~a is empty" (file-namestring file)))
    (with-ledger (board)
      (let ((seen (find fingerprint (getf board :imports)
                         :key (lambda (entry) (getf entry :fingerprint)) :test #'string=)))
        (when seen
          (fail "~a was already imported on ~a as ~a"
                (file-namestring file) (getf seen :at) (getf seen :kind))))
      (let* ((options (list :source source :default-currency default-currency
                            :date-order order :fingerprint fingerprint
                            :lang (setting :default-language)))
             (groups (cond ((string= kind "customers")
                            (list +customer-name-columns+ +customer-id-columns+))
                           ((string= kind "invoices")
                            (list +invoice-number-columns+ +issue-columns+ +amount-columns+))
                           (t (list +bank-date-columns+))))
             (rows nil) (header nil))
        (multiple-value-setq (header rows) (read-table text groups))
        (multiple-value-bind (added updated skipped problems)
            (cond ((string= kind "customers")
                   (multiple-value-bind (added updated problems)
                       (import-customers board rows header options)
                     (values added updated 0 problems)))
                  ((string= kind "invoices") (import-invoices board rows header options))
                  (t (multiple-value-bind (added skipped problems)
                         (import-bank board rows header options)
                       (values added 0 skipped problems))))
          (when (and problems (not allow-errors))
            (fail "~d row~:p refused, nothing imported: ~{~a~^; ~}~@[ (and ~d more)~]~
                   ~%Pass allow_errors to import the rest."
                  (+ skipped (length problems)) (subseq problems 0 (min 5 (length problems)))
                  (max 0 (- (length problems) 5))))
          (setf (getf board :imports)
                (append (getf board :imports)
                        (list (list :fingerprint fingerprint :kind kind
                                    :file (file-namestring file)
                                    :rows (+ added updated (length rows))
                                    :at (iso-date (days-today))))))
          (format nil "imported ~a: ~d added~@[, ~d updated~]~@[, ~d skipped~]~@[; ~d problem~:p: ~{~a~^; ~}~]"
                  (file-namestring file) added (and (plusp updated) updated)
                  (and (plusp skipped) skipped)
                  (and problems (length problems)) (subseq problems 0 (min 5 (length problems)))))))))

;;; --- matching payments to invoices ------------------------------------------------

(defun token-present-p (haystack needle)
  "Whether NEEDLE appears in HAYSTACK as whole letters and digits."
  (let ((canon (canonical-id needle))
        (text (make-string-output-stream))
        (sources (make-array 0 :adjustable t :fill-pointer 0)))
    (when (>= (length canon) 3)
      (loop for char across haystack
            for index from 0
            when (alphanumericp char)
              do (write-char (char-upcase char) text)
                 (vector-push-extend index sources))
      (let ((flat (get-output-stream-string text))
            (start 0)
            (found nil))
        (loop for at = (search canon flat :start2 start)
              while at
              do (let* ((from (aref sources at))
                        (to (aref sources (+ at (1- (length canon)))))
                        (before (and (> from 0) (char haystack (1- from))))
                        (after (and (< (1+ to) (length haystack)) (char haystack (1+ to)))))
                   (when (and (or (null before) (not (alphanumericp before)))
                              (or (null after) (not (alphanumericp after))))
                     (setf found t)
                     (return)))
                 (setf start (1+ at)))
        found))))

(defun names-invoice-p (text invoice)
  "Whether TEXT names INVOICE by its number or its payment reference."
  (let ((reference (getf invoice :reference)))
    (or (token-present-p text (getf invoice :number))
        (and (plusp (length (trim-text reference)))
             (token-present-p text reference)))))

(defparameter +legal-words+
  '("ltd" "limited" "gmbh" "inc" "llc" "plc" "co" "company" "the" "and" "sro"
    "bv" "nv" "ag" "sa" "sas" "oy" "ab" "as" "spa" "srl")
  "Words dropped before a payer is compared with a customer.")

(defun normalise-name (text)
  "TEXT as lowercase words without punctuation or a legal form."
  (join-text " " (loop for word in (split-words (string-downcase (or text "")))
                        for clean = (string-trim ".,()&" word)
                        when (and (plusp (length clean))
                                  (not (member clean +legal-words+ :test #'string=)))
                          collect clean)))

(defun payer-fits-p (counterparty customer-name)
  "Whether the payer COUNTERPARTY can be CUSTOMER-NAME: equal once normalised,
or a bank truncation of at least twelve characters and two whole words."
  (let* ((counterparty (if (position #\, counterparty)
                            (join-text " " (reverse (split-text counterparty #\,)))
                            counterparty))
         (payer (normalise-name counterparty))
         (name (normalise-name customer-name)))
    (cond ((or (zerop (length payer)) (zerop (length name))) t)
          ((string= payer name) t)
          (t (flet ((truncation (short long)
                      (and (<= (length short) (length long))
                           (prefix-p short long)
                           (>= (length short) 12)
                           (>= (length (split-words short)) 2))))
               (or (truncation payer name) (truncation name payer)))))))

(defun apply-payment (board payment invoice method)
  "PAYMENT closes INVOICE in full."
  (setf (getf invoice :balance) 0
        (getf invoice :status) "paid"
        (getf invoice :paid-date) (getf payment :date))
  (setf (getf board :matches)
        (append (getf board :matches)
                (list (list :payment-id (getf payment :id)
                            :invoice-number (getf invoice :number)
                            :amount (getf payment :amount)
                            :currency (getf payment :currency)
                            :method method :confidence "high"
                            :at (iso-date (days-today)) :thanked 0)))))

(defun review-payment (board payment kind detail holds)
  "PAYMENT goes to review, and HOLDS pauses reminders for those invoices."
  (setf (getf board :reviews)
        (append (getf board :reviews)
                (list (list :payment-id (getf payment :id) :kind kind :detail detail
                            :holds (remove-duplicates holds :test #'string=)
                            :at (iso-date (days-today)))))))

(defun named-by (board text)
  "The invoices TEXT names."
  (remove-duplicates
   (loop for invoice in (getf board :invoices)
         when (names-invoice-p text invoice) collect invoice)
   :test (lambda (a b) (string= (getf a :number) (getf b :number)))))

(defun decide-one (board payment invoice)
  "What to do with PAYMENT when it names one INVOICE. => (VALUES KIND DETAIL)."
  (let ((amount (getf payment :amount))
        (balance (getf invoice :balance))
        (customer (customer-by-id board (getf invoice :customer-id)))
        (paid-date (parse-date (getf payment :date)))
        (issue (parse-date (getf invoice :issue-date))))
    (cond
      ((string= "void" (getf invoice :status)) (values :review "VOID" nil))
      ((not (string= (getf payment :currency) (getf invoice :currency)))
       (values :review "CURRENCY" nil))
      ((not (plusp balance)) (values :review "ALREADY_PAID" nil))
      ((< amount balance)
       (values :review "PARTIAL"
               (format nil "~a of ~a; ~a still open" 
                       (format-cents amount (getf payment :currency))
                       (format-cents balance (getf invoice :currency))
                       (format-cents (- balance amount) (getf invoice :currency)))))
      ((> amount balance)
       (values :review "OVERPAID"
               (format nil "~a over the open balance"
                       (format-cents (- amount balance) (getf payment :currency)))))
      ((not (payer-fits-p (getf payment :counterparty) (if customer (getf customer :name) "")))
       (values :review "PAYER" nil))
      ((and paid-date issue (< paid-date (- issue 3)))
       (values :review "EARLY" nil))
      (t (values :apply "reference+exact" nil)))))

(defun decide-payment (board payment as-of)
  "PAYMENT's decision against BOARD at AS-OF. => (VALUES ACTION KIND DETAIL HOLDS)."
  (let* ((text (format nil "~a ~a" (getf payment :reference) (getf payment :memo)))
         (named (named-by board text))
         (amount (getf payment :amount))
         (currency (getf payment :currency))
         (date (parse-date (getf payment :date)))
         (numbers (mapcar (lambda (invoice) (getf invoice :number)) named)))
    (cond
      ((and date (> date as-of)) (values :review "AFTER_AS_OF" "dated after the report date" '()))
      ((minusp amount) (values :review "CREDIT_NOTE" "a credit or a refund" numbers))
      ((= 1 (length named))
       (let ((invoice (first named)))
         (multiple-value-bind (action kind detail) (decide-one board payment invoice)
           (if (eq action :apply)
               (values :apply kind nil numbers)
               (values :review kind detail numbers)))))
      ((> (length named) 1)
       (let* ((customer (getf (first named) :customer-id))
              (open (remove-if-not #'open-p named))
              (total (loop for invoice in open sum (getf invoice :balance))))
         (if (and (= (length open) (length named))
                  (every (lambda (invoice) (string= currency (getf invoice :currency))) named)
                  (every (lambda (invoice) (string= customer (getf invoice :customer-id))) named)
                  (= total amount)
                  (payer-fits-p (getf payment :counterparty)
                                (customer-name board customer)))
             (values :apply "reference+exact-batch" nil numbers)
             (values :review "MULTI_INVOICE"
                     (format nil "~d invoices named; their open balances total ~a"
                             (length named) (format-cents total currency))
                     numbers))))
      (t
       (let* ((open (remove-if-not (lambda (invoice)
                                      (and (open-p invoice)
                                           (string= currency (getf invoice :currency))
                                           (= amount (getf invoice :balance))))
                                    (getf board :invoices)))
              (pair (loop for a in open
                          append (loop for b in open
                                       when (and (not (eq a b))
                                                 (string= (getf a :customer-id) (getf b :customer-id))
                                                 (= (+ (getf a :balance) (getf b :balance)) amount))
                                         collect (list a b)))))
         (cond
           ((= 1 (length open))
            (values :review "MISSING"
                    (format nil "an open invoice for exactly this amount: ~a"
                            (getf (first open) :number))
                    (list (getf (first open) :number))))
           ((> (length open) 1)
            (values :review "AMBIGUOUS"
                    (format nil "~d open invoices for this currency and amount: ~{~a~^, ~}"
                            (length open) (mapcar (lambda (invoice) (getf invoice :number)) open))
                    (mapcar (lambda (invoice) (getf invoice :number)) open)))
           (pair
            (values :review "MULTI_INVOICE"
                    (format nil "two invoices of one customer add up to this amount: ~{~a~^, ~}"
                            (mapcar (lambda (invoice) (getf invoice :number)) (first pair)))
                    (mapcar (lambda (invoice) (getf invoice :number)) (first pair))))
           (t (values :review "UNMATCHED" "no invoice of this currency has this open balance" '()))))))))

(defun match-payments (&key as-of)
  "Match bank payments to open invoices. A payment is applied only when its
reference names exactly one invoice, in that invoice's currency, for exactly
its open balance, from a payer that fits the customer. Everything else goes
to a review list, and a receipt in review pauses that invoice's reminders.
Nothing is sent."
  (with-ledger (board)
    (let ((as-of (as-of-days as-of))
          (applied 0) (reviewed 0) (lines '()))
      (dolist (payment (getf board :payments))
        (unless (or (find (getf payment :id) (getf board :matches)
                          :key (lambda (match) (getf match :payment-id)) :test #'string=)
                    (find (getf payment :id) (getf board :reviews)
                          :key (lambda (review) (getf review :payment-id)) :test #'string=))
          (multiple-value-bind (action kind detail holds)
              (decide-payment board payment as-of)
            (cond
              ((eq action :apply)
               (dolist (number holds)
                 (let ((invoice (invoice-by-number board number)))
                   (when invoice (apply-payment board payment invoice kind))))
               (incf applied)
               (push (format nil "~a ~a: applied ~a"
                             (getf payment :date) (getf payment :id) kind) lines))
              (t
               (review-payment board payment kind detail holds)
               (incf reviewed)
               (push (format nil "~a ~a: ~a~@[ (~a)~]~@[; holds ~{~a~^, ~}~]"
                             (getf payment :date) (getf payment :id) kind detail holds)
                     lines))))))
      (format nil "matched: ~d applied, ~d in review~@[~%~{~a~^~%~}~]"
              applied reviewed (nreverse lines)))))

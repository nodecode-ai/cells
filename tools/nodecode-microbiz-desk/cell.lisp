;;;; cell.lisp --- MicroBiz Desk: customers, sales, debts and a weekly report.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of Danzilla0/microbiz-desk (see NOTICE). The original's five
;;;; tools with their arguments and JSON answers, the money arithmetic in
;;;; whole minor units so totals never drift, the customer lookup that never
;;;; guesses between two names, the refusal of an overpayment, and the words
;;;; of the report are kept. Where the original kept three SQLite tables under
;;;; the harness home, this keeps one JSON file in the folder the section
;;;; names; the harness seams are Nodecode's: the section is the config, a
;;;; tool is one (:tool ...) clause of NLE:DEFINE-CELL, and the original's
;;;; /microbiz command is a slash command beside them. Nothing dials
;;;; anything: the desk is a file on this computer.

(defpackage #:nodecode-microbiz-desk
  (:use #:cl)
  (:export #:microbiz-desk-error))

(in-package #:nodecode-microbiz-desk)

(nlk:define-peripheral microbiz-desk :not-running t :lock "microbiz-desk")

(defconstant +format-version+ 1 "The desk file's own format, so a later one can tell.")
(defparameter *max-days* 3650 "The furthest back one report may look, in days.")
(defparameter *top-count* 3 "How many best customers and top items a report names.")
(defparameter *currency-default* "NGN" "The label amounts are shown in when the section names none.")

;;; --- where the desk is -----------------------------------------------------------

(defun data-folder ()
  "The folder the desk's file lives in, as the section names it."
  (uiop:ensure-directory-pathname (or (setting :data-dir) "")))

(defun desk-path ()
  "The desk's one file."
  (merge-pathnames "desk.json" (data-folder)))

(defun currency ()
  "The label the amounts are shown in; amounts are kept in its minor units."
  (let ((named (nlk:trimmed (or (setting :currency) ""))))
    (if (plusp (length named)) named *currency-default*)))

(defun now-text (&optional (unix (nlk:unix-now)))
  "UNIX as the UTC ISO-8601 stamp a row carries, to the second."
  (nlk:iso-time unix :millis nil))

(defun stamp-unix (stamp)
  "The Unix second an ISO stamp names, or NIL for anything that is not one."
  (let ((universal (nlk:iso-universal stamp)))
    (and universal (- universal nlk:+unix-epoch+))))

(defun day-text (stamp)
  "STAMP's date alone, YYYY-MM-DD, as a statement prints it."
  (subseq stamp 0 (min 10 (length stamp))))

(defun days-since (stamp &optional (now (nlk:unix-now)))
  "Whole days from STAMP to NOW, never negative; 0 for a stamp that does not read."
  (let ((then (stamp-unix stamp)))
    (if then (max 0 (floor (- now then) 86400)) 0)))

;;; --- money -----------------------------------------------------------------------

(defun read-number (text)
  "TEXT -- digits, commas and at most one dot -- as an exact rational of major
units, or NIL for anything that is not one."
  (let ((digits (remove #\, (nlk:trimmed (or text "")))))
    (when (and (plusp (length digits))
               (every (lambda (character) (or (digit-char-p character) (char= character #\.))) digits)
               (<= (count #\. digits) 1)
               (some #'digit-char-p digits))
      (let* ((dot (position #\. digits))
             (whole (if dot (subseq digits 0 dot) digits))
             (fraction (if dot (subseq digits (1+ dot)) "")))
        (+ (if (plusp (length whole)) (parse-integer whole) 0)
           (if (plusp (length fraction))
               (/ (parse-integer fraction) (expt 10 (length fraction)))
               0))))))

(defun exact-number (value)
  "VALUE in major units as an exact rational, or NIL when it is not a finite
number: a JSON number, a ratio, or the text a person typed at the command."
  (cond ((integerp value) value)
        ((floatp value) (and (<= (abs value) 1d300) (rational value)))
        ((rationalp value) value)
        ((stringp value) (read-number value))
        (t nil)))

(defun to-minor (value field &key allow-zero)
  "VALUE in whole minor units -- kobo, cents -- refusing what is not a number
or not a positive amount, the way the original's Decimal did."
  (let ((exact (exact-number value)))
    (unless exact
      (fail "~a must be a number" field))
    (let ((minor (floor (+ (* exact 100) 1/2))))
      (when (or (minusp minor) (and (zerop minor) (not allow-zero)))
        (fail "~a must be greater than zero" field))
      minor)))

(defun major (minor)
  "MINOR, whole minor units, as the number an answer quotes."
  (coerce (/ minor 100) 'double-float))

(defun format-minor (minor)
  "MINOR in the currency, its cents shown only when it has any."
  (multiple-value-bind (whole cents) (floor (abs minor) 100)
    (format nil "~a ~:[~;-~]~:d~@[.~2,'0d~]"
            (currency) (minusp minor) whole (and (plusp cents) cents))))

(defun report-days (value)
  "VALUE -- how many days back -- as a whole number between 1 and *MAX-DAYS*."
  (let ((days (cond ((integerp value) value)
                    ((and (floatp value) (= value (floor value))) (floor value))
                    ((stringp value) (let ((text (nlk:trimmed value)))
                                       (and (plusp (length text))
                                            (every #'digit-char-p text)
                                            (parse-integer text)))))))
    (unless (and days (<= 1 days *max-days*))
      (fail "days must be a whole number between 1 and ~d" *max-days*))
    days))

;;; --- the desk file ---------------------------------------------------------------

(defparameter +customer-fields+
  '((:id "id" :integer) (:name "name" :string) (:phone "phone" :string)
    (:created "created_at" :string))
  "A customer: who they are, where a reminder could go, and when they were added.")

(defparameter +sale-fields+
  '((:id "id" :integer) (:customer "customer_id" :integer) (:item "item" :string)
    (:amount "amount" :integer) (:created "created_at" :string))
  "One sale: what was sold, and the whole price in minor units.")

(defparameter +payment-fields+
  '((:id "id" :integer) (:customer "customer_id" :integer) (:amount "amount" :integer)
    (:note "note" :string) (:created "created_at" :string))
  "One payment: what a customer paid toward what they owe, in minor units.")

(defun record-to-json (record fields)
  "RECORD as a JSON object of FIELDS."
  (apply #'nlk:make-json-object
         (loop for (key wire type) in fields
               append (list wire (case type
                                   (:integer (or (getf record key) 0))
                                   (t (or (getf record key) "")))))))

(defun json-to-record (object fields)
  "OBJECT as a plist of FIELDS, every one defaulted."
  (let ((record '()))
    (dolist (field fields record)
      (destructuring-bind (key wire type) field
        (setf (getf record key) (or (case type
                                      (:integer (nlk:json-value object :integer wire))
                                      (t (nlk:json-value object :string wire)))
                                    (if (eq type :integer) 0 "")))))))

(defun read-records (object wire fields)
  "OBJECT's array WIRE as a list of records of FIELDS."
  (loop for raw across (or (nlk:json-value object :array wire) #())
        when (hash-table-p raw) collect (json-to-record raw fields)))

(defun records-array (records fields)
  "RECORDS as the JSON array FIELDS describes."
  (coerce (mapcar (lambda (record) (record-to-json record fields)) records) 'vector))

(defun read-desk ()
  "The desk as a plist of :CUSTOMERS, :SALES and :PAYMENTS. A missing or torn
file is an empty desk, never an error: the first call writes one."
  (let* ((text (ignore-errors (nlk:read-text (desk-path))))
         (object (and text (plusp (length (nlk:trimmed text)))
                      (ignore-errors (nlk:decode-json text)))))
    (if (hash-table-p object)
        (list :customers (read-records object "customers" +customer-fields+)
              :sales (read-records object "sales" +sale-fields+)
              :payments (read-records object "payments" +payment-fields+))
        (list :customers '() :sales '() :payments '()))))

(defun desk-json (desk)
  "DESK as the JSON object its file holds."
  (nlk:json-object
   "format" +format-version+
   "customers" (records-array (getf desk :customers) +customer-fields+)
   "sales" (records-array (getf desk :sales) +sale-fields+)
   "payments" (records-array (getf desk :payments) +payment-fields+)))

(defun write-desk (desk)
  "DESK to its one file, atomically, under the folder's lock so two processes
share one desk and never write it from two stale reads."
  (let ((file (desk-path)))
    (ensure-directories-exist file)
    (nlk:with-folder-lock ((data-folder))
      (nlk:write-file-atomically file
                                  (format nil "~a~%" (nlk:encode-json-object (desk-json desk)))
                                  :mode #o600 :directory-mode #o700))
    desk))

(defmacro with-desk ((desk) &body body)
  "BODY with DESK read from the file, and written back when it answers."
  `(with-microbiz-desk-lock
     (let ((,desk (read-desk)))
       (multiple-value-prog1 (progn ,@body)
         (write-desk ,desk)))))

(defmacro answering (&body body)
  "BODY's answer, or the refusal it signals as the ERROR: line the model
repairs from -- the shape every tool of the original answers a refusal in."
  `(handler-case (progn ,@body)
     (microbiz-desk-error (condition)
       (nle:failure (format nil "ERROR: microbiz-desk: ~a" condition)))
     ((or error sb-sys:deadline-timeout) (condition)
       (declare (ignore condition))
       (nle:failure "ERROR: microbiz-desk: something went wrong saving or reading business data."))))

;;; --- arguments -------------------------------------------------------------------

(defun argument (args name)
  "ARGS' NAME, or NIL; ARGS is NIL when a call carries no arguments at all."
  (and (hash-table-p args) (gethash name args)))

(defun argument-text (args name)
  (let ((value (argument args name)))
    (and (stringp value) value)))

;;; --- customers -------------------------------------------------------------------

(defun collapse (text)
  "TEXT with every run of blanks cut to one space, and its ends bare."
  (format nil "~{~a~^ ~}" (nlk:split-words (or text ""))))

(defun clean-name (name)
  "NAME as one customer name, refusing an empty one."
  (let ((clean (collapse name)))
    (if (plusp (length clean))
        clean
        (fail "customer name is required"))))

(defun matching-customers (desk name)
  "The customers NAME names: the exact one when one matches exactly, else
every name it is part of, in name order."
  (let* ((clean (clean-name name))
         (customers (getf desk :customers))
         (exact (remove-if-not (lambda (customer) (string-equal clean (getf customer :name)))
                                customers)))
    (if exact
        exact
        (let ((needle (string-downcase clean)))
          (sort (remove-if-not (lambda (customer) (search needle (string-downcase (getf customer :name))))
                                customers)
                #'string-lessp :key (lambda (customer) (getf customer :name)))))))

(defun resolve-customer (desk name)
  "VALUES: the one customer NAME names and true, or NIL and NIL when none
does; refuses when NAME fits more than one, as the original never guessed."
  (let ((matches (matching-customers desk name)))
    (cond ((= 1 (length matches)) (values (first matches) t))
          ((> (length matches) 1)
           (fail "'~a' matches more than one customer (~{~a~^, ~}). Use the full name."
                 (clean-name name)
                 (mapcar (lambda (customer) (getf customer :name)) matches)))
          (t (values nil nil)))))

(defun customer-named (desk name)
  "The one customer NAME names; refuses when none does."
  (multiple-value-bind (customer found) (resolve-customer desk name)
    (if found
        customer
        (fail "No customer called '~a'. Record a sale first to create them." (clean-name name)))))

(defun next-id (records)
  "The next id for RECORDS, one past the largest."
  (1+ (reduce #'max records :key (lambda (record) (or (getf record :id) 0)) :initial-value 0)))

(defun ensure-customer (desk name phone stamp)
  "The customer NAME names, created when no sale has named them yet."
  (multiple-value-bind (customer found) (resolve-customer desk name)
    (if found
        customer
        (let ((new (list :id (next-id (getf desk :customers))
                         :name (clean-name name)
                         :phone (or phone "")
                         :created stamp)))
          (setf (getf desk :customers) (append (getf desk :customers) (list new)))
          new))))

(defun balance (desk customer-id)
  "What the customer still owes DESK: every sale less every payment."
  (flet ((total (records)
           (reduce #'+ records :initial-value 0
                   :key (lambda (record)
                          (if (= customer-id (getf record :customer)) (getf record :amount) 0)))))
    (- (total (getf desk :sales)) (total (getf desk :payments)))))

(defun last-sale-stamp (desk customer-id)
  "When the customer last bought on credit, or NIL when never."
  (reduce (lambda (best sale)
            (if (= customer-id (getf sale :customer)) (getf sale :created) best))
          (getf desk :sales) :initial-value nil))

;;; --- the verbs the tools and the command both call -------------------------------

(defun record-sale (customer item amount &key amount-paid phone (now (nlk:unix-now)))
  "Record a sale, creating the customer when new. Answers a plist."
  (let* ((item (collapse item))
         (total (to-minor amount "amount"))
         (paid (to-minor (or amount-paid 0) "amount_paid" :allow-zero t)))
    (unless (plusp (length item))
      (fail "item is required"))
    (when (> paid total)
      (fail "amount_paid cannot be more than the sale amount"))
    (let ((stamp (now-text now)))
      (with-desk (desk)
        (let* ((who (ensure-customer desk customer phone stamp))
               (id (getf who :id))
               (name (getf who :name)))
          (setf (getf desk :sales)
                (append (getf desk :sales)
                        (list (list :id (next-id (getf desk :sales)) :customer id :item item
                                    :amount total :created stamp))))
          (when (plusp paid)
            (setf (getf desk :payments)
                  (append (getf desk :payments)
                          (list (list :id (next-id (getf desk :payments)) :customer id
                                      :amount paid :note "paid at sale" :created stamp)))))
          (let ((owed (balance desk id)))
            (list :customer name :item item :sale-amount total :paid-now paid :balance owed
                  :summary (format nil "Recorded ~a sale of ~a to ~a. Paid now: ~a. ~a now owes ~a."
                                   (format-minor total) item name (format-minor paid)
                                   name (format-minor owed)))))))))

(defun record-payment (customer amount &key (note "") (now (nlk:unix-now)))
  "Record money a customer pays toward what they owe. Answers a plist."
  (let ((paid (to-minor amount "amount")))
    (let ((stamp (now-text now)))
      (with-desk (desk)
        (let* ((who (customer-named desk customer))
               (id (getf who :id))
               (owed (balance desk id)))
          (when (<= owed 0)
            (fail "~a does not owe anything right now." (getf who :name)))
          (when (> paid owed)
            (fail "~a only owes ~a, so ~a is too much. Check the amount."
                  (getf who :name) (format-minor owed) (format-minor paid)))
          (setf (getf desk :payments)
                (append (getf desk :payments)
                        (list (list :id (next-id (getf desk :payments)) :customer id
                                    :amount paid :note (nlk:trimmed (or note "")) :created stamp))))
          (let ((left (- owed paid)))
            (list :customer (getf who :name) :paid paid :balance left
                  :summary (format nil "Recorded ~a from ~a. ~a"
                                   (format-minor paid) (getf who :name)
                                   (if (plusp left)
                                       (format nil "Still owes ~a." (format-minor left))
                                       "Fully paid up.")))))))))

(defun debtors (&key (now (nlk:unix-now)))
  "Every customer with an unpaid balance, biggest debt first, each a plist of
:CUSTOMER, :PHONE, :OWES and :DAYS since their last sale."
  (let ((desk (read-desk)))
    (let ((rows (loop for customer in (getf desk :customers)
                      for owed = (balance desk (getf customer :id))
                      when (plusp owed)
                        collect (list :customer (getf customer :name)
                                      :phone (getf customer :phone)
                                      :owes owed
                                      :days (days-since (last-sale-stamp desk (getf customer :id)) now)))))
      (sort rows (lambda (left right)
                   (if (= (getf left :owes) (getf right :owes))
                       (string-lessp (getf left :customer) (getf right :customer))
                       (> (getf left :owes) (getf right :owes))))))))

(defun debtors-summary (rows)
  "ROWS as /microbiz-desk owing reads them."
  (if (null rows)
      "Nobody owes you anything right now."
      (format nil "~d customer(s) owe you ~a in total:~%~{~a~^~%~}"
              (length rows)
              (format-minor (reduce #'+ rows :key (lambda (row) (getf row :owes)) :initial-value 0))
              (loop for row in rows
                    for position from 1
                    collect (format nil "~d. ~a: ~a (last sale ~d days ago)"
                                    position (getf row :customer)
                                    (format-minor (getf row :owes)) (getf row :days))))))

(defun statement (customer)
  "One customer's whole history: sales, payments and the balance. Answers a plist."
  (let* ((desk (read-desk))
         (who (customer-named desk customer))
         (id (getf who :id))
         (sales (remove-if-not (lambda (sale) (= id (getf sale :customer))) (getf desk :sales)))
         (payments (remove-if-not (lambda (payment) (= id (getf payment :customer)))
                                   (getf desk :payments)))
         (owed (balance desk id)))
    (list :customer (getf who :name) :sales sales :payments payments :balance owed
          :summary (with-output-to-string (out)
                     (format out "Statement for ~a:~%" (getf who :name))
                     (dolist (sale sales)
                       (format out "  SALE  ~a  ~a  ~a~%" (day-text (getf sale :created))
                               (getf sale :item) (format-minor (getf sale :amount))))
                     (dolist (payment payments)
                       (format out "  PAID  ~a  ~a~@[  (~a)~]~%"
                               (day-text (getf payment :created))
                               (format-minor (getf payment :amount))
                               (let ((note (getf payment :note)))
                                 (and (plusp (length note)) note))))
                     (format out "Balance owed: ~a" (format-minor owed))))))

(defun in-period-p (stamp since)
  "Whether STAMP falls at or after SINCE, in Unix seconds."
  (let ((unix (stamp-unix stamp)))
    (and unix (>= unix since))))

(defun top-customers (desk sales)
  "The customers whose SALES came to most, at most *TOP-COUNT* of them."
  (let ((totals (make-hash-table :test #'eql)))
    (dolist (sale sales)
      (incf (gethash (getf sale :customer) totals 0) (getf sale :amount)))
    (let ((rows (loop for id being the hash-keys of totals using (hash-value total)
                      collect (list :customer (or (customer-name desk id) "")
                                    :total total))))
      (subseq (sort rows (lambda (left right)
                           (if (= (getf left :total) (getf right :total))
                               (string-lessp (getf left :customer) (getf right :customer))
                               (> (getf left :total) (getf right :total)))))
              0 (min *top-count* (length rows))))))

(defun customer-name (desk id)
  "The name of the customer ID names, or NIL."
  (let ((customer (find id (getf desk :customers) :key (lambda (one) (getf one :id)))))
    (and customer (getf customer :name))))

(defun top-items (sales)
  "The items whose SALES came to most, one row per item without case, at most
*TOP-COUNT* of them."
  (let ((totals (make-hash-table :test #'equal)))
    (dolist (sale sales)
      (let ((row (gethash (string-downcase (getf sale :item)) totals)))
        (if row
            (progn (incf (getf row :total) (getf sale :amount))
                   (incf (getf row :count)))
            (setf (gethash (string-downcase (getf sale :item)) totals)
                  (list :item (getf sale :item) :total (getf sale :amount) :count 1)))))
    (let ((rows (loop for row being the hash-values of totals collect row)))
      (subseq (sort rows (lambda (left right)
                           (if (= (getf left :total) (getf right :total))
                               (string-lessp (getf left :item) (getf right :item))
                               (> (getf left :total) (getf right :total)))))
              0 (min *top-count* (length rows))))))

(defun report-summary (days sales-count revenue collected owed owing best items)
  "The plain-English report /microbiz-desk report and the tool both answer."
  (with-output-to-string (out)
    (format out "Business report: last ~d day(s)~%" days)
    (format out "Sales made: ~d totalling ~a~%" sales-count (format-minor revenue))
    (format out "Cash collected: ~a~%" (format-minor collected))
    (format out "Still owed to you (all time): ~a by ~d customer(s)~%"
            (format-minor owed) owing)
    (when best
      (format out "Best customers: ~{~a~^, ~}~%"
              (loop for row in best collect (format nil "~a (~a)"
                                                    (getf row :customer)
                                                    (format-minor (getf row :total))))))
    (when items
      (format out "Top sellers: ~{~a~^, ~}~%"
              (loop for row in items collect (format nil "~a (~a)"
                                                     (getf row :item)
                                                     (format-minor (getf row :total))))))
    (when (zerop sales-count)
      (format out "No sales recorded in this period yet.~%"))))

(defun business-report (&key (days 7) (now (nlk:unix-now)))
  "The last DAYS days as a plist: what was sold, what was collected, what is
still owed, and the best customers and items."
  (let* ((days (report-days days))
         (since (- now (* days 86400)))
         (desk (read-desk))
         (sales (remove-if-not (lambda (sale) (in-period-p (getf sale :created) since))
                                (getf desk :sales)))
         (payments (remove-if-not (lambda (payment) (in-period-p (getf payment :created) since))
                                   (getf desk :payments)))
         (revenue (reduce #'+ sales :key (lambda (sale) (getf sale :amount)) :initial-value 0))
         (collected (reduce #'+ payments :key (lambda (payment) (getf payment :amount)) :initial-value 0))
         (rows (debtors :now now))
         (owed (reduce #'+ rows :key (lambda (row) (getf row :owes)) :initial-value 0))
         (best (top-customers desk sales))
         (items (top-items sales)))
    (list :days days :sales-count (length sales) :revenue revenue :collected collected
          :owed owed :owing (length rows) :best best :items items
          :summary (report-summary days (length sales) revenue collected owed (length rows)
                                   best items))))

;;; --- the tools -------------------------------------------------------------------

(defun tool-schema (description properties)
  "DESCRIPTION and PROPERTIES -- (NAME TYPE DOC REQUIRED-P) ... -- as a tool schema."
  (list :description description
        :parameters
        (nlk:json-object
         "type" "object"
         "additionalProperties" :false
         "properties" (apply #'nlk:make-json-object
                             (loop for (name type doc) in properties
                                   nconc (list name (nlk:json-object "type" type "description" doc))))
         "required" (coerce (loop for property in properties
                                   when (fourth property) collect (first property))
                            'vector))))

(defparameter *add-sale-schema*
  (tool-schema
   "Record a sale for a small business and create the customer if new. Use when the user says they sold something, e.g. 'I sold 2 bags of rice to Mama Ngozi for 45000, she paid 20000'. Set amount_paid to what the customer paid right now (0 if all on credit). Amounts are in the currency the desk shows."
   '(("customer" "string" "Customer name, e.g. 'Mama Ngozi'." t)
     ("item" "string" "What was sold, e.g. '2 bags of rice'." t)
     ("amount" "number" "Total price of the sale." t)
     ("amount_paid" "number" "Amount paid now, 0 if fully on credit.")
     ("phone" "string" "Customer phone number (optional).")))
  "The microbiz_add_sale tool, as the original's schema has it.")

(defparameter *record-payment-schema*
  (tool-schema
   "Record money a customer has paid toward what they owe. Use when the user says a customer paid or cleared part of a debt, e.g. 'Mama Ngozi paid 10000'. A payment larger than the debt is refused."
   '(("customer" "string" "Customer name." t)
     ("amount" "number" "Amount paid." t)
     ("note" "string" "Optional note, e.g. 'bank transfer'.")))
  "The microbiz_record_payment tool.")

(defparameter *who-owes-schema*
  (tool-schema
   "List every customer who still owes money, biggest debt first, with the total owed and days since their last sale. Use for 'who owes me?', 'my debtors', or 'who should I remind?'."
   '())
  "The microbiz_who_owes_me tool.")

(defparameter *statement-schema*
  (tool-schema
   "Show one customer's full history of sales, payments and current balance."
   '(("customer" "string" "Customer name." t)))
  "The microbiz_customer_statement tool.")

(defparameter *report-schema*
  (tool-schema
   "Business summary for the last N days: sales made, cash collected, money still owed, best customers and top-selling items. Use for 'how was my week?' or 'give me a report'."
   '(("days" "integer" "How many days back, 1 to 3650 (default 7).")))
  "The microbiz_report tool.")

(defun answer (object)
  "OBJECT as the JSON text a tool answers."
  (nlk:encode-json-object object))

(defun tool-add-sale (args)
  "microbiz_add_sale: one sale, and the customer it created or found."
  (answering
   (let ((result (record-sale (argument-text args "customer") (argument-text args "item")
                              (argument args "amount")
                              :amount-paid (argument args "amount_paid")
                              :phone (argument-text args "phone"))))
     (answer (nlk:json-object
              "customer" (getf result :customer)
              "item" (getf result :item)
              "sale_amount" (major (getf result :sale-amount))
              "paid_now" (major (getf result :paid-now))
              "customer_now_owes" (major (getf result :balance))
              "summary" (getf result :summary))))))

(defun tool-record-payment (args)
  "microbiz_record_payment: money paid toward a debt."
  (answering
   (let ((result (record-payment (argument-text args "customer") (argument args "amount")
                                  :note (or (argument-text args "note") ""))))
     (answer (nlk:json-object
              "customer" (getf result :customer)
              "paid" (major (getf result :paid))
              "balance_left" (major (getf result :balance))
              "summary" (getf result :summary))))))

(defun tool-who-owes (args)
  "microbiz_who_owes_me: every debt, biggest first."
  (declare (ignore args))
  (answering
   (let ((rows (debtors)))
     (answer (nlk:json-object
              "total_owed" (major (reduce #'+ rows :key (lambda (row) (getf row :owes))
                                          :initial-value 0))
              "count" (length rows)
              "debtors" (coerce (loop for row in rows
                                       collect (nlk:json-object
                                                "customer" (getf row :customer)
                                                "phone" (getf row :phone)
                                                "owes" (major (getf row :owes))
                                                "days_since_last_sale" (getf row :days)))
                                 'vector)
              "summary" (debtors-summary rows))))))

(defun tool-customer-statement (args)
  "microbiz_customer_statement: one customer's history."
  (answering
   (let ((result (statement (argument-text args "customer"))))
     (answer (nlk:json-object
              "customer" (getf result :customer)
              "sales" (coerce (loop for sale in (getf result :sales)
                                     collect (nlk:json-object
                                              "item" (getf sale :item)
                                              "amount" (major (getf sale :amount))
                                              "date" (day-text (getf sale :created))))
                              'vector)
              "payments" (coerce (loop for payment in (getf result :payments)
                                        collect (nlk:json-object
                                                 "amount" (major (getf payment :amount))
                                                 "note" (getf payment :note)
                                                 "date" (day-text (getf payment :created))))
                                 'vector)
              "balance_owed" (major (getf result :balance))
              "summary" (getf result :summary))))))

(defun tool-report (args)
  "microbiz_report: the last N days, in words and numbers."
  (answering
   (let ((result (business-report :days (or (argument args "days") 7))))
     (answer (nlk:json-object
              "days" (getf result :days)
              "sales_count" (getf result :sales-count)
              "revenue" (major (getf result :revenue))
              "collected" (major (getf result :collected))
              "total_owed_all_time" (major (getf result :owed))
              "customers_owing" (getf result :owing)
              "top_customers" (coerce (loop for row in (getf result :best)
                                             collect (nlk:json-object
                                                      "customer" (getf row :customer)
                                                      "total" (major (getf row :total))))
                                       'vector)
              "top_items" (coerce (loop for row in (getf result :items)
                                         collect (nlk:json-object
                                                  "item" (getf row :item)
                                                  "total" (major (getf row :total))
                                                  "count" (getf row :count)))
                                   'vector)
              "summary" (getf result :summary))))))

;;; --- /microbiz-desk --------------------------------------------------------------

(defparameter +command-help+
  (format nil "MicroBiz Desk commands:~%  ~
               sale \"Customer\" \"Item\" AMOUNT [--paid AMOUNT] [--phone NUMBER]~%  ~
               pay \"Customer\" AMOUNT [--note TEXT]~%  ~
               owing~%  report [--days N]~%  statement \"Customer\"")
  "What /microbiz-desk with no arguments, and help, answers.")

(defun split-command-line (text)
  "TEXT split into words on blanks, single or double quotes keeping a word's
blanks inside it, as the original's shlex did. Second value: whether a quote
was left open."
  (let ((words '()) (current (make-string-output-stream)) (quote nil) (started nil))
    (loop for character across (or text "")
          do (cond (quote
                    (if (char= character quote)
                        (setf quote nil)
                        (write-char character current)))
                   ((or (char= character #\") (char= character #\'))
                    (setf quote character started t))
                   ((member character '(#\Space #\Tab #\Return #\Newline))
                    (when started
                      (push (get-output-stream-string current) words)
                      (setf started nil)))
                   (t (write-char character current) (setf started t)))
          finally (when started (push (get-output-stream-string current) words)))
    (values (nreverse words) (and quote t))))

(defun split-options (words names)
  "WORDS as (VALUES POSITIONAL OPTIONS): a --NAME VALUE pair for every word
whose name is in NAMES, every other word a positional one."
  (let ((positional '()) (options '()))
    (loop while words
          for word = (pop words)
          do (cond
               ((and (> (length word) 2) (string= word "--" :end1 2))
                (let ((name (string-downcase (subseq word 2))))
                  (unless (member name names :test #'string=)
                    (fail "~a is not an option; try --~{~a~^, --~}" word names))
                  (unless words
                    (fail "~a needs a value" word))
                  (setf (getf options (intern (string-upcase name) :keyword)) (pop words))))
               (t (push word positional))))
    (values (nreverse positional) options)))

(defun command-sale (words options)
  "`/microbiz-desk sale \"Customer\" \"Item\" AMOUNT [--paid AMOUNT]`."
  (unless (>= (length words) 3)
    (return-from command-sale
      (format nil "Usage: /microbiz-desk sale \"Customer\" \"Item\" AMOUNT [--paid AMOUNT]~%~a"
              +command-help+)))
  (getf (record-sale (first words) (second words) (third words)
                     :amount-paid (or (getf options :paid) 0)
                     :phone (getf options :phone))
        :summary))

(defun command-pay (words options)
  "`/microbiz-desk pay \"Customer\" AMOUNT [--note TEXT]`."
  (unless (>= (length words) 2)
    (return-from command-pay
      (format nil "Usage: /microbiz-desk pay \"Customer\" AMOUNT~%~a" +command-help+)))
  (getf (record-payment (first words) (second words) :note (or (getf options :note) ""))
        :summary))

(defun command-statement (words)
  "`/microbiz-desk statement \"Customer\"`."
  (unless words
    (return-from command-statement
      (format nil "Usage: /microbiz-desk statement \"Customer\"~%~a" +command-help+)))
  (getf (statement (first words)) :summary))

(defun run-command (args session-id)
  "One /microbiz-desk invocation: a sale, a payment, the debts, a report or a
statement, or the help."
  (declare (ignore session-id))
  ;; Asked first and outside the handler below: while the cell is not running
  ;; the command must refuse as MICROBIZ-DESK-ERROR, the way every other verb
  ;; does, and not answer it as the "Problem:" line a mistake gets.
  (running-settings)
  (multiple-value-bind (words open-quote) (split-command-line (nlk:trimmed (or args "")))
    (if open-quote
        (format nil "Problem: check your quotation marks.~%~a" +command-help+)
        (handler-case
            (let ((verb (string-downcase (or (first words) ""))))
              (multiple-value-bind (positional options)
                  (split-options (rest words) '("paid" "phone" "days" "note"))
                (cond
                  ((member verb '("" "help" "-h" "--help") :test #'string=) +command-help+)
                  ((string= verb "sale") (command-sale positional options))
                  ((string= verb "pay") (command-pay positional options))
                  ((string= verb "owing") (debtors-summary (debtors)))
                  ((string= verb "report")
                   (getf (business-report :days (or (getf options :days) 7)) :summary))
                  ((string= verb "statement") (command-statement positional))
                  (t (format nil "Unknown command: ~a. Try /microbiz-desk help." verb)))))
          (microbiz-desk-error (condition) (format nil "Problem: ~a" condition))
          (error (condition)
            (declare (ignore condition))
            "Problem: something went wrong. Please try again.")))))

;;; --- the manual ------------------------------------------------------------------

(defparameter +primer+
  "MicroBiz Desk keeps a small business's customers, sales and debts on this computer: one
JSON file in the folder the microbiz-desk section's data_dir names, no network, no key.
(help :microbiz-desk) is this page.
  microbiz_add_sale         {customer, item, amount, amount_paid?, phone?}
      one sale; the customer is created when new; amount_paid is what they paid now
  microbiz_record_payment   {customer, amount, note?}
      money paid toward a debt; more than the debt is refused
  microbiz_who_owes_me      {}   every debtor, biggest first, with days since their last sale
  microbiz_customer_statement {customer}   one customer's sales, payments and balance
  microbiz_report           {days?}   sales, cash collected, what is still owed, best
      customers and top items for the last N days (default 7)
Amounts are kept in whole minor units of the section's currency, so totals never drift;
a customer name may be partial when exactly one customer matches, and an ambiguous one is
refused rather than guessed. /microbiz-desk sale \"Customer\" \"Item\" AMOUNT [--paid AMOUNT],
/microbiz-desk pay \"Customer\" AMOUNT [--note TEXT], /microbiz-desk owing,
/microbiz-desk report [--days N], /microbiz-desk statement \"Customer\". A refusal is
ERROR: microbiz-desk with the reason, for the model to repair."
  "What (help :microbiz-desk) answers while the cell runs.")

;;; --- the cell --------------------------------------------------------------------

(nle:define-cell microbiz-desk
  (:section ("microbiz-desk")
    (:guide "data_dir is the folder the desk's file is kept in; next step: /microbiz-desk help")
    ("data_dir" :path :required t
     :doc "the folder the desk's one file is kept in, one desk per folder")
    ("currency" :string :default "NGN"
     :doc "the label amounts are shown in; amounts are kept in its minor units"))
  (:help :microbiz-desk "microbiz-desk: customers, sales, debts and a weekly report, offline" +primer+)
  (:command "microbiz-desk" 'run-command
            :description "Record a sale or a payment, list who owes, a statement, or the report"
            :argument-hint "sale · pay · owing · report · statement · help")
  (:tool "microbiz_add_sale" *add-sale-schema* 'tool-add-sale)
  (:tool "microbiz_record_payment" *record-payment-schema* 'tool-record-payment)
  (:tool "microbiz_who_owes_me" *who-owes-schema* 'tool-who-owes)
  (:tool "microbiz_customer_statement" *statement-schema* 'tool-customer-statement)
  (:tool "microbiz_report" *report-schema* 'tool-report)
  (:start (lambda ()
            ;; A desk needs a folder before it can have a file: a section that
            ;; names none refuses at start, naming the member.
            (unless (plusp (length (or (setting :data-dir) "")))
              (error 'nlk:config-refusal
                     :detail "microbiz-desk: data_dir names the folder the desk's file is kept in; set it in the microbiz-desk section of config.jsonc or in /settings"))
            (ensure-directories-exist (desk-path)))))

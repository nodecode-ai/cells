;;;; verbs.lisp --- what a person and the model do with the ledger.
;;;;
;;;; Reading: STATUS, BRIEF, QUEUE, LEDGER. Deciding: APPROVE and SEND-DRAFT,
;;;; both the person's. Rewording: REVISE, under Figure Lock. Recording what a
;;;; customer said: RECORD-REPLY, HOLD, RELEASE.

(in-package #:nodecode-cash-desk)

(defparameter +approval-fields+
  '((:draft-id "draft_id" :string "") (:hash "hash" :string "") (:at "at" :string ""))
  "One approval: the draft, the exact text's hash, and when it was given.")

(defparameter +send-fields+
  '((:draft-id "draft_id" :string "") (:hash "hash" :string "")
    (:file "file" :string "") (:at "at" :string ""))
  "One reminder written out to the outbox.")

;;; --- the draft a person is looking at ---------------------------------------------

(defun draft-by-id (board id)
  "BOARD's draft ID, or a refusal naming the queue."
  (or (find (trim-text id) (getf board :drafts) :key (lambda (d) (getf d :id)) :test #'string=)
      (fail "no draft ~a: cashdesk_queue lists the ids" (trim-text id))))

(defun draft-invoices (board draft)
  "The invoices DRAFT is about."
  (loop for number in (getf draft :invoice-numbers)
        for invoice = (invoice-by-number board number)
        when invoice collect invoice))

(defun draft-summary-line (draft)
  (format nil "~a [~a] ~a · ~a · hash ~a"
          (getf draft :id) (getf draft :status) (getf draft :kind)
          (getf draft :subject) (getf draft :hash)))

;;; --- reading ----------------------------------------------------------------------

(defun money-sum (records key)
  "RECORDS' KEY summed per currency, as text."
  (let ((by-currency '()))
    (dolist (record records)
      (let ((currency (getf record :currency))
            (cell (assoc (getf record :currency) by-currency :test #'string=)))
        (if cell
            (incf (cdr cell) (getf record key))
            (push (cons currency (getf record key)) by-currency))))
    (if by-currency
        (join-text " + " (loop for (currency . cents) in (sort by-currency #'string< :key #'car)
                                collect (format-cents cents currency)))
        "-")))

(defparameter +bucket-order+ '("not yet due" "1-30 days" "31-60 days" "61-90 days" "90+ days")
  "Age buckets, oldest reading last.")

(defun bucket-of (days)
  "The bucket DAYS overdue falls in."
  (cond ((<= days 0) "not yet due")
        ((<= days 30) "1-30 days")
        ((<= days 60) "31-60 days")
        ((<= days 90) "61-90 days")
        (t "90+ days")))

(defun status (&key as-of)
  "The ledger's state: open balance by age bucket and currency, drafts
waiting, customers on hold, and the last import. Read-only."
  (let* ((board (read-ledger))
         (as-of (as-of-days as-of))
         (open (remove-if-not #'open-p (getf board :invoices)))
         (buckets (make-hash-table :test #'equal)))
    (dolist (invoice open)
      (incf (gethash (cons (bucket-of (days-overdue invoice as-of)) (getf invoice :currency))
                      buckets 0)
            (getf invoice :balance)))
    (let ((lines '()))
      (dolist (label +bucket-order+)
        (let ((cells '()))
          (maphash (lambda (key cents)
                     (when (string= label (car key))
                       (push (cons (cdr key) cents) cells)))
                   buckets)
          (push (if cells
                    (format nil "~a: ~a" label
                            (join-text " | "
                                       (loop for (currency . cents) in (sort cells #'string< :key #'car)
                                             collect (format-cents cents currency))))
                    (format nil "~a: -" label))
                lines)))
      (format nil "Cash Desk, as of ~a~%open: ~d invoice~:p~%~{~a~^~%~}~%drafts: ~d pending, ~d approved, ~d sent~%holds: ~@[~{~a~^, ~}~]~%last import: ~a"
              (iso-date as-of)
              (length open)
              (nreverse lines)
              (count-if (lambda (d) (string= "pending" (getf d :status))) (getf board :drafts))
              (count-if (lambda (d) (string= "approved" (getf d :status))) (getf board :drafts))
              (count-if (lambda (d) (string= "sent" (getf d :status))) (getf board :drafts))
              (loop for customer in (getf board :customers)
                    when (plusp (getf customer :on-hold))
                      collect (format nil "~a (~a)" (getf customer :name)
                                      (getf customer :hold-reason)))
              (let ((last (first (last (getf board :imports)))))
                (if last
                    (format nil "~a, ~a (~d rows) on ~a" (getf last :kind)
                            (getf last :file) (getf last :rows) (getf last :at))
                    "none"))))))

(defun brief (&key (lang "en") as-of)
  "Morning cash brief: what arrived, what became overdue, what waits for
approval and who is on hold. Read-only: it drafts, revises and sends nothing."
  (let* ((board (read-ledger))
         (as-of (as-of-days as-of))
         (cs (string= lang "cs"))
         (stamp (ledger-kv board "last_brief"))
         (since (if (and (stringp stamp) (parse-date stamp))
                    (parse-date stamp)
                    (- as-of 1)))
         (payments (remove-if-not
                    (lambda (payment)
                      (let ((at (parse-date (getf payment :date))))
                        (and at (> at since) (plusp (getf payment :amount)))))
                    (getf board :payments)))
         (overdue (remove-if-not
                   (lambda (invoice)
                     (let ((due (parse-date (getf invoice :due-date))))
                       (and (open-p invoice) due (<= 1 (- as-of due) 7))))
                   (getf board :invoices)))
         (pending (count-if (lambda (d) (string= "pending" (getf d :status)))
                            (getf board :drafts)))
         (approved (count-if (lambda (d) (string= "approved" (getf d :status)))
                             (getf board :drafts)))
         (held (loop for customer in (getf board :customers)
                     when (plusp (getf customer :on-hold))
                       collect (getf customer :name))))
    (let ((pair (assoc "last_brief" (getf board :kv) :test #'string=)))
      (if pair
          (setf (cdr pair) (iso-date as-of))
          (setf (getf board :kv)
                (append (getf board :kv) (list (cons "last_brief" (iso-date as-of)))))))
    (write-ledger board)
    (format nil "~a~%~a~%~a~%~a~%~a"
            (if cs
                (format nil "Přehled Cash Desk k ~a" (iso-date as-of))
                (format nil "Cash Desk brief for ~a" (iso-date as-of)))
            (if cs
                (format nil "Přijaté platby od ~a: ~d (~a)" (iso-date since)
                        (length payments) (money-sum payments :amount))
                (format nil "Payments received since ~a: ~d (~a)" (iso-date since)
                        (length payments) (money-sum payments :amount)))
            (if cs
                (format nil "Nově po splatnosti: ~d (~a)" (length overdue)
                        (money-sum overdue :balance))
                (format nil "Newly overdue: ~d (~a)" (length overdue)
                        (money-sum overdue :balance)))
            (format nil "~a: ~d, ~d approved"
                    (if cs "Čeká na schválení" "Awaiting approval") pending approved)
            (format nil "~a: ~d~@[ (~{~a~^, ~})~]"
                    (if cs "Pozastaveno" "On hold") (length held) held))))

(defun queue (&key (status "pending") (customer-id ""))
  "The drafts by status, with their exact subject, body, recipients and the
hash a person approves."
  (let ((status (trim-text (or status "pending")))
        (customer-id (trim-text (or customer-id ""))))
    (unless (member status '("pending" "approved" "sent" "all") :test #'string=)
      (fail "~s is not a draft status: pending, approved, sent or all" status))
    (let ((drafts (remove-if-not
                   (lambda (draft)
                     (and (or (string= status "all")
                              (string= status (getf draft :status)))
                          (or (zerop (length customer-id))
                              (string= customer-id (getf draft :customer-id)))))
                   (getf (read-ledger) :drafts))))
      (if (null drafts)
          (format nil "no ~a drafts: cashdesk_draft plans today's follow-ups" status)
          (with-output-to-string (out)
            (format out "~d ~a draft~:p:~%" (length drafts) status)
            (dolist (draft drafts)
              (format out "~%~a~%Subject: ~a~%To: ~@[~{~a~^, ~}~]~%~a~%---~%"
                      (draft-summary-line draft)
                      (getf draft :subject)
                      (getf draft :recipients)
                      (getf draft :body))))))))

;;; --- Figure Lock -------------------------------------------------------------------

(defun figure-kind (figure)
  (let ((colon (position #\: figure)))
    (and colon (subseq figure 0 colon))))

(defun trim-token (token)
  "TOKEN without the punctuation around it."
  (string-trim '(#\Space #\Tab #\Newline #\Return #\, #\. #\; #\: #\( #\) #\[ #\]
                 #\< #\> #\" #\' #\« #\» #\! #\?)
                token))

(defun figure-token-canonicals (token)
  "Every canonical figure TOKEN could be, or NIL."
  (let* ((token (trim-token token))
         (length (length token)))
    (cond
      ((zerop length) nil)
      ((and (>= length 15) (<= length 34)
            (alpha-char-p (char token 0)) (alpha-char-p (char token 1))
            (digit-char-p (char token 2)) (digit-char-p (char token 3))
            (every #'alphanumericp token))
       (list (figure-iban token)))
      ((and (= length 10) (char= (char token 4) #\-) (char= (char token 7) #\-)
            (every #'digit-char-p (remove #\- token)))
       (let ((days (parse-date token)))
         (and days (list (figure-date (iso-date days))))))
      ((and (or (find #\. token) (find #\/ token))
            (let ((parts (remove-if #'zerop (split-text token (if (find #\. token) #\. #\/))
                                    :key #'length)))
              (and (= 3 (length parts))
                   (every (lambda (part) (number-text part)) parts))))
       (let ((dmy (parse-date token :order "dmy"))
             (mdy (parse-date token :order "mdy")))
         (remove-duplicates
          (remove nil (list (and dmy (figure-date (iso-date dmy)))
                            (and mdy (figure-date (iso-date mdy)))))
          :test #'string=)))
      ((and (>= length 4) (some #'alpha-char-p token) (some #'digit-char-p token))
       (list (figure-invoice token)))
      ((and (>= length 5) (every #'digit-char-p token))
       (list (figure-invoice token)))
      (t nil))))

(defun iban-head-p (token)
  "Whether TOKEN starts an IBAN a bank groups in fours: two letters, two digits."
  (let ((token (trim-token token)))
    (and (<= 4 (length token) 8)
         (alpha-char-p (char token 0)) (alpha-char-p (char token 1))
         (digit-char-p (char token 2)) (digit-char-p (char token 3))
         (every #'alphanumericp token))))

(defun gather-iban (tokens index)
  "The IBAN the tokens from INDEX spell, and how many tokens it took, or NIL."
  (when (iban-head-p (nth index tokens))
    (let ((text (string-upcase (trim-token (nth index tokens))))
          (used 1)
          (at (1+ index)))
      (loop while (and (< at (length tokens)) (< (length text) 34))
            for piece = (trim-token (nth at tokens))
            while (and (plusp (length piece)) (<= (length piece) 4)
                       (every #'alphanumericp piece)
                       (some #'digit-char-p piece))
            do (setf text (concatenate 'string text (string-upcase piece)))
               (incf used)
               (incf at))
      (values (list :text text :canonicals (list (figure-iban text))) used))))

(defun extract-figures (text)
  "Every figure TEXT prints, as plists (:TEXT :CANONICALS)."
  (let* ((tokens (split-words text))
         (count (length tokens))
         (found '())
         (index 0))
    (loop while (< index count)
          for token = (nth index tokens)
          do (multiple-value-bind (iban used) (gather-iban tokens index)
               (let ((next (nth (1+ index) tokens))
                     (canonicals (figure-token-canonicals token)))
                 (cond
                   (iban
                    (push iban found)
                    (incf index (1- used)))
                   ((and (member (string-upcase (trim-token token)) +currency-codes+
                                 :test #'string=)
                         next)
                    (let ((cents (ignore-errors (parse-amount next :what "an amount"))))
                      (when cents
                        (push (list :text (format nil "~a ~a" token next)
                                    :canonicals (list (figure-money
                                                       cents (string-upcase token))))
                              found))))
                   (canonicals
                    (push (list :text token :canonicals canonicals) found)))))
             (incf index))
    (nreverse found)))

(defun describe-figure (figure)
  "A canonical figure as a person reads it."
  (let* ((kind (figure-kind figure))
         (rest (subseq figure (1+ (length (or kind ""))))))
    (cond ((string= kind "money")
           (let ((colon (position #\: rest)))
             (format-cents (parse-integer rest :start (1+ colon))
                           (subseq rest 0 colon))))
          ((string= kind "date") rest)
          ((string= kind "invoice") (format nil "invoice ~a" rest))
          ((string= kind "reference") (format nil "reference ~a" rest))
          ((string= kind "iban") (format nil "IBAN ~a" (group-iban rest)))
          (t figure))))

(defun amount-explained (cents allowed)
  "Whether an amount without a currency is a ledger amount."
  (and cents
       (loop for figure in allowed
             when (and (string= "money" (figure-kind figure))
                       (= cents (let ((colon (position #\: figure :from-end t)))
                                  (parse-integer figure :start (1+ colon)))))
               return t)))

(defun check-figures (text required allowed)
  "Every figure TEXT prints is in ALLOWED, and every REQUIRED figure is
printed with a canonical form. => (VALUES FINDINGS SEEN)."
  (let ((seen '())
        (findings '()))
    (dolist (item (extract-figures text))
      (let ((canonicals (getf item :canonicals)))
        (cond
          ((loop for canonical in canonicals
                 when (member canonical allowed :test #'string=) return t)
           (setf seen (union canonicals seen :test #'string=)))
          ((amount-explained (ignore-errors (parse-amount (getf item :text)
                                                          :what "an amount"))
                             allowed)
           nil)
          (t (push (format nil "extra: ~a" (getf item :text)) findings)))))
    (dolist (canonical required)
      (unless (member canonical seen :test #'string=)
        (push (format nil "missing: ~a" (describe-figure canonical)) findings)))
    (values (nreverse findings) seen)))

(defun financial-lines (text)
  "TEXT's lines that carry a figure."
  (loop for line in (split-text text #\Newline)
        when (extract-figures line)
          collect (collapse-space line)))

(defun revise (draft-id new-text &key new-subject)
  "Reword a draft under Figure Lock: every amount, invoice number, date,
reference and IBAN exactly as the ledger has it, and nothing added. A
refusal leaves the draft untouched; an accepted revision gets a new hash and
needs a fresh approval."
  (let ((new-text (or new-text "")))
    (when (zerop (length (trim-text new-text)))
      (fail "a revision needs the complete new body"))
    (with-ledger (board)
      (let* ((draft (draft-by-id board draft-id))
             (invoices (draft-invoices board draft))
             (allowed (append (ledger-figures invoices) (getf draft :figures)))
             (required (getf draft :figures)))
        (unless (member (getf draft :status) '("pending" "approved") :test #'string=)
          (fail "draft ~a is ~a: plan a new one with cashdesk_draft"
                (getf draft :id) (getf draft :status)))
        (let ((findings (check-figures new-text required allowed)))
          (when findings
            (fail "Figure Lock rejected the revision; the draft is unchanged. ~{~a~^; ~}"
                  (subseq findings 0 (min 8 (length findings))))))
        (let ((new-lines (mapcar #'collapse-space (split-text new-text #\Newline)))
              (missing-lines '()))
          (dolist (line (financial-lines (getf draft :body)))
            (unless (find line new-lines :test #'string=)
              (push line missing-lines)))
          (when missing-lines
            (fail "Figure Lock: a financial line changed; keep it as it is: ~{~a~^; ~}"
                  (subseq missing-lines 0 (min 3 (length missing-lines))))))
        (let ((subject (let ((given (trim-text (or new-subject ""))))
                         (if (plusp (length given)) given (getf draft :subject)))))
          (setf (getf draft :subject) subject
                (getf draft :body) new-text
                (getf draft :status) "pending"
                (getf draft :hash) (draft-hash subject new-text))
          (format nil "revised ~a; new hash ~a; a person approves it again (/cashdesk approve ~a)"
                  (getf draft :id) (getf draft :hash) (getf draft :id)))))))

;;; --- the person's two decisions ------------------------------------------------------

(defun approve (draft-id)
  "A person approves the exact text of a pending draft. SEND-DRAFT then needs
this hash; a revision changes it and needs a fresh approval."
  (with-ledger (board)
    (let ((draft (draft-by-id board draft-id)))
      (unless (string= "pending" (getf draft :status))
        (fail "draft ~a is ~a, not pending" (getf draft :id) (getf draft :status)))
      (setf (getf draft :status) "approved")
      (setf (getf board :approvals)
            (append (getf board :approvals)
                    (list (list :draft-id (getf draft :id)
                                :hash (getf draft :hash)
                                :at (nlk:iso-time (nlk:unix-now) :millis nil)))))
      (format nil "approved ~a (~a); cashdesk_send sends it as an .eml file"
              (getf draft :id) (getf draft :hash)))))

(defun eml-text (draft recipient from)
  "DRAFT as the .eml file a mail client opens."
  (format nil "~@[From: ~a~%~]To: ~a~%Subject: ~a~%Date: ~a~%X-Cash-Desk-Draft: ~a ~a~%MIME-Version: 1.0~%Content-Type: text/plain; charset=utf-8~%~%~a~%"
          (and (plusp (length (trim-text from))) (trim-text from))
          recipient
          (getf draft :subject)
          (nlk:iso-now)
          (getf draft :id)
          (getf draft :hash)
          (getf draft :body)))

(defun send-draft (draft-id draft-hash)
  "Write one approved draft to the outbox as an .eml file per recipient.
Refused unless a person approved exactly this text."
  (with-ledger (board)
    (let* ((draft (draft-by-id board draft-id))
           (hash (trim-text (or draft-hash "")))
           (recipients (getf draft :recipients)))
      (unless (string= "approved" (getf draft :status))
        (fail "draft ~a is ~a: a person approves it first (/cashdesk approve ~a)"
              (getf draft :id) (getf draft :status) (getf draft :id)))
      (unless (string= hash (getf draft :hash))
        (fail "the hash ~a is not this text's (~a): approve the draft again after a revision"
              hash (getf draft :hash)))
      (when (null recipients)
        (fail "draft ~a is an internal note: it has no recipient" (getf draft :id)))
      (let* ((named (trim-text (setting :outbox)))
             (outbox (if (plusp (length named))
                         (uiop:parse-native-namestring named)
                         (nlk:home "cash-desk/outbox/")))
             (files '()))
        (ensure-directories-exist outbox)
        (loop for recipient in recipients
              for at from 1
              for file = (merge-pathnames
                          (format nil "~a-~d.eml" (string-downcase (getf draft :id)) at)
                          outbox)
              do (nlk:write-file-atomically
                  file (eml-text draft recipient (setting :from-address)) :mode #o600)
                 (push (file-namestring file) files))
        (setf files (nreverse files)
              (getf draft :status) "sent")
        (dolist (file files)
          (setf (getf board :sends)
                (append (getf board :sends)
                        (list (list :draft-id (getf draft :id) :hash (getf draft :hash)
                                    :file file
                                    :at (nlk:iso-time (nlk:unix-now) :millis nil))))))
        (format nil "sent ~a to ~{~a~^, ~}: ~{~a~^, ~} in the outbox; open ~a in your mail client to send it"
                (getf draft :id) recipients files (first files))))))

;;; --- what a customer said ------------------------------------------------------------

(defun record-reply (customer-id text &key kind promised-date)
  "Record a customer's reply as dispute, paid, promise or other. A dispute
holds the customer; a promise with a date pauses their reminders until two
days after it; paid changes nothing until the bank statement shows it."
  (let ((kind (string-downcase (trim-text (or kind "other")))))
    (unless (member kind '("dispute" "paid" "promise" "other") :test #'string=)
      (fail "~s is not a reply kind: dispute, paid, promise or other" kind))
    (with-ledger (board)
      (let ((customer (customer-by-id board (trim-text customer-id))))
        (unless customer
          (fail "no customer ~a in the ledger" (trim-text customer-id)))
        (setf (getf customer :reply-kind) kind
              (getf customer :note) (nlk:clip (trim-text (or text "")) 500))
        (cond
          ((string= kind "dispute")
           (setf (getf customer :on-hold) 1
                 (getf customer :hold-reason) "customer disputes the invoice"))
          ((string= kind "promise")
           (let ((at (parse-date (trim-text (or promised-date "")))))
             (unless at (fail "a promise needs its date, YYYY-MM-DD"))
             (setf (getf customer :promised-date) (iso-date at))))
          ((string= kind "paid")
           (setf (getf customer :promised-date) "")))
        (format nil "~a: recorded ~a~@[; ~a~]~@[; no reminder before ~a~]"
                (getf customer :name) kind (getf customer :note)
                (and (string= kind "promise")
                     (let ((at (parse-date (getf customer :promised-date))))
                       (and at (iso-date (+ at 2))))))))))

(defun hold (customer-id &key reason)
  "Put a customer on hold: no draft for them is approved or sent."
  (with-ledger (board)
    (let ((customer (customer-by-id board (trim-text customer-id))))
      (unless customer (fail "no customer ~a in the ledger" (trim-text customer-id)))
      (setf (getf customer :on-hold) 1
            (getf customer :hold-reason)
            (let ((given (trim-text (or reason ""))))
              (if (plusp (length given)) given "on hold")))
      (format nil "~a is on hold: ~a (/cashdesk release ~a lifts it)"
              (getf customer :name) (getf customer :hold-reason)
              (getf customer :id)))))

(defun release (customer-id)
  "Lift a hold, and a dispute with it."
  (with-ledger (board)
    (let ((customer (customer-by-id board (trim-text customer-id))))
      (unless customer (fail "no customer ~a in the ledger" (trim-text customer-id)))
      (setf (getf customer :on-hold) 0
            (getf customer :hold-reason) ""
            (getf customer :promised-date) ""
            (getf customer :reply-kind) "")
      (format nil "~a is off hold" (getf customer :name)))))

(defun ledger ()
  "The ledger as text: customers and invoices, the way a person reads it."
  (let ((board (read-ledger)))
    (if (null (getf board :customers))
        "the ledger is empty: cashdesk_import reads customers, invoices and a bank statement (kind customers, invoices or bank)"
        (format nil "~d customer~:p, ~d invoice~:p, ~d payment~:p~%~{~a~^~%~}"
                (length (getf board :customers))
                (length (getf board :invoices))
                (length (getf board :payments))
                (loop for invoice in (getf board :invoices)
                      collect (format nil "~a · ~a · due ~a · ~a · ~a · ~a"
                                      (getf invoice :number)
                                      (customer-name board (getf invoice :customer-id))
                                      (getf invoice :due-date)
                                      (format-cents (getf invoice :balance)
                                                    (getf invoice :currency))
                                      (getf invoice :status)
                                      (getf invoice :reference)))))))

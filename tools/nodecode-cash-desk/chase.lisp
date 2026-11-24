;;;; chase.lisp --- the reminder ladder, its figures, and the outbox.
;;;;
;;;; The ledger owns the numbers; the words are rendered from it. A draft is
;;;; built once, at 7, 14, 21 or 28 days overdue (the highest stage the
;;;; oldest invoice reached), one per customer, in their language. Every
;;;; figure the draft prints is recorded beside it, and a revision passes
;;;; Figure Lock -- each figure exactly as the ledger has it, nothing added --
;;;; or it is refused with the diff. A person approves the hash of the exact
;;;; text; only then does SEND-DRAFT write an .eml file into the outbox.

(in-package #:nodecode-cash-desk)

;;; --- the ladder ------------------------------------------------------------------

(defparameter +stages+
  '((7 . "stage07") (14 . "stage14") (21 . "stage21") (28 . "stage28"))
  "The reminder ladder: days overdue, and the stage that names that letter.")

(defparameter +stage-rank+
  '(("stage07" . 1) ("stage14" . 2) ("stage21" . 3) ("stage28" . 4) ("escalation" . 5))
  "How far along a draft is; a higher rank means a stronger letter.")

(defun stage-rank (stage)
  (or (cdr (assoc stage +stage-rank+ :test #'string=)) 0))

(defun stage-for (days)
  "The highest reminder stage DAYS overdue has reached, or NIL below seven."
  (loop for (threshold . stage) in +stages+
        when (>= days threshold) maximize threshold into found
        finally (return (and found (cdr (assoc found +stages+))))))

(defun stage-days (stage)
  (car (rassoc stage +stages+ :test #'string=)))

;;; --- canonical figures ------------------------------------------------------------

(defun canonical-id (text)
  "TEXT as letters and digits, uppercased: INV-2026/0042 => INV20260042."
  (string-upcase (remove-if-not #'alphanumericp (or text ""))))

(defun figure-money (cents currency)
  (format nil "money:~a:~d" (string-upcase (or currency "")) cents))

(defun figure-date (iso)
  (format nil "date:~a" iso))

(defun figure-invoice (number)
  (format nil "invoice:~a" (canonical-id number)))

(defun figure-reference (reference)
  (format nil "reference:~a" (canonical-id reference)))

(defun figure-iban (iban)
  (format nil "iban:~a" (canonical-id iban)))

(defun group-iban (iban)
  "IBAN with a space every four characters, as a person reads it."
  (let ((clean (canonical-id iban)))
    (join-text " " (loop for start from 0 below (length clean) by 4
                          collect (subseq clean start (min (length clean) (+ start 4)))))))

(defun ledger-figures (invoices)
  "Every figure the ledger holds for INVOICES: what a draft may quote."
  (loop for invoice in invoices
        append (append
                (list (figure-invoice (getf invoice :number))
                      (figure-money (getf invoice :amount) (getf invoice :currency))
                      (figure-money (getf invoice :balance) (getf invoice :currency))
                      (figure-date (getf invoice :due-date))
                      (figure-date (getf invoice :issue-date)))
                (let ((reference (getf invoice :reference))
                      (iban (getf invoice :iban))
                      (paid (getf invoice :paid-date)))
                  (append (and (plusp (length reference)) (list (figure-reference reference)))
                          (and (plusp (length iban)) (list (figure-iban iban)))
                          (and (plusp (length paid)) (list (figure-date paid))))))))

;;; --- rendering --------------------------------------------------------------------

(defun total-text (invoices lang)
  "INVOICES' balances per currency, joined: never summed across currencies."
  (let ((currencies '()))
    (dolist (invoice invoices)
      (let ((currency (getf invoice :currency)))
        (unless (assoc currency currencies :test #'string=)
          (push (cons currency 0) currencies))))
    (when currencies
      (join-text " + "
                 (loop for (currency . nil) in (sort currencies #'string< :key #'car)
                       collect (format-cents (loop for invoice in invoices
                                                   when (string= currency (getf invoice :currency))
                                                     sum (getf invoice :balance))
                                             currency :lang lang))))))

(defun days-overdue (invoice as-of)
  "How many days past its due date INVOICE is at AS-OF (negative: not due)."
  (- as-of (or (parse-date (getf invoice :due-date)) as-of)))

(defun invoice-line (invoice as-of lang)
  "One invoice as a reminder lists it, and the figures it printed."
  (let* ((days (max 0 (days-overdue invoice as-of)))
         (part (and (< (getf invoice :balance) (getf invoice :amount))
                    (plusp (getf invoice :balance))))
         (money (format-cents (getf invoice :balance) (getf invoice :currency) :lang lang))
         (line
           (if (string= lang "cs")
               (format nil "  - ~a, splatnost ~a: ~a (~d ~a~@[, částečně uhrazeno~])"
                       (getf invoice :number) (getf invoice :due-date) money days
                       (if (= days 1) "den" "dní") part)
               (format nil "  - ~a due ~a: ~a (~d day~:p overdue~@[, not fully paid~])"
                       (getf invoice :number) (getf invoice :due-date) money days part))))
    (values line
            (list (figure-invoice (getf invoice :number))
                  (figure-money (getf invoice :balance) (getf invoice :currency))
                  (figure-date (getf invoice :due-date))))))

(defun customer-greeting (customer lang)
  (let* ((contact (getf customer :contact-name))
         (first (and contact (plusp (length (trim-text contact)))
                     (first (split-words contact)))))
    (cond ((and first (string= lang "cs")) (format nil "Dobrý den, ~a," first))
          ((string= lang "cs") "Dobrý den,")
          (first (format nil "Hi ~a," first))
          (t "Hello,"))))

(defun stage-ask (stage lang)
  (cond
    ((string= stage "stage07")
     (if (string= lang "cs")
         "Pokud už je platba na cestě, berte prosím tento dopis na vědomí; jinak zkontrolujte, prosím, zda byla odeslána."
         "If payment is already on its way, please ignore this note; otherwise we would be grateful if you could check whether it has been sent."))
    ((string= stage "stage14")
     (if (string= lang "cs")
         "Platbu jsme dosud neobdrželi. Zkontrolujte ji prosím s vaším účetním oddělením a dejte nám vědět, kdy ji plánujete."
         "We have not yet received payment. Could you check with your accounts team and let us know when it is scheduled?"))
    ((string= stage "stage21")
     (if (string= lang "cs")
         "Prosím uhraďte fakturu tento týden, nebo nám dejte vědět, co platbu zdržuje."
         "Please arrange payment this week, or tell us if anything is holding it up."))
    (t
     (if (string= lang "cs")
         "Faktura je nyní naléhavá. Bez platby ji předáme našemu account managerovi k vymáhání."
         "This invoice is now urgent. Without payment it will be passed to our account manager for collection."))))

(defun bank-line (invoices lang)
  "The bank details line, and the figures it printed."
  (let* ((default-iban (setting :payment-iban))
         (pairs '())
         (figures '()))
    (dolist (invoice invoices)
      (let* ((iban (let ((own (getf invoice :iban)))
                     (if (plusp (length own)) own default-iban)))
             (currency (getf invoice :currency)))
        (when (plusp (length iban))
          (pushnew (cons currency iban) pairs :test (lambda (a b) (and (string= (car a) (car b))
                                                                        (string= (cdr a) (cdr b)))))
          (pushnew (figure-iban iban) figures :test #'string=))))
    (when pairs
      (setf pairs (nreverse pairs))
      (values
       (let ((printed (mapcar (lambda (pair)
                                (format nil "IBAN ~a (~a)" (group-iban (cdr pair)) (car pair)))
                              pairs)))
         (if (string= lang "cs")
             (format nil "Bankovní spojení: ~{~a~^; ~}. Do poznámky uveďte prosím variabilní symbol."
                     printed)
             (format nil "Bank details: ~{~a~^; ~}. Please quote the payment reference with your transfer."
                     printed)))
       figures))))

(defun render-draft (board customer invoices kind sender as-of lang)
  "KIND's letter to CUSTOMER about INVOICES. => (VALUES SUBJECT BODY FIGURES)."
  (declare (ignore board))
  (let* ((days (loop for invoice in invoices maximize (max 0 (days-overdue invoice as-of))))
         (total (total-text invoices lang))
         (lines (loop for invoice in invoices
                      collect (multiple-value-bind (line figures) (invoice-line invoice as-of lang)
                                (declare (ignore figures))
                                line)))
         (required (loop for invoice in invoices
                         append (multiple-value-bind (line figures) (invoice-line invoice as-of lang)
                                  (declare (ignore line))
                                  figures)))
         (history (and (some (lambda (invoice)
                                (plusp (stage-rank
                                        (let ((entry (find (getf invoice :number) (getf (read-ledger) :chase)
                                                           :key (lambda (c) (getf c :invoice-number))
                                                           :test #'string=)))
                                          (and entry (getf entry :stage))))))
                              invoices)
                       (if (string= lang "cs")
                           "Tento dopis navazuje na naše dřívější upomínky k těmto fakturám."
                           "This follows our earlier reminder(s) about these invoices.")))
         (subject
           (cond
             ((string= kind "thanks")
              (if (string= lang "cs")
                  (format nil "Děkujeme za platbu (~a)" total)
                  (format nil "Thank you for your payment (~a)" total)))
             ((string= kind "escalation")
              (format nil "Escalation: ~a, ~d days overdue" (getf (first invoices) :number) days))
             ((= 1 (length invoices))
              (if (string= lang "cs")
                  (format nil "Faktura ~a je ~d dní po splatnosti (~a)" (getf (first invoices) :number) days total)
                  (format nil "Invoice ~a is ~d days overdue (~a)" (getf (first invoices) :number) days total)))
             (t
              (if (string= lang "cs")
                  (format nil "~d faktury po splatnosti, nejstarší ~d dní (~a)" (length invoices) days total)
                  (format nil "~d invoices overdue, the oldest by ~d days (~a)" (length invoices) days total)))))
         (body
           (cond
             ((string= kind "escalation")
              (format nil "Internal note: ~a is ~d days overdue.~%~{~a~^~%~}~%Stage 07, 14, 21 and 28 have already been drafted; no further reminder is drafted until a person decides."
                      (getf (first invoices) :number) days lines))
             ((string= kind "thanks")
              (format nil "~a~%~%Thank you: we have received ~a for invoice ~a on ~a.~%~%Kind regards,~%~a"
                      (customer-greeting customer lang)
                      (format-cents (getf (first invoices) :balance) (getf (first invoices) :currency) :lang lang)
                      (getf (first invoices) :number) as-of sender))
             (t
              (multiple-value-bind (bank bank-figures) (bank-line invoices lang)
                (setf required (append required bank-figures))
                (format nil "~a~%~@[~a~%~]~%~a~%~{~a~^~%~}~%~@[~%~a~]~%~%~a~%~%~a~%~a"
                        (customer-greeting customer lang)
                        history
                        (if (string= lang "cs")
                            (format nil "Naše evidence ukazuje, že ~:[tyto faktury jsou~;faktura ~a je~] po splatnosti."
                                    (= 1 (length invoices)) (getf (first invoices) :number))
                            (format nil "Our records show that ~:[these invoices are~;invoice ~a is~] overdue."
                                    (= 1 (length invoices)) (getf (first invoices) :number)))
                        lines bank (stage-ask kind lang) "Kind regards," sender))))))
    (when (string= kind "thanks")
      (setf required (list (figure-invoice (getf (first invoices) :number))
                            (figure-money (getf (first invoices) :balance)
                                          (getf (first invoices) :currency))
                            (figure-date (iso-date as-of)))))
    (values subject body (remove-duplicates required :test #'string=))))

;;; --- planning today's drafts --------------------------------------------------------

(defun as-of-days (as-of)
  "AS-OF as a day number; today when it is blank."
  (let ((text (trim-text (or as-of ""))))
    (if (zerop (length text))
        (days-today)
        (or (parse-date text :order "auto")
            (fail "~s is not a date" text)))))

(defun ignore-below ()
  "The smallest balance that is chased, in cents."
  (or (ignore-errors (parse-amount (setting :ignore-balances-below) :what "ignore_balances_below"))
      1000))

(defun review-holds (board)
  "Every invoice number a receipt in review may pay."
  (loop for review in (getf board :reviews) append (getf review :holds)))

(defun chase-stage-of (board number)
  "The stage last drafted for invoice NUMBER, or NIL."
  (let ((entry (find number (getf board :chase)
                     :key (lambda (c) (getf c :invoice-number)) :test #'string=)))
    (and entry (getf entry :stage))))

(defun chase-date-of (board number)
  (let ((entry (find number (getf board :chase)
                     :key (lambda (c) (getf c :invoice-number)) :test #'string=)))
    (and entry (getf entry :at))))

(defun customer-held (board customer as-of)
  "Why CUSTOMER gets no reminder at AS-OF, or NIL. => (VALUES REASON)."
  (let ((promised (getf customer :promised-date)))
    (cond
      ((plusp (getf customer :on-hold)) (format nil "on hold: ~a" (getf customer :hold-reason)))
      ((string= "dispute" (getf customer :reply-kind)) "in dispute")
      ((and (plusp (length promised))
            (let ((at (parse-date promised)))
              (and at (<= as-of (+ at 2))))) "a payment was promised")
      ((zerop (length (trim-text (getf customer :email)))) "no email address")
      (t (let ((disputed (find-if (lambda (invoice)
                                    (and (string= "disputed" (getf invoice :status))
                                         (plusp (getf invoice :balance))))
                                  (customer-invoices board (getf customer :id)))))
           (and disputed (format nil "invoice ~a is disputed" (getf disputed :number))))))))

(defun chaseable-invoices (board customer as-of)
  "CUSTOMER's invoices this ladder may chase at AS-OF."
  (let ((holds (review-holds board))
        (floor (ignore-below)))
    (remove-if-not
     (lambda (invoice)
       (and (string= "open" (getf invoice :status))
            (plusp (getf invoice :balance))
            (>= (getf invoice :balance) floor)
            (>= (days-overdue invoice as-of) 1)
            (not (member (getf invoice :number) holds :test #'string=))))
     (customer-invoices board (getf customer :id)))))

(defun store-draft (board customer invoices kind subject body figures recipients lang sender as-of)
  "A new pending DRAFT on BOARD. => the draft."
  (let* ((id (format nil "D~d" (getf board :next-draft 1)))
         (draft (list :id id :kind kind :customer-id (getf customer :id)
                      :invoice-numbers (mapcar (lambda (invoice) (getf invoice :number)) invoices)
                      :language lang :subject subject :body body :recipients recipients
                      :status "pending" :hash (draft-hash subject body) :figures figures
                      :as-of (iso-date as-of) :created (nlk:iso-time (nlk:unix-now) :millis nil)
                      :sender sender)))
    (setf (getf board :next-draft) (1+ (getf board :next-draft 1))
          (getf board :drafts) (append (getf board :drafts) (list draft)))
    draft))

(defun draft-hash (subject body)
  "The hash a person approves: over the exact subject and body."
  (subseq (nlk:sha256-text (format nil "~a~%~a" subject body)) 0 16))

(defun record-stage (board invoice stage as-of)
  "Record that STAGE was drafted for INVOICE at AS-OF."
  (let ((entry (find (getf invoice :number) (getf board :chase)
                     :key (lambda (c) (getf c :invoice-number)) :test #'string=)))
    (cond (entry (setf (getf entry :stage) stage (getf entry :at) (iso-date as-of)))
          (t (setf (getf board :chase)
                   (append (getf board :chase)
                           (list (list :invoice-number (getf invoice :number)
                                       :stage stage :at (iso-date as-of)))))))))

(defun draft-reminders (&key sender as-of)
  "Plan today's follow-ups and store them as pending drafts: reminders at 7,
14, 21 and 28 days overdue, thank-you notes for payments received, and one
internal escalation note a week after an unanswered 28-day reminder. Nothing
is sent; every draft waits for a person's approval."
  (let ((as-of (as-of-days as-of))
        (sender (let ((given (trim-text (or sender ""))))
                  (if (plusp (length given)) given (trim-text (setting :sender-name))))))
    (when (zerop (length sender))
      (fail "no sender: set cash-desk.sender_name (the business that signs the letters) or pass sender"))
    (with-ledger (board)
      (let ((made '()) (skipped '()))
        ;; reminders, one per customer, at the highest stage reached
        (dolist (customer (getf board :customers))
          (let* ((invoices (chaseable-invoices board customer as-of))
                 (held (customer-held board customer as-of))
                 (days (loop for invoice in invoices maximize (days-overdue invoice as-of)))
                 (stage (and invoices (stage-for days))))
            (cond
              ((null invoices))
              (held (push (format nil "~a: ~a" (getf customer :name) held) skipped))
              ((null stage))
              (t
               (let* ((rank (stage-rank stage))
                      (drafted (or (loop for invoice in invoices
                                         maximize (stage-rank (chase-stage-of board (getf invoice :number))))
                                    0)))
                 (cond
                   ((> rank drafted)
                    (let ((lang (if (member (getf customer :language) '("en" "cs") :test #'string=)
                                    (getf customer :language) (setting :default-language))))
                      (multiple-value-bind (subject body figures)
                          (render-draft board customer invoices stage sender as-of lang)
                        (let ((draft (store-draft board customer invoices stage subject body figures
                                                  (list (getf customer :email)) lang sender as-of)))
                          (dolist (invoice invoices) (record-stage board invoice stage as-of))
                          (push (format nil "~a ~a for ~a (~a)" (getf draft :id) stage
                                         (getf customer :name) (getf draft :hash))
                                made)))))
                   ;; an unanswered 28-day reminder, a week on
                   ((and (> days 30)
                         (= drafted (stage-rank "stage28"))
                         (let ((oldest (find-if (lambda (invoice)
                                                  (= (days-overdue invoice as-of) days))
                                                invoices)))
                           (let ((at (and oldest (chase-date-of board (getf oldest :number)))))
                             (and at (let ((then (parse-date at)))
                                       (and then (>= (- as-of then) 7)))))))
                    (let* ((oldest (find-if (lambda (invoice) (= (days-overdue invoice as-of) days)) invoices))
                           (lang (if (member (getf customer :language) '("en" "cs") :test #'string=)
                                     (getf customer :language) (setting :default-language))))
                      (multiple-value-bind (subject body figures)
                          (render-draft board customer (list oldest) "escalation" sender as-of lang)
                        (let ((draft (store-draft board customer (list oldest) "escalation" subject body
                                                  figures '() lang sender as-of)))
                          (setf (getf (first (last (getf board :chase))) :stage) "escalation")
                          (push (format nil "~a escalation for ~a" (getf draft :id) (getf customer :name))
                                made)))))
                   (t (push (format nil "~a: already drafted at ~a" (getf customer :name) stage)
                            skipped))))))))
        ;; thank-you notes for payments applied and not yet thanked
        (dolist (match (getf board :matches))
          (when (zerop (getf match :thanked))
            (let* ((invoice (invoice-by-number board (getf match :invoice-number)))
                   (customer (and invoice (customer-by-id board (getf invoice :customer-id)))))
              (when (and invoice customer
                         (plusp (length (trim-text (getf customer :email))))
                         (not (customer-held board customer as-of)))
                (let ((lang (if (member (getf customer :language) '("en" "cs") :test #'string=)
                                (getf customer :language) (setting :default-language))))
                  (multiple-value-bind (subject body figures)
                      (render-draft board customer (list invoice) "thanks" sender as-of lang)
                    (let ((draft (store-draft board customer (list invoice) "thanks" subject body figures
                                              (list (getf customer :email)) lang sender as-of)))
                      (setf (getf match :thanked) 1)
                      (push (format nil "~a thanks to ~a" (getf draft :id)
                                     (getf customer :name))
                            made))))))))
        (let ((pending (count-if (lambda (draft) (string= "pending" (getf draft :status)))
                                 (getf board :drafts))))
          (format nil "drafted ~d (~d pending in all)~@[; skipped: ~{~a~^, ~}~]~@[~%~{~a~^~%~}~]"
                  (length made) pending
                  (and skipped (subseq skipped 0 (min 8 (length skipped))))
                  (nreverse made)))))))

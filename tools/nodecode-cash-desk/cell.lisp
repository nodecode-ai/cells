;;;; cell.lisp --- the Cash Desk cell: the tools, the command, the section.
;;;;
;;;; The ledger and its words live in the other files; this one declares what
;;;; the model and the person reach. Nothing here is sent by the model alone:
;;;; APPROVE and SEND-DRAFT are the person's, and a draft's hash is what they
;;;; approve.

(in-package #:nodecode-cash-desk)

(defparameter +primer+
  "Cash Desk chases overdue invoices from CSV exports: the model writes the
words, the ledger owns the numbers. The verbs answer a string; a refusal is
ERROR: CASH-DESK-ERROR with the reason.
  (cashdesk:import-file \"customers\" \"customers.csv\")
  (cashdesk:import-file \"invoices\" \"invoices.csv\")
  (cashdesk:import-file \"bank\" \"bank-day1.csv\" :default-currency \"EUR\")
      kind is customers, invoices or bank; the same file is never imported
      twice; paths are inside the ledger folder or cash-desk.import_dirs.
  (cashdesk:match-payments)   apply the payments that are certain, and list
      the rest with the invoices they pause
  (cashdesk:status)           open balance by age bucket and currency
  (cashdesk:draft-reminders)  one reminder per customer at 7, 14, 21 or 28
      days overdue, in their language; nothing is sent
  (cashdesk:queue)            the drafts, with the exact text and its hash
  (cashdesk:revise \"D1\" \"…\")  reword a draft under Figure Lock
  (cashdesk:approve \"D1\")     a person approves that exact text
  (cashdesk:send-draft \"D1\" \"hash\")  write it to the outbox as an .eml
  (cashdesk:record-reply \"C001\" \"we dispute it\" :kind \"dispute\")
The same ten things are the tools cashdesk_status, cashdesk_brief,
cashdesk_import, cashdesk_match, cashdesk_queue, cashdesk_draft,
cashdesk_revise, cashdesk_reply, cashdesk_hold and cashdesk_send. Figure
Lock keeps every amount, invoice number, due date, reference and IBAN exactly
as the ledger has it; a revision that changes one is refused with the diff."
  "What (help :cash-desk) answers while the cell runs.")

;;; --- contracts ---------------------------------------------------------------------

(nlk:define-contract status-query
  (:doc "Read the Cash Desk ledger: open balance by age bucket and currency, drafts waiting, customers on hold, last import. Read-only.")
  ("as_of" :string :default "" :doc "ISO date to treat as today; default today"))

(nlk:define-contract brief-query
  (:doc "The morning cash brief: payments received, newly overdue invoices, drafts awaiting approval, customers on hold. Read-only; it sends nothing.")
  ("lang" :string :default "en" :options '("en" "cs") :doc "language of the brief")
  ("as_of" :string :default "" :doc "ISO date to treat as today; default today"))

(nlk:define-contract import-request
  (:doc "Import one CSV export into the ledger, once. kind=customers reads customer_id, name, email, language; kind=invoices reads a generic, Xero, QuickBooks or Stripe invoice export; kind=bank reads a statement. Reads only the file you pass, inside the ledger folder or import_dirs.")
  ("kind" :text :required t :options '("customers" "invoices" "bank")
   :doc "which export this is")
  ("path" :text :required t :doc "the CSV file to read")
  ("source" :string :default "generic" :doc "invoice export format: generic, xero, quickbooks or stripe")
  ("default_currency" :string :default "" :doc "currency for rows without one; a bank statement with no currency column needs it")
  ("date_order" :string :default "auto" :doc "how slashed dates read: auto, dmy or mdy")
  ("allow_errors" :boolean :default nil :doc "import the good rows even when some rows are refused"))

(nlk:define-contract match-request
  (:doc "Match bank payments to open invoices. A payment is applied only when its reference names exactly one invoice, in that invoice's currency, for exactly its open balance; everything else goes to review with the invoices it pauses. Nothing is sent.")
  ("as_of" :string :default "" :doc "ISO date to treat as today; default today"))

(nlk:define-contract queue-query
  (:doc "List drafts with their exact subject, body, recipients and the hash a person approves.")
  ("status" :string :default "pending" :doc "pending, approved, sent or all")
  ("customer_id" :string :default "" :doc "only this customer's drafts"))

(nlk:define-contract draft-request
  (:doc "Plan today's follow-ups and store them as pending drafts: reminders at 7, 14, 21 and 28 days overdue in the customer's language, thank-you notes for payments received, and an internal escalation note. Every draft waits for a person's approval; nothing is sent.")
  ("sender" :string :default "" :doc "the business that signs; default the sender_name setting")
  ("as_of" :string :default "" :doc "the date to plan as of; default today"))

(nlk:define-contract revise-request
  (:doc "Reword a draft's prose under Figure Lock. Every amount, invoice number, due date, IBAN and reference must stay exactly as the ledger has it, with nothing added; a financial line may not change. A rejected revision returns the diff and leaves the draft untouched; an accepted one needs a fresh approval.")
  ("draft_id" :text :required t :doc "the draft's id, as cashdesk_queue shows it")
  ("new_text" :text :required t :doc "the complete new body")
  ("new_subject" :string :default "" :doc "optional new subject line"))

(nlk:define-contract reply-request
  (:doc "Record a customer's reply as dispute, paid, promise or other. A dispute holds the customer; a promise with a date pauses reminders until two days after it; paid changes nothing until the bank statement shows the money.")
  ("customer_id" :text :required t :doc "the customer who replied")
  ("kind" :text :required t :options '("dispute" "paid" "promise" "other")
   :doc "dispute contests the invoice; paid says they paid; promise is a date; other is anything else")
  ("text" :string :default "" :doc "what they said, in one or two sentences")
  ("promised_date" :string :default "" :doc "kind=promise: the date they promised, YYYY-MM-DD"))

(nlk:define-contract hold-request
  (:doc "Put a customer on hold: their drafts are not approved or sent until a person releases them (/cashdesk release).")
  ("customer_id" :text :required t :doc "the customer to hold")
  ("reason" :string :default "" :doc "why, in a few words"))

(nlk:define-contract send-request
  (:doc "Write one approved draft to the outbox as an .eml file per recipient. Refused unless a person approved exactly this text: pass the hash cashdesk_queue shows.")
  ("draft_id" :text :required t :doc "the approved draft's id")
  ("draft_hash" :text :required t :doc "the hash of the approved text"))

;;; --- the tools ---------------------------------------------------------------------

(defun tool-status (query)
  (status :as-of (status-query-as-of query)))

(defun tool-brief (query)
  (brief :lang (brief-query-lang query) :as-of (brief-query-as-of query)))

(defun tool-import (request)
  (import-file (import-request-kind request)
               (import-request-path request)
               :source (import-request-source request)
               :default-currency (let ((given (import-request-default-currency request)))
                                   (and (plusp (length (trim-text given))) given))
               :date-order (import-request-date-order request)
               :allow-errors (import-request-allow-errors request)))

(defun tool-match (query)
  (match-payments :as-of (match-request-as-of query)))

(defun tool-queue (query)
  (queue :status (queue-query-status query)
         :customer-id (queue-query-customer-id query)))

(defun tool-draft (request)
  (draft-reminders :sender (draft-request-sender request)
                    :as-of (draft-request-as-of request)))

(defun tool-revise (request)
  (revise (revise-request-draft-id request)
          (revise-request-new-text request)
          :new-subject (revise-request-new-subject request)))

(defun tool-reply (request)
  (record-reply (reply-request-customer-id request)
                (reply-request-text request)
                :kind (reply-request-kind request)
                :promised-date (reply-request-promised-date request)))

(defun tool-hold (request)
  (hold (hold-request-customer-id request) :reason (hold-request-reason request)))

(defun tool-send (request)
  (send-draft (send-request-draft-id request) (send-request-draft-hash request)))

;;; --- /cashdesk ----------------------------------------------------------------------

(defun panel ()
  "The dialog /cashdesk answers: approve a draft, send an approved one, or
release a held customer."
  (let* ((board (read-ledger))
         (drafts (remove-if (lambda (draft) (string= "sent" (getf draft :status)))
                            (getf board :drafts)))
         (held (remove-if-not (lambda (customer) (plusp (getf customer :on-hold)))
                              (getf board :customers))))
    (nle:list-dialog
     :title "Cash Desk" :summary (format nil "~d drafts, ~d on hold" (length drafts) (length held))
     :rows (append
            (loop for draft in drafts
                  collect (if (string= "approved" (getf draft :status))
                              (nle:run-row
                               (format nil "Send ~a to ~{~a~^, ~}" (getf draft :id)
                                       (getf draft :recipients))
                               (format nil "/cashdesk send ~a" (getf draft :id))
                               :detail (getf draft :subject))
                              (nle:run-row
                               (format nil "Approve ~a" (getf draft :id))
                               (format nil "/cashdesk approve ~a" (getf draft :id))
                               :detail (getf draft :subject))))
            (loop for customer in held
                  collect (nle:run-row
                           (format nil "Release ~a" (getf customer :name))
                           (format nil "/cashdesk release ~a" (getf customer :id))
                           :detail (getf customer :hold-reason)))))))

(defun run-slash (args session-id)
  "/cashdesk: the ledger, its drafts, and the person's decisions. approve ID,
send ID, release ID, hold ID [reason], drafts, brief."
  (declare (ignore session-id))
  (let* ((text (nlk:trimmed (or args "")))
         (space (position-if (lambda (char) (member char '(#\Space #\Tab))) text))
         (head (string-downcase (subseq text 0 space)))
         (rest (if space (nlk:trimmed (subseq text space)) "")))
    (cond
      ((string= head "") (values (status) (panel)))
      ((string= head "approve") (approve rest))
      ((string= head "send") (send-draft rest (or (getf (find rest (getf (read-ledger) :drafts)
                                                              :key (lambda (d) (getf d :id))
                                                              :test #'string=)
                                                        :hash)
                                                  "")))
      ((string= head "release") (release rest))
      ((string= head "hold")
       (let ((second (position-if (lambda (char) (member char '(#\Space #\Tab))) rest)))
         (hold (if second (nlk:trimmed (subseq rest 0 second)) rest)
               :reason (if second (nlk:trimmed (subseq rest second)) ""))))
      ((string= head "drafts") (queue))
      ((string= head "brief") (brief))
      (t (status)))))

;;; --- the declaration ---------------------------------------------------------------

(nle:define-cell cash-desk
  (:section ("cash-desk")
    (:guide "with no sender_name the drafting refuses; import_dirs is where cashdesk_import may read CSV files from; outbox is where approved .eml files are written; nothing is sent without a person's approval")
    ("sender_name" :string :default ""
     :doc "the business that signs every reminder; drafting refuses until it is set")
    ("from_address" :string :default ""
     :doc "the From header of the .eml files (empty leaves it out)")
    ("default_language" :choice :options '("en" "cs") :default "en"
     :doc "the language for customers whose export carries none")
    ("currency" :string :default "EUR"
     :doc "the currency for files without a currency column")
    ("payment_iban" :string :default ""
     :doc "printed in reminders for invoices whose export carries no IBAN")
    ("date_order" :choice :options '("auto" "dmy" "mdy") :default "auto"
     :doc "how slashed dates read; auto refuses a file whose dates read both ways")
    ("ignore_balances_below" :string :default "10.00"
     :doc "an open balance under this is not chased automatically")
    ("import_dirs" :list
     :doc "folders cashdesk_import may read CSV exports from, beside its own ledger folder")
    ("outbox" :string :default ""
     :doc "where approved reminders are written as .eml files; empty means the ledger's own outbox folder"))
  (:tool "cashdesk_status" (nlk:contract 'status-query) 'tool-status)
  (:tool "cashdesk_brief" (nlk:contract 'brief-query) 'tool-brief)
  (:tool "cashdesk_import" (nlk:contract 'import-request) 'tool-import)
  (:tool "cashdesk_match" (nlk:contract 'match-request) 'tool-match)
  (:tool "cashdesk_queue" (nlk:contract 'queue-query) 'tool-queue)
  (:tool "cashdesk_draft" (nlk:contract 'draft-request) 'tool-draft)
  (:tool "cashdesk_revise" (nlk:contract 'revise-request) 'tool-revise)
  (:tool "cashdesk_reply" (nlk:contract 'reply-request) 'tool-reply)
  (:tool "cashdesk_hold" (nlk:contract 'hold-request) 'tool-hold)
  (:tool "cashdesk_send" (nlk:contract 'send-request) 'tool-send)
  (:help :cash-desk "cash-desk: chase overdue invoices from CSV exports - match payments, draft reminders, approve and send; /cashdesk" +primer+)
  (:command "cashdesk" 'run-slash
            :catches 'cash-desk-error
            :description "The Cash Desk ledger: drafts, approvals, holds"
            :argument-hint "[approve ID | send ID | release ID | drafts | brief]"))

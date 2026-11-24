;;;; cell.lisp --- the manual, the three verbs, the tools, /invinoveritas-receipts.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A decision is real, and self-graded: what a guard refused, what a person
;;;; allowed, what a model signed off on leaves no record anyone outside this
;;;; machine can check. A receipt is one: invinoveritas signs a salted
;;;; commitment to the question, the options, the choice and the SHA-256 of
;;;; the context, and hands the salt back here alone. Anyone can check the
;;;; signature for free; only this machine can open the receipt to say what
;;;; the decision was about.
;;;;
;;;; Three verbs, three tools that call them, one command. None of them
;;;; approves, blocks, delays or changes anything: they record.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "invinoveritas-receipts": {"api_url": "https://api.babyblueviper.com",
;;;;                "api_key_env": "INVINOVERITAS_API_KEY",
;;;;                "timeout_seconds": 10, "listed": 10}
;;;; A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-invinoveritas-receipts)

;;; --- the manual ----------------------------------------------------------

(defparameter +primer+
  "Signed, salted receipts for decisions, through the nodecode-invinoveritas-receipts cell:
invinoveritas signs a commitment to a decision, this machine keeps the salt.
Three tools, and the same three verbs through eval.
  issue_receipt   (invinoveritas-receipts:issue \"approve\" \"what it was about\")
      asks invinoveritas for a receipt for one decision and keeps it. The
      question, the options, the choice and the SHA-256 of the context are
      sent; the context itself is not, and a 32-byte nonce makes its hash
      unguessable. Paid per call: 100 sats from the account the key names,
      so one receipt per decision, never per turn. Decided by a model? name
      it in decider.
  verify_receipt  (invinoveritas-receipts:verify \"<id or its first characters>\")
      asks the free, unauthenticated endpoint whether the receipt's signature
      holds. No key needed.
  check_receipt   (invinoveritas-receipts:check \"<id>\")
      opens a kept receipt on this machine, with no network: recomputes the
      context hash and both salted commitments from the salt. It answers what
      the decision was about, which the public receipt alone does not.
/invinoveritas-receipts lists what is kept, shows one, or verifies one. A
receipt attests that this decision was committed at that time: it does not
say the decision was right. ERROR: INVINOVERITAS-RECEIPTS-ERROR names what
went wrong, and its text says which setting to fix."
  "What (help :invinoveritas-receipts) answers while the cell runs.")

;;; --- words ---------------------------------------------------------------

(defun nonblank (text)
  "TEXT as a trimmed string, \"\" when NIL."
  (nlk:trimmed (or text "")))

(defun ensure-running (verb)
  "Refuse VERB when the cell is not started: the tools are gone with it."
  (unless (running-settings)
    (fail "the invinoveritas-receipts cell is not running: turn it on from /cells before you ~a" verb)))

(defun true-value (object key)
  "OBJECT's member KEY as a boolean: JSON true is T, false and null are not."
  (let ((value (and (hash-table-p object) (gethash key object))))
    (and value (not (eq value :null)) t)))

;;; --- the verbs ------------------------------------------------------------

(defun issue (choice context &key question options decider reference session)
  "Commit CHOICE over CONTEXT to a signed receipt and keep it. => a string.
The only things sent are the question, the options, the choice and the
SHA-256 of the context; the salt that opens it is kept here."
  (ensure-running "ask for a receipt")
  (let* ((choice (nonblank choice))
         (options (if (and options (plusp (length options)))
                      (coerce options 'vector)
                      (coerce +options+ 'vector)))
         (question (let ((given (nonblank question)))
                     (if (plusp (length given)) given +question+)))
         (decider (let ((given (nonblank decider)))
                    (if (plusp (length given)) given "human")))
         (reference (nonblank reference))
         (session (or session nlk:*scribe-session-id*)))
    (unless (plusp (length choice))
      (fail "a receipt needs a choice: one of ~{~a~^, ~}" (coerce options 'list)))
    (unless (find choice (coerce options 'list) :test #'string-equal)
      (fail "~s is no option of this receipt: it offers ~{~a~^, ~}"
            choice (coerce options 'list)))
    (unless (api-key)
      (fail "no invinoveritas key: put one in the environment variable ~a names (the receipts.api_key_env setting); registration is free at https://invinoveritas.dev and a receipt costs 100 sats"
            (setting :api-key-env)))
    (let* ((nonce (new-nonce))
           (hashed (context-object nonce (or context "") reference session))
           (sha (context-sha256 hashed))
           (answer (issue-request (jobject "question" question
                                            "options" options
                                            "choice" choice
                                            "context_sha256" sha
                                            "decider" (jobject "provider" "nodecode"
                                                                "model" decider
                                                                "request_id" reference))))
           (event (and (hash-table-p answer) (gethash "event" answer)))
           (receipt (and (hash-table-p answer) (gethash "receipt" answer)))
           (reveal (and (hash-table-p answer) (gethash "reveal" answer)))
           (salt (text-field reveal "salt"))
           (id (text-field event "id")))
      (remember (jobject "id" (or id "") "event" event "receipt" receipt
                         "salt" (or salt "") "choice" choice "decider" decider
                         "context" hashed "context_sha256" sha "question" question
                         "issued_at" (nlk:iso-now) "api_url" (base-url)
                         "verify" (verify-hint)))
      (format nil "receipt ~a: ~s~@[, decided by ~a~]. Kept in ~a; ~a. The context itself never left this machine, and (invinoveritas-receipts:check \"~a\") opens it here."
              (if id id "(the service named no id)") choice
              (unless (string-equal decider "human") decider)
              (store-path) (verify-hint) (or id "")))))

(defun verify (&optional id)
  "Ask the free endpoint whether a kept receipt's signature holds. => a string.
Reads the event from the local ledger, so ID is the receipt's id here."
  (ensure-running "verify a receipt")
  (let* ((record (kept-record id))
         (event (and (hash-table-p record) (gethash "event" record)))
         (receipt-id (or (text-field record "id") "(no id)")))
    (unless (hash-table-p event)
      (fail "the kept receipt ~a holds no event to verify" receipt-id))
    (let ((answer (verify-request event)))
      (format nil "receipt ~a: ~a~@[ (~a)~]"
              receipt-id
              (if (true-value answer "valid") "signature valid" "signature NOT valid")
              (nonblank (text-field answer "reason"))))))

(defun check (&optional id)
  "Open a kept receipt on this machine: no network, no key. => a string.
Recomputes the context hash and both salted commitments from the salt."
  (ensure-running "open a receipt")
  (let* ((record (kept-record id))
         (receipt-id (or (text-field record "id") "(no id)"))
         (salt (text-field record "salt"))
         (choice (nonblank (text-field record "choice")))
         (hashed (and (hash-table-p record) (gethash "context" record)))
         (sha (nonblank (text-field record "context_sha256")))
         (receipt (and (hash-table-p record) (gethash "receipt" record))))
    (unless (plusp (length salt))
      (fail "the kept receipt ~a holds no salt: only the machine that asked for it can open it" receipt-id))
    (unless (hash-table-p hashed)
      (fail "the kept receipt ~a holds no context to open" receipt-id))
    (flet ((holding (label held wanted)
             (cond ((zerop (length held)) (format nil "~a: not held in the kept receipt" label))
                   ((string-equal held wanted) (format nil "~a: holds" label))
                   (t (format nil "~a: does NOT hold, it recomputes to ~a" label wanted)))))
      (format nil "receipt ~a opens as ~s: ~a; ~a; ~a.~%~a"
              receipt-id choice
              (holding "context hash" sha (context-sha256 hashed))
              (holding "choice commitment" (nonblank (text-field receipt "choice_commitment"))
                       (commitment salt choice))
              (holding "context commitment" (nonblank (text-field receipt "context_commitment"))
                       (commitment salt sha))
              (verify-hint)))))

;;; --- the command ---------------------------------------------------------

(defun list-kept ()
  "What /invinoveritas-receipts answers typed alone: the newest receipts, and where they are."
  (let ((records (newest-kept))
        (all (length (kept))))
    (if (null records)
        (format nil "no receipt is kept in ~a yet: ask for one with the issue_receipt tool, or (invinoveritas-receipts:issue \"approve\" \"what it was about\")"
                (store-path))
        (format nil "~d receipt~:p kept in ~a, ~d shown, newest first:~%~{  ~a~%~}/invinoveritas-receipts <id> shows one; /invinoveritas-receipts verify <id> checks its signature for free."
                all (store-path) (length records)
                (mapcar (lambda (record)
                          (format nil "~a  ~a  ~a~@[  by ~a~]"
                                  (or (text-field record "id") "(no id)")
                                  (or (text-field record "choice") "?")
                                  (or (text-field record "issued_at") "?")
                                  (text-field record "decider")))
                        records)))))

(defun detail (id)
  "What /invinoveritas-receipts <id> answers: one receipt, and how to open or verify it."
  (let* ((record (kept-record id))
         (receipt-id (or (text-field record "id") "(no id)")))
    (format nil "receipt ~a~%  chose: ~a~@[ by ~a~]~%  question: ~a~%  context sha256: ~a~%  issued: ~a~%  kept in: ~a~%(invinoveritas-receipts:check \"~a\") opens it here; /invinoveritas-receipts verify ~a checks its signature for free."
            receipt-id
            (or (text-field record "choice") "?")
            (text-field record "decider")
            (or (text-field record "question") "?")
            (or (text-field record "context_sha256") "?")
            (or (text-field record "issued_at") "?")
            (store-path)
            receipt-id receipt-id)))

(defun run-slash (args session-id)
  "One /invinoveritas-receipts: list what is kept, show one, or verify one."
  (declare (ignore session-id))
  (let* ((text (nonblank args))
         (lower (string-downcase text)))
    (cond ((zerop (length text)) (list-kept))
          ((uiop:string-prefix-p "verify " lower) (verify (nonblank (subseq text 7))))
          ((uiop:string-prefix-p "check " lower) (check (nonblank (subseq text 6))))
          (t (detail text)))))

;;; --- the tools -----------------------------------------------------------

(nlk:define-contract issue-receipt
  (:doc "Commit one decision to a signed, salted receipt at invinoveritas, and keep the salt that opens it. Send it when someone may later need to prove a choice was made and when: an approved dangerous command, a refusal, a release go-ahead. The question and the hashed context are sent, never the context itself, and the call is paid (100 sats from the account the key names), so one receipt per decision. It records; it never approves, blocks or changes anything.")
  ("choice" :text :required t
            :doc "what was decided, spelled as one of options does")
  ("context" :text :required t
             :doc "what the decision was about, in your own words: the command, the change, the question. It is hashed here with a random nonce and never sent, so write the detail the operator would want to read back later")
  ("question" :string :default ""
              :doc "the question the receipt answers, when the default one does not fit")
  ("options" :array :of :string
             :doc "the choices the question offered, at least two; approve and deny when empty")
  ("decider" :string :default "human"
             :doc "who or what decided: human, or the name of the model or guard that did")
  ("reference" :string :default ""
               :doc "the id the decision happened under, when one exists; it is hashed with the context, never sent"))

(nlk:define-contract verify-receipt
  (:doc "Check a kept receipt's signature at the free, unauthenticated invinoveritas endpoint. Needs no key and costs nothing. Use it when someone asks whether a receipt still holds.")
  ("receipt" :string :default "" :slot receipt-id
             :doc "the receipt's id, or its first characters; empty is the newest one kept here"))

(nlk:define-contract check-receipt
  (:doc "Open a kept receipt on this machine, with no network and no key: recompute the context hash and both salted commitments from the salt, and read back what the decision was about. Use it when the operator asks what a receipt says.")
  ("receipt" :string :default "" :slot receipt-id
             :doc "the receipt's id, or its first characters; empty is the newest one kept here"))

(defun issue-tool (request)
  "The issue_receipt tool: one receipt, or the reason there is none."
  (handler-case
      (issue (issue-receipt-choice request) (issue-receipt-context request)
             :question (issue-receipt-question request)
             :options (coerce (issue-receipt-options request) 'list)
             :decider (issue-receipt-decider request)
             :reference (issue-receipt-reference request))
    (invinoveritas-receipts-error (condition) (format nil "ERROR: INVINOVERITAS-RECEIPTS-ERROR: ~a" condition))))

(defun verify-tool (request)
  "The verify_receipt tool."
  (handler-case (verify (verify-receipt-receipt-id request))
    (invinoveritas-receipts-error (condition) (format nil "ERROR: INVINOVERITAS-RECEIPTS-ERROR: ~a" condition))))

(defun check-tool (request)
  "The check_receipt tool."
  (handler-case (check (check-receipt-receipt-id request))
    (invinoveritas-receipts-error (condition) (format nil "ERROR: INVINOVERITAS-RECEIPTS-ERROR: ~a" condition))))

;;; --- the cell ------------------------------------------------------------

(nle:define-cell invinoveritas-receipts
  (:section ("invinoveritas-receipts")
    (:guide "A key registers free at invinoveritas.dev and the environment variable api_key_env names it; a receipt costs 100 sats from that account, verification is free and needs no key. api_url is the service base; listed bounds how many /invinoveritas-receipts shows")
    ("api_url" :string :default "https://api.babyblueviper.com"
               :doc "the invinoveritas API base")
    ("api_key_env" :string :default "INVINOVERITAS_API_KEY"
                   :doc "the environment variable holding the key; the cell reads no other credential")
    ("timeout_seconds" :integer :default 10 :min 1
                       :doc "one request's ceiling")
    ("listed" :integer :default 10 :min 1
              :doc "how many kept receipts /invinoveritas-receipts shows"))
  (:tool "issue_receipt" (nlk:contract 'issue-receipt) 'issue-tool)
  (:tool "verify_receipt" (nlk:contract 'verify-receipt) 'verify-tool)
  (:tool "check_receipt" (nlk:contract 'check-receipt) 'check-tool)
  (:help :invinoveritas-receipts "invinoveritas-receipts: commit a decision to a signed receipt, verify it free, open it here" +primer+)
  (:command "invinoveritas-receipts" 'run-slash
            :catches 'invinoveritas-receipts-error
            :description "Receipts for decisions made here: list, show, verify"))

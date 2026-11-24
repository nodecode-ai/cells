;;;; cell.lisp --- the section, the tool, /frihet, the manual, the gate.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Config, a sibling top-level key next to `frihet':
;;;;   "frihet": {"api_key": "...", "api_key_env": "FRIHET_API_KEY",
;;;;               "mcp_url": "https://mcp.frihet.io/mcp",
;;;;               "timeout_seconds": 15, "ask": true}
;;;; The key is the one required member, pasted here or named by a variable --
;;;; the original read FRIHET_API_KEY from the environment -- and a section
;;;; naming no key refuses the start, the way the original's tools could not
;;;; call without one.

(in-package #:nodecode-frihet)

;;; --- the manual ---------------------------------------------------------------
;;; (help :frihet) answers it while the cell runs; a request carries the one
;;; line the :HELP clause below gives, never this text.

(defparameter *primer*
  "Frihet is a business-management platform -- invoices, expenses, clients, quotes,
products, webhooks, tax filings -- reached through its own MCP at
https://mcp.frihet.io/mcp. This cell is the operating layer over it: it holds no
Frihet operation of its own, it calls the endpoint's.
  (frihet:status)                    what the cell is set to, no network call
  (frihet:doctor)                    the handshake, the key, the tool list
  (frihet:operations)                every operation the endpoint advertises
  (frihet:call-operation \"list_invoices\" :arguments (nlk:json-object \"limit\" 5))
                                      one operation, answered as it answered
  The model calls the same through the frihet_mcp tool.
Doctrine, the original's operating contract:
 1. Read before write. Before creating, updating, sending, marking paid, deleting
    or filing anything, call the matching get/list operation to confirm the target
    exists and the arguments name it.
 2. Draft first. createInvoice, createQuote and createCreditNote leave a draft:
    build it, show the totals, and hand the decision back. Never auto-send from a
    loop. createPayment has no draft status and is irreversible.
 3. Honour Idempotency-Key. A retry after a blip reuses the key; a rerun for a
    new business reason takes a new one.
 4. Reconcile before retry on ambiguity. After idempotency_pending, unknown_status,
    a 5xx or a timeout, read the record by the key you used before retrying: two
    retries with the key are safe, one without checking is how double charges happen.
 5. Frihet is the authority for workspace, scopes, roles and resources. A record
    no get/list returns does not exist for you.
The gate: an operation whose name reaches a tax authority, moves money or deletes
state -- send, markPaid, issue, delete, revoke, registerVerifactu, applyCreditNote -
is irreversible, and one that matches nothing the cell knows is unclassified. Either
way the cell asks the operator, and the call runs only on Allow once, or when the
call itself carries \"confirm\": true after the operator agreed in words. Reads and
draft-writes pass without a question. A refusal is the call's failure: do not
disguise it as a retry, and do not work around it.
Secrets: the key lives in the section, never in a call's arguments and never in
what this cell prints."
  "What (help :frihet) answers while the cell runs.")

;;; --- the boundary: what a model call carries ---------------------------------------

(nlk:define-contract operation-request
  (:doc "Call one operation on the Frihet MCP -- its own invoices, expenses,
clients, quotes or tax filings. An irreversible operation (send, markPaid, issue,
delete, registerVerifactu) or one this cell cannot classify needs the operator's
agreement: \"confirm\": true once they gave it.")
  ("operation" :text :required t
   :doc "the Frihet MCP operation to call, e.g. list_invoices or get_invoice; (frihet:operations) lists them")
  ("arguments" :object
   :doc "the operation's own arguments as an object, e.g. {\"invoice_id\": \"...\"}; empty for a bare list")
  ("confirm" :boolean
   :doc "true once the operator agreed to an irreversible operation; the gate asks for it"))

;;; --- the command ----------------------------------------------------------------

(defparameter +subcommands+ '("status" "setup" "doctor" "operations"))

(defun run-command (args session-id)
  "/frihet [status|setup|doctor|operations]: the operator's own check, answered as
the same call answers it from eval."
  (declare (ignore session-id))
  (let* ((parts (uiop:split-string (string-trim '(#\Space #\Tab) (or args ""))
                                    :separator '(#\Space #\Tab)))
         (action (if parts (string-downcase (first parts)) "status"))
         (extra (rest parts)))
    (handler-case
        (cond
          (extra
           ;; A pasted key belongs in the section, never in a transcript: name
           ;; the count, never the words.
           (format nil "frihet: /frihet takes one word, not ~d more; use /frihet status, setup, doctor or operations. ~
A key goes in the frihet section's api_key, not on this line, so it cannot land in a chat log."
                   (length extra)))
          ((string= action "status") (status))
          ((string= action "setup") (setup))
          ((string= action "doctor") (doctor))
          ((string= action "operations") (operations))
          ((member action '("help" "--help" "-h") :test #'string=)
           (format nil "frihet: /frihet status | setup | doctor | operations~%~%~a" *primer*))
          (t
           (format nil "frihet: unknown subcommand; use /frihet status, setup, doctor or operations")))
      (frihet-error (condition)
        (format nil "ERROR: ~a" (redact (princ-to-string condition)))))))

;;; --- the declaration ---------------------------------------------------------------

(nle:define-cell frihet
  (:section ("frihet")
    (:guide "make an API key in your Frihet workspace, then paste it into api_key or name the variable holding it in api_key_env; mcp_url is the canonical endpoint unless you run a mirror; ask is whether an irreversible operation waits for you to say yes")
    ("api_key" :secret
               :doc "the Frihet API key, written here; left empty, the variable api_key_env names is read")
    ("api_key_env" :env :default "FRIHET_API_KEY"
                   :doc "the environment variable holding the key, read when api_key is empty")
    ("mcp_url" :string :default "https://mcp.frihet.io/mcp"
               :doc "the Frihet MCP endpoint; leave it unless you run a private mirror")
    ("timeout_seconds" :integer :default 15 :min 1
                       :doc "seconds one exchange with the endpoint may take")
    ("ask" :boolean :default t
           :doc "whether an irreversible operation asks you before it runs"))
  (:settings #'read-settings)
  (:tool "frihet_mcp" (nlk:contract 'operation-request) #'operation-tool)
  (:command "frihet" 'run-command
            :description "Frihet: status, setup, doctor, operations"
            :argument-hint "status|setup|doctor|operations")
  (:help :frihet
   "frihet: the Frihet MCP -- status, doctor, the operations it lists, and one call, with a gate over irreversible ones"
   *primer*)
  (:hook :tool #'gate))

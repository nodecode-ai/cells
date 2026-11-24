;;;; cell.lisp --- the section, the runtime's tools, /kiwi, the manual, START-CELL.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Config, a top-level key beside the others:
;;;;   "kiwi": {"command": "npx", "package": "@harrylabsj/kiwi@0.12.24",
;;;;            "timeout_seconds": 120, "db_path": ""}
;;;; Nothing is required when Node.js and npm are on PATH: the pinned buyer
;;;; runtime is started from npx, and its store keeps its own default under
;;;; the home unless db_path names another file.

(in-package #:nodecode-kiwi)

;;; --- the manual -------------------------------------------------------------
;;; (help :kiwi) answers it while the cell runs; a request carries the one line
;;; the :HELP clause below gives, never this text.

(defparameter *primer*
  "Kiwi is reachable through the nodecode-kiwi cell: a buyer runtime for supplier
discovery and price negotiation, started on this machine from a pinned npm package.
  kiwi_search            find merchants on the Kiwi Network: short product words, never a
                         specification essay.
  kiwi_request_quotes     start an RFQ: a CommerceIntent (items with an object-shaped
                         quantity, constraints), the merchants found, an idempotency key.
  kiwi_get_task           the quotes, partial failures and pending approvals of a task.
  kiwi_negotiate          counter-offer or clarify with one merchant.
  kiwi_accept_agreement   accept the non-binding agreement (asks you first).
  kiwi_get_agreement      read the agreement and its audit digest.
  kiwi_handoff            the checkout / PO / contact entry point (asks you first).
  kiwi_approve, kiwi_reject          answer a pending approval after the operator agrees.
  kiwi_follow_merchant, kiwi_unfollow_merchant, kiwi_list_follows,
  kiwi_get_follow_updates            the merchants followed, and what they changed.
  kiwi_tools              the tools the installed runtime answers, as it describes them.
The loop: search, quote, read, negotiate, accept, hand off. Kiwi never pays, orders
or locks stock: the handoff is an entry point the operator finishes themselves.
Two rules the model owns. Approval: kiwi_accept_agreement and kiwi_handoff ask by
default, so show the operator the candidates, the terms and the amount, and get an
explicit yes before kiwi_approve -- never approve on the model's own reading.
Sources: kiwi_search covers the Kiwi Network only. Internet listings, when this
session has web tools, are a second, separately labelled section with the platform
and the original link; they have no Kiwi merchant identity, so they never go into
merchant_ids and never carry an agreement. A page price is not a quote, and two
currencies or units are not a cheapest choice. Pass only what the purchase needs
in the intent: no chat history, no memory, no e-mail address.
ERROR: KIWI: ... is the runtime refusing or not answering; read it, fix one thing,
retry once. A handshake failure usually means Node.js or npm is not on PATH, or
the pinned package could not be downloaded."
  "What (help :kiwi) answers while the cell runs.")

;;; --- the tools the pinned runtime answers --------------------------------------
;;; One row a tool: its name on the runtime, what the model reads, and the
;;; arguments declared here. The runtime is asked with the same names and the
;;; arguments the model sends; kiwi_tools answers what the installed version
;;; really declares, which is where a bumped pin is read back.

(defparameter +mcp-tools+
  '(("kiwi_search"
     "Find suppliers and merchants on the Kiwi Network. Short product words work best; put specifications in constraints or preferences. Covers the Kiwi Network only -- internet e-commerce listings come from this session's own web tools, in a separately labelled section, and searching never sends an RFQ."
     (("query" "string" t "short product words to match merchants on")
      ("max_results" "integer" nil "how many merchants to list")
      ("currency" "string" nil "the currency quotes should be read in")))
    ("kiwi_request_quotes"
     "Start an RFQ: fan a purchase intent out to the merchants found and collect their quotes. The intent carries only what the purchase needs -- items with a short query and an object-shaped quantity, constraints, preferences -- never chat history, memory or e-mail addresses. Send an idempotency key so a retry does not quote twice."
     (("intent" "object" t "the CommerceIntent: intent_type, items, constraints")
      ("merchant_ids" "array" nil "the merchant ids kiwi_search answered")
      ("idempotency_key" "string" nil "one key per RFQ, reused on a retry")))
    ("kiwi_get_task"
     "Read one RFQ task: the quotes that came back, the items that failed, and any approval waiting. Check it before negotiating or accepting, and report partial_success as successes and failures, not as one answer."
     (("task_id" "string" t "the task kiwi_request_quotes answered")))
    ("kiwi_negotiate"
     "Counter-offer or clarify with one merchant on a task: a price, a quantity, a lead time, an MOQ. One message per round, and read the task again before quoting a merchant's number back to the operator."
     (("task_id" "string" t "the task to negotiate on")
      ("merchant_id" "string" nil "the merchant to talk to")
      ("message" "string" nil "what to ask or offer, in words")))
    ("kiwi_accept_agreement"
     "Accept a merchant's non-binding agreement on a task. Asks the operator by default: show the candidates, the terms and the amount, get an explicit yes, call kiwi_approve, then retry here with the approval_id."
     (("task_id" "string" t "the task whose agreement is accepted")
      ("merchant_id" "string" nil "whose agreement")
      ("approval_id" "string" nil "the approval kiwi_approve answered")))
    ("kiwi_get_agreement"
     "Read an accepted agreement and its audit digest, before it is handed off."
     (("task_id" "string" t "the task whose agreement is read")
      ("approval_id" "string" nil "the approval the acceptance answered")))
    ("kiwi_handoff"
     "Produce the trade handoff: the checkout, purchase-order or contact entry point for an accepted agreement. Kiwi neither pays nor orders, so verify the target URL and say plainly that nothing was paid or placed."
     (("task_id" "string" t "the task to hand off")
      ("approval_id" "string" nil "the approval the acceptance answered")))
    ("kiwi_approve"
     "Approve one pending approval, after the operator's explicit yes. Never answer one on the model's own reading of the terms."
     (("approval_id" "string" t "the approval waiting")
      ("note" "string" nil "one line of what was approved, for the audit")))
    ("kiwi_reject"
     "Reject one pending approval. A rejection is a hard refusal: report it, do not retry it on your own."
     (("approval_id" "string" t "the approval to reject")
      ("reason" "string" nil "why, in one line")))
    ("kiwi_follow_merchant"
     "Follow a merchant, so its updates are pulled later without searching again."
     (("merchant_id" "string" t "the merchant to follow")))
    ("kiwi_unfollow_merchant"
     "Stop following a merchant."
     (("merchant_id" "string" t "the merchant to unfollow")))
    ("kiwi_list_follows"
     "The merchants followed on this machine, and when each last answered."
     ())
    ("kiwi_get_follow_updates"
     "Pull what the followed merchants changed: new listings, prices, lead times. Read it before quoting an old price back to the operator."
     (("merchant_id" "string" nil "one merchant; omit for every one followed")
      ("since" "string" nil "an RFC3339 time to read from"))))
  "The 13 tools the pinned buyer runtime exposes, with the arguments this cell
declares for the model. Descriptions are the plugin's own workflow rules.")

(defun schema-of (name)
  "The schema tool NAME is registered with, in the plist shape REGISTER-TOOL
reads: the row's description, and the JSON-Schema object of the arguments the
row declares -- open, so an argument the runtime also takes still reaches it."
  (let* ((entry (assoc name +mcp-tools+ :test #'string=))
         (arguments (third entry))
         (properties (nlk:make-json-object)))
    (dolist (argument arguments)
      (destructuring-bind (key type required doc) argument
        (declare (ignore required))
        (setf (gethash key properties)
              (nlk:json-object "type" type "description" doc))))
    (list :description (second entry)
          :parameters (nlk:json-object
                       "type" "object"
                       "properties" properties
                       "required" (coerce (loop for argument in arguments
                                                 when (third argument) collect (first argument))
                                           'vector)
                       "additionalProperties" t))))

(defun tool-body (name)
  "The function one registered tool is: NAME on the runtime, with the
arguments the model sent."
  (lambda (arguments) (call-mcp name arguments)))

(defparameter *tools-schema*
  (list :description
        "List the tools the installed Kiwi buyer runtime answers, one line each with its description, and reach for the one a sourcing question needs. Use it when a kiwi_* tool answers that it does not know an argument, or after the package was bumped."
        :parameters (nlk:json-object "type" "object"
                                      "properties" (nlk:make-json-object)
                                      "additionalProperties" nil))
  "kiwi_tools takes no arguments.")

(defun tools-tool (arguments)
  "What the runtime answers, in words; a runtime that will not start answers
an ERROR line instead of throwing."
  (declare (ignore arguments))
  (handler-case (tools-text)
    (kiwi-error (condition) (format nil "ERROR: KIWI: ~a" condition))
    (error (condition) (format nil "ERROR: KIWI: ~a" condition))))

;;; --- reading the section -------------------------------------------------------

(defun named-blank-p (table key)
  "Whether the config named KEY and left it blank -- which the declaration
reads as absent, and the default would then quietly answer."
  (multiple-value-bind (value present) (and table (gethash key table))
    (and present (null (nonblank value)))))

(defun read-settings (values table)
  "The declared VALUES, with the launcher and the store's path read here once."
  (let ((command (nonblank (getf values :command)))
        (package (nonblank (getf values :package)))
        (db (nonblank (getf values :db-path))))
    (when (named-blank-p table "command") (setf command nil))
    (when (named-blank-p table "package") (setf package nil))
    (unless command
      (nlk:config-error
       "no command: kiwi.command names the launcher that starts the buyer runtime, usually npx"))
    (unless package
      (nlk:config-error
       "no package: kiwi.package names the pinned runtime package, usually @harrylabsj/kiwi@0.12.24"))
    (list :command command
          :package package
          :timeout-seconds (or (getf values :timeout-seconds) +timeout-default+)
          :db-path db
          ;; npx takes -y so a first launch needs no prompt; a wrapper around
          ;; it that takes the same verbs answers the same way.
          :argv (list command "-y" package "mcp" "serve"))))

;;; --- /kiwi --------------------------------------------------------------------

(defparameter *slash-help*
  (format nil "Usage: /kiwi [tools|help]~%  tools        Start the buyer runtime and list the tools it answers.~%  help         This text.~%Requires Node.js and npm on PATH; the pinned package is downloaded on the first call.")
  "What /kiwi with no arguments explains.")

(defun words-of (args)
  "ARGS split on whitespace."
  (uiop:split-string (or args "") :separator '(#\Space #\Tab #\Newline)))

(defun run-slash (args session-id)
  "One /kiwi invocation: the runtime's tools, or the usage text."
  (declare (ignore session-id))
  (let ((command (string-downcase (or (first (words-of args)) "tools"))))
    (cond ((member command '("help" "-h" "--help") :test #'string=) *slash-help*)
          ((member command '("tools" "status" "list") :test #'string=)
           (handler-case (tools-text)
             (kiwi-error (condition) (format nil "Kiwi error: ~a" condition))
             (error (condition) (format nil "Kiwi error: ~a" condition))))
          (t (format nil "Unknown /kiwi subcommand: ~a~%~%~a" command *slash-help*)))))

;;; --- the declaration -----------------------------------------------------------

(nle:define-cell kiwi
  (:section ("kiwi")
    (:guide "nothing here is required when Node.js and npm are on PATH: the pinned buyer runtime is started with npx -y @harrylabsj/kiwi@0.12.24 mcp serve; command and package name what is started, timeout_seconds bounds one call, and db_path moves the runtime's own SQLite store")
    ("command" :string :default "npx"
     :doc "the launcher that starts the buyer runtime: npx, or a wrapper that takes `-y PACKAGE mcp serve`")
    ("package" :string :default "@harrylabsj/kiwi@0.12.24"
     :doc "the pinned npm package and version the runtime is started from")
    ("timeout_seconds" :integer :default 120 :min 5
     :doc "the longest one call may take, the first download included")
    ("db_path" :string
     :doc "the file the runtime keeps its SQLite store in; empty leaves it at the runtime's own default under home"))
  (:settings #'read-settings)
  (:tool "kiwi_search" (schema-of "kiwi_search") (tool-body "kiwi_search"))
  (:tool "kiwi_request_quotes" (schema-of "kiwi_request_quotes") (tool-body "kiwi_request_quotes"))
  (:tool "kiwi_get_task" (schema-of "kiwi_get_task") (tool-body "kiwi_get_task"))
  (:tool "kiwi_negotiate" (schema-of "kiwi_negotiate") (tool-body "kiwi_negotiate"))
  (:tool "kiwi_accept_agreement" (schema-of "kiwi_accept_agreement") (tool-body "kiwi_accept_agreement"))
  (:tool "kiwi_get_agreement" (schema-of "kiwi_get_agreement") (tool-body "kiwi_get_agreement"))
  (:tool "kiwi_handoff" (schema-of "kiwi_handoff") (tool-body "kiwi_handoff"))
  (:tool "kiwi_approve" (schema-of "kiwi_approve") (tool-body "kiwi_approve"))
  (:tool "kiwi_reject" (schema-of "kiwi_reject") (tool-body "kiwi_reject"))
  (:tool "kiwi_follow_merchant" (schema-of "kiwi_follow_merchant") (tool-body "kiwi_follow_merchant"))
  (:tool "kiwi_unfollow_merchant" (schema-of "kiwi_unfollow_merchant") (tool-body "kiwi_unfollow_merchant"))
  (:tool "kiwi_list_follows" (schema-of "kiwi_list_follows") (tool-body "kiwi_list_follows"))
  (:tool "kiwi_get_follow_updates" (schema-of "kiwi_get_follow_updates") (tool-body "kiwi_get_follow_updates"))
  (:tool "kiwi_tools" *tools-schema* 'tools-tool)
  (:command "kiwi" 'run-slash
            :description "Kiwi buyer runtime: the tools it answers")
  (:help :kiwi
   "kiwi: source suppliers and negotiate purchases on the Kiwi network - the kiwi_* tools, /kiwi tools"
   *primer*))

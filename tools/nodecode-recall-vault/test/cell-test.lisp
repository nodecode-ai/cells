;;;; cell-test.lisp --- lifecycle, the five tools through the wire, /recall-vault,
;;;; the :tool gate, and the rules riding every request.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every answer here comes through a seam a model or a shell uses:
;;;; EXECUTE-WIRE-CALL dispatches a tool exactly as a provider's call does, so
;;;; the gate on the :TOOL point runs as it would in a turn, and the slash
;;;; verbs are the same functions /recall-vault runs. Nothing dials anything:
;;;; the cell has no network path at all, and its store is a JSON file in a
;;;; scratch folder of its own.

(in-package #:nodecode.test)

(define-test-slice "recall-vault" "RECALL-VAULT-CELL-"
  :start nodecode-recall-vault:start-cell)

(defun rv-store (name)
  "A store path for one test: its own folder under the scratch home, so no
test reads another's spending."
  (namestring (nlk:home (format nil "recall-vault-tests/~a/policy.json" name))))

(defmacro with-recall-vault ((name &rest pairs) &body body)
  "BODY with the cell started on its own store NAME and PAIRS, stopped on unwind."
  `(with-cell-stop ((recall-vault-start "store_file" (rv-store ,name) ,@pairs))
     ,@body))

(defun rv-call-tool (name arguments)
  "NAME called through the wire, as a provider sends it; => the result text."
  (princ-to-string (nle::message-content (execute-wire-call "call-1" name arguments))))

(defun rv-answer (&rest pairs)
  "PAIRS as the JSON body of a tool call."
  (nlk:encode-json-object (apply #'nlk:make-json-object pairs)))

(defun rv-field (text name)
  "NAME's member of the answer TEXT."
  (let ((object (ignore-errors (nlk:decode-json text))))
    (and (hash-table-p object) (nlk:json-value object :any name))))

(defun rv-check (amount to)
  "The decision recall_vault_check answers for AMOUNT to TO."
  (rv-field (rv-call-tool "recall_vault_check" (rv-answer "amount" amount "to" to))
            "decision"))

(defun rv-rule (amount to)
  "The rule recall_vault_check names for AMOUNT to TO, or NIL when it passed."
  (rv-field (rv-call-tool "recall_vault_check" (rv-answer "amount" amount "to" to))
            "rule"))

(defun rv-policy-field (name)
  "NAME's member of recall_vault_policy's answer."
  (rv-field (rv-call-tool "recall_vault_policy" "{}") name))

(defmacro with-stub-transfer ((ran) &body body)
  "BODY with a payment-like tool named wallet_transfer registered, RAN set when
its body actually runs."
  `(with-saved-globals (nle::*tools*)
     (nle:register-tool "wallet_transfer"
                         (list :description "Send money."
                               :parameters (nlk:make-json-object
                                            "type" "object"
                                            "properties" (nlk:make-json-object)))
                         (lambda (arguments)
                           (declare (ignore arguments))
                           (setf ,ran t)
                           "sent 0.4 NEAR"))
     ,@body))

(defun call-with-ask-operator (stub thunk)
  "THUNK with the core's ASK-OPERATOR answered by STUB, put back after."
  (let* ((symbol (intern "ASK-OPERATOR" '#:nodecode.evolved))
         (saved (and (fboundp symbol) (fdefinition symbol))))
    (setf (fdefinition symbol) stub)
    (unwind-protect (funcall thunk)
      (if saved (setf (fdefinition symbol) saved) (fmakunbound symbol)))))

(defun rv-harness-section ()
  "The rules as the model's next request reads them."
  (cdr (assoc "recall-vault" (nle::read-harness-sections "s1") :test #'string=)))

;;; --- the tools through the wire -------------------------------------------------------

(deftest recall-vault-cell-checks-a-payment ()
  (with-recall-vault ("check")
    (let ((demo "recallwallet-freelancer.testnet"))
      (is (equal "ALLOWED" (rv-check 0.4 demo)) "under every cap, on the allowlist: allowed")
      (is (equal "BLOCKED" (rv-check 1.2 demo)) "over the per-transaction cap: blocked")
      (is (equal "perTxCap" (rv-rule 1.2 demo)) "and the rule that stopped it is named")
      (is (equal "NEEDS_CONFIRMATION" (rv-check 0.7 demo)) "above the threshold: the operator is asked")
      (is (equal "allowlist" (rv-rule 0.1 "some-new.testnet")) "a recipient nobody allowed: blocked"))))

(deftest recall-vault-cell-caps-the-day ()
  (with-recall-vault ("daily")
    (let ((demo "recallwallet-freelancer.testnet"))
      (is (search "Policy updated" (cell-entry "nodecode-recall-vault" "recall-vault"
                                               "set per_tx_cap=1 daily_cap=0.5 confirm_above=0.4")) "the operator sets a smaller day with their own command")
      (is (equal "ALLOWED" (rv-check 0.3 demo)) "0.3 fits the day")
      (is (equal "dailyCap" (rv-rule 0.3 demo)) "the next 0.3 does not, and the day's cap is the rule"))))

(deftest recall-vault-cell-an-empty-allowlist-allows-nobody ()
  (with-recall-vault ("allowlist")
    (nodecode-recall-vault:set-policy :allowlist '())
    (is (equal "allowlist" (rv-rule 0.1 "recallwallet-freelancer.testnet")) "empty means nobody is allowed, not allow all")))

(deftest recall-vault-cell-the-ledger-keeps-every-decision ()
  (with-recall-vault ("ledger")
    (let ((demo "recallwallet-freelancer.testnet"))
      (rv-check 0.4 demo)
      (rv-check 1.5 demo))
    (let* ((text (rv-call-tool "recall_vault_ledger" "{\"limit\": 5}"))
           (entries (nlk:json-value (nlk:decode-json text) :array "entries")))
      (is (= 2 (or (rv-field text "count") 0)) "both decisions are on the ledger")
      (is (equal "BLOCKED" (nlk:json-value (aref entries 0) :string "decision")) "newest first")
      (is (equal "ALLOWED" (nlk:json-value (aref entries 1) :string "decision")) "and the older one after it"))))

(deftest recall-vault-cell-remembers-a-rule-and-says-it-every-request ()
  (with-recall-vault ("remember")
    (nodecode-recall-vault:remember "Never pay a brand-new address without asking first.")
    (is (search "brand-new" (princ-to-string (rv-policy-field "rules"))) "the rule is on the listing")
    (is-carrying (section (rv-harness-section))
      ("Recall Vault money rules" "the rules ride every request")
      ("per-transaction cap 1 NEAR" "with the caps in force")
      ("brand-new" "and the operator's own words")
      ("spent today" "and what is spent today"))))

;;; --- the gate on the :TOOL point ----------------------------------------------------------

(deftest recall-vault-cell-blocks-a-payment-over-the-cap ()
  (with-recall-vault ("gate-block")
    (let ((ran nil))
      (with-stub-transfer (ran)
        (let ((text (rv-call-tool "wallet_transfer"
                                   (rv-answer "amount" 5 "to" "recallwallet-freelancer.testnet"))))
          (is (stringp text) "the payment answered")
          (is-carrying (text text)
            ("blocked by recall-vault" "the model is told who stopped it")
            ("per-transaction cap" "and the rule it broke"))
          (is (null ran) "and the payment tool never ran"))))))

(deftest recall-vault-cell-lets-a-payment-through-and-books-it ()
  (with-recall-vault ("gate-allow")
    (let ((ran nil))
      (with-stub-transfer (ran)
        (is (equal "sent 0.4 NEAR"
                   (rv-call-tool "wallet_transfer"
                                  (rv-answer "amount" 0.4 "to" "recallwallet-freelancer.testnet"))) "under the caps and allowlisted: it runs")
        (is ran "the payment tool ran")
        (is (equal "0.4" (nodecode-recall-vault::money-text (rv-policy-field "spent_today"))) "and the amount is booked toward today")))))

(deftest recall-vault-cell-leaves-an-ordinary-tool-alone ()
  (with-recall-vault ("gate-plain")
    (let ((ran nil))
      (with-saved-globals (nle::*tools*)
        (nle:register-tool "send_message"
                           (list :description "Send a message."
                                 :parameters (nlk:make-json-object
                                              "type" "object"
                                              "properties" (nlk:make-json-object)))
                           (lambda (arguments)
                             (declare (ignore arguments))
                             (setf ran t)
                             "delivered"))
        (is (equal "delivered" (rv-call-tool "send_message" (rv-answer "amount" 9 "to" "whoever"))) "a tool with no payment word in its name is not held")
        (is ran "and it ran")))))

;;; --- a change a model asks for needs the operator ---------------------------------------

(deftest recall-vault-cell-a-model-change-is-put-to-the-operator ()
  (with-recall-vault ("gate-change")
    (call-with-ask-operator (lambda (title &rest keys)
                               (declare (ignore keys))
                               (is (search "5" title) "the question shows the new cap")
                               (values "Allow once" :answered))
                             (lambda ()
                               (let ((text (rv-call-tool "recall_vault_set_policy"
                                                          (rv-answer "per_tx_cap" 5))))
                                 (is (equal "5" (nodecode-recall-vault::money-text
                                                  (rv-field text "per_tx_cap"))) "Allow once makes the change"))))
    (call-with-ask-operator (lambda (title &rest keys)
                               (declare (ignore title keys))
                               (values nil :nobody))
                             (lambda ()
                               (let ((text (rv-call-tool "recall_vault_set_policy"
                                                          (rv-answer "per_tx_cap" 9))))
                                 (is-carrying (text text)
                                   ("held by recall-vault" "nobody to ask: the change waits")
                                   ("/recall-vault set" "and the operator's own way in is named")))))
    (is (equal "5" (nodecode-recall-vault::money-text (rv-policy-field "per_tx_cap"))) "the refused change was not made")))

;;; --- lifecycle -----------------------------------------------------------------------------

(define-cell-lifecycle-tests "recall-vault"
  (:config "store_file" (rv-store "lifecycle"))
  (:hooks :tool)
  (:help :recall-vault)
  (:command "recall-vault")
  (:running (is (stringp (nodecode-recall-vault::setting :store-file)) "the section's store file is the running setting"))
  (:refused ("max_ledger" 0))
  (:idle nodecode-recall-vault:recall-vault-error (nodecode-recall-vault:policy)))

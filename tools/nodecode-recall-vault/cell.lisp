;;;; cell.lisp --- the rules, the five tools, /recall-vault, and the one hook.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of the Hermes Agent plugin tydavid5/recall-vault (see NOTICE).
;;;; The original gave an agent a memory of its own money rules and a
;;;; pre_tool_call hook that held every payment-like tool call to them. In this
;;;; image the same rules ride two seams:
;;;;
;;;;   the :TOOL point   one around-chain over every tool dispatch (turn.lisp
;;;;                      calls NLE:HOOK's chain with (:name :arguments
;;;;                      :call-id)). A call whose name looks like a payment
;;;;                      and whose arguments carry an amount and a recipient
;;;;                      is checked before it runs: over a cap, blocked with the
;;;;                      rule quoted back and the call never made; above the
;;;;                      confirm threshold, put to the operator as Allow once or
;;;;                      Deny; a recipient off the allowlist, blocked.
;;;;
;;;;   the harness sections   the rules ride a standing section of their own, so
;;;;                           the model reads the caps, the threshold, today's
;;;;                           spending and the operator's own rules at every
;;;;                           request, instead of forgetting them.
;;;;
;;;; Dry run, as the original: nothing here moves money. A check logs a
;;;; decision and counts toward today; the payment itself is the other tool's
;;;; business.
;;;;
;;;; NOT a security boundary: EVAL has the host's full authority, so a model
;;;; can call (recall-vault:set-policy ...) from Lisp and step around the
;;;; approval gate. It is a speed bump for the common slip, as guard is.

(in-package #:nodecode-recall-vault)

;;; --- the words ---------------------------------------------------------------------

(defparameter +primer+
  "Recall Vault keeps the agent's money rules and holds every payment-like tool
call to them before it runs: a call whose name carries pay, payment, transfer,
withdraw, payout or settle, and whose arguments carry both an amount and a
recipient.

  ALLOWED            under the per-transaction cap, under the day's cap, and
                     the recipient is on the allowlist: the call runs, and the
                     amount is booked.
  BLOCKED            over a cap, or a recipient nobody approved: the call does
                     not run, and the rule that stopped it is quoted.
  NEEDS_CONFIRMATION above the confirm threshold but within the caps: the
                     operator is asked Allow once or Deny first.

  recall_vault_check       amount=..., to=...  a dry check; it answers the
                           decision and books an allowed one toward today
  recall_vault_policy      the caps, the threshold, the allowlist, today's
                           spending and the remembered rules
  recall_vault_set_policy  per_tx_cap=, daily_cap=, confirm_above=,
                           allowlist=[...], reset_spent=true - a change from
                           the model is put to the operator first
  recall_vault_ledger      every decision, newest first
  recall_vault_remember    rule=... a plain-language rule, shown to the model
                           every request; a model's rule is put to the
                           operator first

The operator types /recall-vault to read it, /recall-vault set per_tx_cap=2 to
change a cap, and /recall-vault check 0.4 <address> to try one. Only NEAR is
held, and no money moves: this is a policy vault, not a wallet."
  "What (help :recall-vault) answers while the cell runs.")

(defparameter *section-name* "recall-vault"
  "The harness section the rules ride, and the folder's own name.")

;;; --- what a payment looks like --------------------------------------------------------

(defparameter +payment-name-words+ '("pay" "payment" "transfer" "withdraw" "payout" "settle")
  "A tool name carrying one of these looks like it can move money.")

(defparameter +amount-keys+ '("amount" "amount_near" "near_amount" "payment_amount"
                              "value" "total" "sum")
  "The argument keys that carry an amount.")

(defparameter +recipient-keys+ '("to" "recipient" "to_address" "destination"
                                 "payee" "address" "account")
  "The argument keys that carry a recipient.")

(defun argument (arguments key)
  "KEY's member of the ARGUMENTS hash, or NIL."
  (and (hash-table-p arguments) (gethash key arguments)))

(defun argument-string (arguments key)
  "KEY's member of ARGUMENTS as a trimmed string, or NIL when it is empty."
  (let ((value (argument arguments key)))
    (when (or (stringp value) (numberp value))
      (let ((text (nlk:trimmed (princ-to-string value))))
        (and (plusp (length text)) text)))))

(defun payment-of (name arguments)
  "A payment-like call's (values AMOUNT TO), or NIL when this is no payment:
its name has to carry a payment word, and its arguments both an amount and
a recipient -- so send_message, send_email and read_file are never held."
  (let* ((name (string-downcase (or name "")))
         (looks (or (some (lambda (word) (search word name)) +payment-name-words+)
                    (and (search "send" name)
                         (or (search "money" name) (search "payment" name)
                             (search "near" name))))))
    (when looks
      (let ((amount (loop for key in +amount-keys+
                          when (argument arguments key)
                            do (return (amount-value (argument arguments key)))))
            (to (loop for key in +recipient-keys+
                      when (argument-string arguments key)
                        do (return (argument-string arguments key)))))
        (when (and amount to) (list amount to))))))

;;; --- asking the operator --------------------------------------------------------------

(defparameter +allow-once+ "Allow once")
(defparameter +deny+ "Deny")

(defun ask-operator-function ()
  "The core's ASK-OPERATOR, or NIL in a core that has none."
  (let ((symbol (find-symbol "ASK-OPERATOR" '#:nodecode.evolved)))
    (and symbol (fboundp symbol) (symbol-function symbol))))

(defun ask-operator (title detail)
  "Put TITLE and DETAIL to the operator. => (values ANSWER WHY), WHY :OFF when
there is nothing to ask, :NOBODY when no surface is open, :TIMEOUT otherwise."
  (let ((ask (and (setting :confirm-by-ask) (ask-operator-function))))
    (if (null ask)
        (values nil :off)
        (funcall ask title
                 :detail detail
                 :choices (list +allow-once+ +deny+)
                 :seconds (max 5 (or (setting :ask-seconds) 120))))))

(defun asked-text (why)
  "The clause a refusal ends on, saying what became of the question."
  (case why
    (:deny "the operator was asked and chose Deny")
    (:timeout "the operator was asked and did not answer in time")
    (:nobody "the operator was not at a surface that answers, so nobody could be asked")
    (t "the operator could not be asked")))

;;; --- answers, in the shape the tools speak ------------------------------------------------

(defun answer (&rest pairs)
  "PAIRS as the JSON string a tool answers."
  (nlk:encode-json-object (apply #'nlk:make-json-object pairs)))

(defun allowlist-vector (policy)
  "POLICY's allowlist as the JSON array an answer carries."
  (apply #'vector (getf policy :allowlist)))

(defun decision-pairs (result)
  "RESULT as the members every answer about a decision carries."
  (list "decision" (getf result :decision)
        "rule" (or (getf result :rule) :null)
        "reason" (getf result :reason)
        "amount" (coerce (or (getf result :amount) 0.0d0) 'double-float)
        "to" (or (getf result :to) "")))

(defun policy-pairs (state)
  "STATE as the members an answer about the policy carries."
  (let ((policy (getf state :policy)))
    (list "currency" (or (getf policy :currency) *currency*)
          "per_tx_cap" (coerce (getf policy :per-tx-cap) 'double-float)
          "daily_cap" (coerce (getf policy :daily-cap) 'double-float)
          "confirm_above" (coerce (getf policy :confirm-above) 'double-float)
          "allowlist" (allowlist-vector policy)
          "allowlist_note" (if (null (getf policy :allowlist))
                                "Empty allowlist means nobody is allowed, not allow all."
                                :null)
          "spent_today" (coerce (or (getf state :spent-today) 0.0d0) 'double-float)
          "spent_day" (or (getf state :spent-day) "")
          "rules" (apply #'vector (getf state :rules))
          "dry_run" t
          "note" "Dry-run only, no money moves.")))

;;; --- the check, shared by the tool, the command and the verb ------------------------------

(defun decide (amount to)
  "AMOUNT and TO against the vault as it stands, booking what is allowed.
=> (values RESULT STATE-AFTER) or (values NIL ERROR-TEXT) for a currency
this version does not hold."
  (let* ((state (load-state))
         (result (check-payment (getf state :policy) amount to
                                 (getf state :spent-today))))
    (if (equal "ALLOWED" (getf result :decision))
        (let ((entry (record-allowed amount to)))
          (values (append result (list :amount amount :to to
                                       :spent-today-after (getf entry :spent-today-after)))
                  state))
        (progn (record-entry (list :decision (getf result :decision)
                                   :rule (getf result :rule)
                                   :reason (getf result :reason)
                                   :amount (or amount 0.0d0)
                                   :to to))
               (values (append result (list :amount (or amount 0.0d0) :to to))
                       state)))))

;;; --- the five tools --------------------------------------------------------------------------

(nlk:define-contract check-call
  (:doc "Check a NEAR payment against the operator's money rules before paying. Answers ALLOWED, BLOCKED with the rule that stopped it, or NEEDS_CONFIRMATION when the amount is above the confirm threshold. An allowed check is booked toward today's spending and written to the audit ledger. This is a dry check: it moves no money.")
  ("amount" :number :required t :doc "payment amount in NEAR, for example 0.4")
  ("to" :text :required t :doc "recipient address, for example someone.testnet")
  ("currency" :string :doc "only NEAR is held in this version"))

(defparameter +policy-schema+
  (list :description
        "Show the money rules in force: the per-transaction cap, the daily cap, the confirm threshold, the allowlist, what is spent today, and the rules the operator wrote down."
        :parameters (nlk:make-json-object "type" "object" "properties" (nlk:make-json-object)))
  "The recall_vault_policy schema: a hand-written one, because the tool takes
no argument at all and a contract with no members types nothing.")

(nlk:define-contract set-policy-call
  (:doc "Change the money rules. Give only the members to change; the caps and the threshold are in NEAR, and the allowlist is replaced whole. An empty allowlist means nobody is allowed, not allow all. A change made by the model is put to the operator as a question first, showing the exact change; the operator changes rules directly with /recall-vault set.")
  ("per_tx_cap" :number :doc "the most NEAR one payment may be")
  ("daily_cap" :number :doc "the most NEAR one day may spend")
  ("confirm_above" :number :doc "a payment above this many NEAR is put to the operator first")
  ("allowlist" :array :of :string :doc "the whole list of recipients allowed; an empty list allows nobody")
  ("reset_spent" :boolean :doc "true resets what is spent today to zero"))

(nlk:define-contract ledger-call
  (:doc "Show the audit ledger: every allowed, blocked and held payment decision, newest first.")
  ("limit" :integer :doc "how many recent entries to show, 20 by default, at most 100"))

(nlk:define-contract remember-call
  (:doc "Write down one plain-language money rule for the agent, for example 'Never pay a brand-new address without asking first.' Remembered rules are shown to the model at every request. Enforcement uses the caps, the threshold and the allowlist; a rule is advice. A rule the model writes is put to the operator as a question first, showing its text.")
  ("rule" :text :required t :doc "the rule, in the operator's own words"))

(defun check-tool (call)
  "The recall_vault_check tool: CALL checked, and booked when it passes."
  (let ((currency (nlk:trimmed (princ-to-string (or (check-call-currency call) ""))))
        (amount (amount-value (check-call-amount call)))
        (to (nlk:trimmed (check-call-to call))))
    (if (and (plusp (length currency)) (not (string-equal currency *currency*)))
        (answer "ok" nil "error" "Only NEAR is supported in this version.")
        (multiple-value-bind (result state) (decide amount to)
          (declare (ignore state))
          (if (equal "ALLOWED" (getf result :decision))
              (apply #'answer (append (list "ok" t) (decision-pairs result)
                                      (list "currency" *currency* "dry_run" t
                                            "note" "Dry check only, no money moved."
                                            "spent_today" (coerce (getf result :spent-today-after)
                                                                   'double-float))))
              (apply #'answer (append (list "ok" t) (decision-pairs result)
                                      (list "currency" *currency* "dry_run" t))))))))

(defun policy-tool (call)
  "The recall_vault_policy tool: the rules in force."
  (declare (ignore call))
  (apply #'answer (append (list "ok" t) (policy-pairs (load-state)))))

(defun set-policy-member-value (call member)
  "CALL's value for MEMBER, one of the three numbers the policy holds."
  (case member
    (:per-tx-cap (set-policy-call-per-tx-cap call))
    (:daily-cap (set-policy-call-daily-cap call))
    (:confirm-above (set-policy-call-confirm-above call))))

(defun set-policy-tool (call)
  "The recall_vault_set_policy tool: CALL's members written to the policy."
  (let ((state (load-state))
        (changed '())
        (refusal nil))
    (dolist (member '(:per-tx-cap :daily-cap :confirm-above))
      (let ((value (set-policy-member-value call member)))
        (when value
          (let ((number (amount-value value)))
            (cond ((null number)
                   (setf refusal (format nil "~a must be a number." (policy-name member))))
                  ((<= number 0)
                   (setf refusal (format nil "~a must be greater than zero." (policy-name member))))
                  (t (setf (getf (getf state :policy) member) number)
                     (push member changed)))))))
    (let ((allowlist (set-policy-call-allowlist call)))
      (when (and allowlist (null refusal))
        (setf (getf (getf state :policy) :allowlist)
              (loop for item across allowlist
                    for text = (nlk:trimmed (princ-to-string item))
                    when (plusp (length text)) collect text))
        (push :allowlist changed)))
    (when (and (set-policy-call-reset-spent call) (null refusal))
      (setf (getf state :spent-today) 0.0d0)
      (push :reset-spent changed))
    (cond
      (refusal (answer "ok" nil "error" refusal))
      ((null changed)
       (answer "ok" nil "error" "Nothing to change. Give per_tx_cap, daily_cap, confirm_above, allowlist, or reset_spent."))
      (t (save-state state)
         (apply #'answer (append (list "ok" t
                                       "changed" (apply #'vector (mapcar #'policy-name changed)))
                                 (policy-pairs (load-state))))))))

(defun policy-name (key)
  "KEY as the member name a person reads."
  (string-downcase (substitute #\_ #\- (symbol-name key))))

(defun ledger-tool (call)
  "The recall_vault_ledger tool: the newest decisions."
  (ledger-answer (ledger-call-limit call)))

(defun ledger-answer (limit)
  "LIMIT of the newest ledger entries, as the JSON an answer carries."
  (let* ((limit (max 1 (min 100 (or limit 20))))
         (entries (load-ledger))
         (recent (reverse (last entries (min limit (length entries))))))
    (answer "ok" t
            "count" (length entries)
            "shown" (length recent)
            "entries" (apply #'vector
                             (loop for entry in recent
                                   collect (nlk:make-json-object
                                            "decision" (getf entry :decision)
                                            "rule" (or (getf entry :rule) :null)
                                            "reason" (or (getf entry :reason) "")
                                            "amount" (coerce (or (getf entry :amount) 0.0d0)
                                                             'double-float)
                                            "to" (or (getf entry :to) "")
                                            "currency" (or (getf entry :currency) *currency*)
                                            "at" (or (getf entry :at) "")))))))

(defun remember-tool (call)
  "The recall_vault_remember tool: CALL's rule written down."
  (let* ((rule (nlk:trimmed (remember-call-rule call)))
         (count (remember-rule rule)))
    (if (null count)
        (answer "ok" nil "error" "Need a rule to remember.")
        (answer "ok" t
                "remembered" rule
                "rule_count" count
                "rules" (apply #'vector (getf (load-state) :rules))))))

;;; --- the operator's way in ---------------------------------------------------------------

(defun policy-text (&optional (state (load-state)))
  "The policy as the lines /recall-vault policy prints."
  (let ((policy (getf state :policy)))
    (format nil "Recall Vault policy (~a, dry-run): per-transaction cap ~a, daily cap ~a, confirm above ~a, spent today ~a, allowlist: ~a. Rules: ~a"
            (or (getf policy :currency) *currency*)
            (money-text (getf policy :per-tx-cap))
            (money-text (getf policy :daily-cap))
            (money-text (getf policy :confirm-above))
            (money-text (getf state :spent-today))
            (if (null (getf policy :allowlist))
                "empty, nobody is allowed"
                (or (format nil "~{~a~^, ~}" (getf policy :allowlist)) "none set"))
            (or (format nil "~{~a~^; ~}" (getf state :rules)) "none"))))

(defun ledger-text (&optional (limit 10))
  "The ledger as the lines /recall-vault ledger prints, newest first."
  (let* ((entries (load-ledger))
         (recent (reverse (last entries (min limit (length entries))))))
    (with-output-to-string (out)
      (format out "Recall Vault ledger (~d entries):" (length entries))
      (dolist (entry recent)
        (format out "~%- ~a ~a NEAR to ~a: ~a (~a)"
                (getf entry :decision)
                (money-text (getf entry :amount))
                (getf entry :to)
                (getf entry :reason)
                (getf entry :at))))))

(defun parse-policy-words (text)
  "TEXT as the key=value members /recall-vault set takes. => the members, or
a refusal string saying which one is wrong."
  (let ((parsed '()))
    (dolist (token (uiop:split-string text :separator " "))
      (unless (zerop (length token))
        (let ((at (position #\= token)))
          (when (null at)
            (return-from parse-policy-words
              (format nil "Bad field ~s. Use key=value, for example per_tx_cap=1." token)))
          (let ((key (string-downcase (subseq token 0 at)))
                (value (subseq token (1+ at))))
            (cond ((member key '("per_tx_cap" "daily_cap" "confirm_above") :test #'equal)
                   (let ((number (amount-value value)))
                     (if number
                         (setf (getf parsed (intern (string-upcase (substitute #\- #\_ key)) :keyword))
                               number)
                         (return-from parse-policy-words
                           (format nil "Bad value for ~a: ~s must be a number." key value)))))
                  ((string= key "allowlist")
                   ;; an explicitly empty allowlist is a state, not an absence:
                   ;; it means nobody is allowed, so say it was given.
                   (setf (getf parsed :allowlist)
                         (loop for item in (uiop:split-string value :separator ",")
                               for address = (nlk:trimmed item)
                               when (plusp (length address)) collect address)
                         (getf parsed :allowlist-p) t))
                  ((string= key "reset_spent")
                   (setf (getf parsed :reset-spent)
                         (member (string-downcase value) '("true" "1" "yes" "y")
                                 :test #'equal)))
                  (t (return-from parse-policy-words
                       (format nil "Unknown policy field ~s. Use per_tx_cap, daily_cap, confirm_above, allowlist, or reset_spent." key))))))))
    parsed))

(defun set-policy-words (words)
  "WORDS from PARSE-POLICY-WORDS written to the policy. => the line to print."
  (let ((state (load-state))
        (changed '()))
    (dolist (key '(:per-tx-cap :daily-cap :confirm-above))
      (when (getf words key)
        (setf (getf (getf state :policy) key) (getf words key))
        (push (policy-name key) changed)))
    (when (getf words :allowlist-p)
      (setf (getf (getf state :policy) :allowlist) (getf words :allowlist))
      (push "allowlist" changed))
    (when (getf words :reset-spent)
      (setf (getf state :spent-today) 0.0d0)
      (push "reset_spent" changed))
    (save-state state)
    (format nil "Policy updated: changed ~{~a~^, ~}. ~a"
            (nreverse changed) (policy-text (load-state)))))

(defun run-slash (args session-id)
  "One /recall-vault invocation: policy, ledger, check AMOUNT ADDRESS,
remember TEXT, or set key=value ..."
  (declare (ignore session-id))
  (let* ((text (nlk:trimmed (or args "")))
         (split (and (plusp (length text)) (position #\Space text)))
         (command (string-downcase (if split (subseq text 0 split) text)))
         (rest (if split (nlk:trimmed (subseq text (1+ split))) "")))
    (cond
      ((zerop (length text))
       "Recall Vault: /recall-vault policy | /recall-vault ledger | /recall-vault check <amount> <to> | /recall-vault remember <rule> | /recall-vault set per_tx_cap=1 daily_cap=2 confirm_above=0.5 allowlist=a,b reset_spent=true")
      ((string= command "policy") (policy-text))
      ((string= command "ledger") (ledger-text))
      ((string= command "check")
       (let ((fields (uiop:split-string rest :separator " ")))
         (if (or (< (length fields) 2) (null (amount-value (first fields))))
             "Usage: /recall-vault check <amount> <to>"
             (multiple-value-bind (result state) (decide (amount-value (first fields))
                                                          (second fields))
               (declare (ignore state))
               (format nil "~a: ~a" (getf result :decision) (getf result :reason))))))
      ((string= command "remember")
       (if (zerop (length rest))
           "Usage: /recall-vault remember <rule>"
           (progn (remember-rule rest) (format nil "Remembered: ~a" rest))))
      ((string= command "set")
       (if (zerop (length rest))
           "Usage: /recall-vault set per_tx_cap=1 daily_cap=2 confirm_above=0.5 allowlist=a,b reset_spent=true. An empty allowlist (allowlist=) means nobody is allowed."
           (let ((words (parse-policy-words rest)))
             (cond ((stringp words) words)
                   ((null words) "Nothing to change.")
                   (t (set-policy-words words))))))
      (t "Unknown Recall Vault command. Try: /recall-vault policy | /recall-vault ledger | /recall-vault check <amount> <to> | /recall-vault remember <rule> | /recall-vault set per_tx_cap=1 daily_cap=2 confirm_above=0.5 allowlist=a,b reset_spent=true"))))

;;; --- the rules, over every tool call -----------------------------------------------------

(defun set-policy-diff (arguments)
  "The plain-language change a model's set_policy call asks for: old -> new."
  (let* ((state (load-state))
         (policy (getf state :policy))
         (parts '()))
    (dolist (pair '(("per_tx_cap" . :per-tx-cap)
                    ("daily_cap" . :daily-cap)
                    ("confirm_above" . :confirm-above)))
      (let ((value (argument arguments (car pair))))
        (when value
          (push (format nil "~a: ~a -> ~a" (car pair)
                        (money-text (getf policy (cdr pair)))
                        (money-text (amount-value value)))
                parts))))
    (let ((allowlist (argument arguments "allowlist")))
      (when (and allowlist (vectorp allowlist))
        (push (format nil "allowlist: (~{~a~^, ~}) -> (~{~a~^, ~})"
                      (getf policy :allowlist)
                      (loop for item across allowlist
                            for text = (nlk:trimmed (princ-to-string item))
                            when (plusp (length text)) collect text))
              parts)))
    (when (argument arguments "reset_spent")
      (push (format nil "reset_spent: spent today ~a NEAR -> 0"
                    (money-text (getf state :spent-today)))
            parts))
    (or (format nil "~{~a~^; ~}" (nreverse parts)) "no recognised field given")))

(defun clean-result-p (text)
  "Whether TEXT reads as a call that ran. The original's test, kept: an error
in the answer means the payment did not settle, so it is not booked."
  (let ((text (string-downcase (if (stringp text) text (princ-to-string text)))))
    (not (or (uiop:string-prefix-p "error" (nlk:trimmed text))
             (search "error:" text)
             (search "\"error\"" text)
             (search "\"ok\": false" text)
             (search "\"ok\":false" text)))))

(defun run-and-book (next op amount to-address name)
  "Run the payment the rules allowed, and book it when it ran clean."
  (let ((text (funcall next op)))
    (when (clean-result-p text)
      (record-allowed amount to-address name))
    text))

(defun ask-before-change (op next title hint)
  "TITLE put to the operator; the change runs on Allow once, else it does not."
  (multiple-value-bind (answer why) (ask-operator title title)
    (cond ((equal answer +allow-once+) (funcall next op))
          ((eq why :off)
           (nle:failure
            (format nil "ERROR: held by recall-vault: ~a Nothing can be asked here, so nothing changed; the operator makes it with ~a."
                    title hint)))
          (t (nle:failure
              (format nil "ERROR: held by recall-vault: ~a ~a, so the change did not run; the operator makes it with ~a."
                      title (asked-text why) hint))))))

(defun gate-payment (op next name arguments payment)
  "One payment-like call held to the rules. => the call's text, or its refusal."
  (declare (ignore arguments))
  (destructuring-bind (amount to) payment
    (let* ((state (load-state))
           (result (check-payment (getf state :policy) amount to
                                   (getf state :spent-today)))
           (decision (getf result :decision)))
      (cond
        ((equal decision "BLOCKED")
         (record-entry (list :decision "BLOCKED" :rule (getf result :rule)
                             :reason (getf result :reason) :amount amount
                             :to to :tool name))
         (nle:failure
          (format nil "ERROR: blocked by recall-vault: ~a The call did not run -- tell the operator, do not work around the rule."
                  (getf result :reason))))
        ((equal decision "NEEDS_CONFIRMATION")
         (record-entry (list :decision "NEEDS_CONFIRMATION" :rule (getf result :rule)
                             :reason (getf result :reason) :amount amount
                             :to to :tool name))
         (multiple-value-bind (answer why)
             (ask-operator (format nil "Recall Vault holds a payment: ~a NEAR to ~a"
                                   (money-text amount) to)
                           (format nil "~a Tool: ~a." (getf result :reason) name))
           (if (equal answer +allow-once+)
               (run-and-book next op amount to name)
               (nle:failure
                (format nil "ERROR: held by recall-vault: ~a ~a, so the call did not run."
                        (getf result :reason) (asked-text why))))))
        (t (run-and-book next op amount to name))))))

(defun gate-call (op next)
  "The :TOOL point: a payment-like call, or a call that could loosen the
rules, is put to the vault before it runs; everything else runs untouched."
  (let ((name (string-downcase (princ-to-string (or (getf op :name) "")))))
    (cond
      ((string= name "recall_vault_set_policy")
       (ask-before-change op next
                          (format nil "Recall Vault policy change: ~a."
                                  (set-policy-diff (getf op :arguments)))
                          "/recall-vault set"))
      ((string= name "recall_vault_remember")
       (ask-before-change op next
                          (format nil "Recall Vault rule to remember: ~s."
                                  (nlk:trimmed (princ-to-string
                                                (or (argument (getf op :arguments) "rule") ""))))
                          "/recall-vault remember"))
      ((uiop:string-prefix-p "recall_vault_" name) (funcall next op))
      (t (let ((payment (payment-of name (getf op :arguments))))
           (if payment
               (gate-payment op next name (getf op :arguments) payment)
               (funcall next op)))))))

;;; --- the rules in front of the model -----------------------------------------------------

(defun reminder-text (&optional (state (load-state)))
  "The line the model reads at every request: the rules, and what is spent."
  (let* ((policy (getf state :policy))
         (allowlist (getf policy :allowlist))
         (rules (getf state :rules)))
    (format nil "Recall Vault money rules (~a, dry-run, no money moves): per-transaction cap ~a ~a, daily cap ~a ~a, confirm above ~a ~a, spent today ~a ~a, allowlist: ~a. Remembered rules: ~a. Check any payment with recall_vault_check before paying, and never pay an address that is not allowlisted without asking the operator first."
            *currency*
            (money-text (getf policy :per-tx-cap)) *currency*
            (money-text (getf policy :daily-cap)) *currency*
            (money-text (getf policy :confirm-above)) *currency*
            (money-text (getf state :spent-today)) *currency*
            (cond ((null allowlist) "empty, nobody is allowed")
                  (t (format nil "~{~a~^, ~}" allowlist)))
            (if rules (format nil "~{~a~^; ~}" (subseq rules 0 (min 4 (length rules)))) "none"))))

(defun remind (next session-id)
  "The harness-section read: the rules ride a standing section of their own, so
every request carries them and no turn has to remember what it was told."
  (let ((sections (funcall next session-id))
        (text (and (ignore-errors (setting :inject))
                   (ignore-errors (reminder-text)))))
    (if text
        (acons *section-name* text
               (remove *section-name* sections :key #'car :test #'equal))
        sections)))

;;; --- the verbs an eval form reads -----------------------------------------------------------

(define-verb check (amount to &optional (currency *currency*))
  "Check a payment and book it when it passes. => one line."
  (if (not (string-equal (nlk:trimmed (princ-to-string currency)) *currency*))
      (format nil "Only ~a is held in this version." *currency*)
      (multiple-value-bind (result state) (decide (amount-value amount)
                                                   (nlk:trimmed (princ-to-string to)))
        (declare (ignore state))
        (if (equal "ALLOWED" (getf result :decision))
            (format nil "~a: ~a Spent today ~a ~a."
                    (getf result :decision) (getf result :reason)
                    (money-text (getf result :spent-today-after)) *currency*)
            (format nil "~a: ~a" (getf result :decision) (getf result :reason))))))

(define-verb policy ()
  "The rules in force, as one line."
  (policy-text))

(define-verb ledger (&optional (limit 10))
  "The newest decisions, one to a line."
  (ledger-text limit))

(define-verb remember (rule)
  "Write one plain-language rule down. => one line."
  (if (remember-rule rule)
      (format nil "Remembered: ~a" (nlk:trimmed (princ-to-string rule)))
      "Need a rule to remember."))

(define-verb set-policy (&key per-tx-cap daily-cap confirm-above
                         (allowlist nil allowlist-p) reset-spent)
  "Change the rules: the caps and the threshold in NEAR, the allowlist whole.
The operator's own way in, so nothing asks a question here."
  (let ((words (list :per-tx-cap (and per-tx-cap (amount-value per-tx-cap))
                     :daily-cap (and daily-cap (amount-value daily-cap))
                     :confirm-above (and confirm-above (amount-value confirm-above))
                     :reset-spent reset-spent))
        (given (when (or per-tx-cap daily-cap confirm-above reset-spent) t)))
    (when allowlist-p
      (setf (getf words :allowlist)
            (loop for item in (if (listp allowlist) allowlist (list allowlist))
                  for text = (nlk:trimmed (princ-to-string item))
                  when (plusp (length text)) collect text)
            (getf words :allowlist-p) t
            given t))
    (if given
        (set-policy-words words)
        "Nothing to change: give per_tx_cap, daily_cap, confirm_above, allowlist or reset_spent.")))

;;; --- the declaration ------------------------------------------------------------------------

(nle:define-cell recall-vault
  (:section ("recall-vault")
    (:guide "nothing here is required: a fresh vault starts with a 1 NEAR per-transaction cap, a 2 NEAR daily cap, a 0.5 NEAR confirm threshold and the demo testnet allowlist, changed with /recall-vault set; store_file is where the two JSON files live, max_ledger how many decisions are kept, confirm_by_ask whether a payment over the threshold is put to you as Allow once or Deny, inject whether the rules ride every request")
    ("store_file" :path :default "recall-vault/policy.json"
                  :doc "the JSON file the rules, today's spending and the remembered rules are kept in, under the home unless it is absolute; the ledger sits beside it")
    ("max_ledger" :integer :default 200 :min 1
                  :doc "how many decisions the audit ledger keeps, oldest dropped first")
    ("confirm_by_ask" :boolean :default t
                      :doc "ask Allow once or Deny before a payment above the confirm threshold runs; off blocks it instead")
    ("ask_seconds" :integer :default 120 :min 5
                   :doc "how long a question waits for an answer before the payment is refused")
    ("inject" :boolean :default t
              :doc "whether the money rules ride every model request"))
  (:tool "recall_vault_check" (nlk:contract 'check-call) #'check-tool)
  (:tool "recall_vault_policy" +policy-schema+ #'policy-tool)
  (:tool "recall_vault_set_policy" (nlk:contract 'set-policy-call) #'set-policy-tool)
  (:tool "recall_vault_ledger" (nlk:contract 'ledger-call) #'ledger-tool)
  (:tool "recall_vault_remember" (nlk:contract 'remember-call) #'remember-tool)
  (:hook :tool #'gate-call)
  (:hook 'nle::read-harness-sections #'remind)
  (:command "recall-vault" 'run-slash
            :catches 'recall-vault-error
            :description "The money rules: read them, change a cap, try a payment")
  (:help :recall-vault
         "recall-vault: money rules the agent is held to before it pays anything - caps, an allowlist, an approval and a ledger; /recall-vault"
         +primer+))

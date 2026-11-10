;;;; store.lisp --- the two JSON files a vault keeps, and nothing else.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original kept its state in the Hermes plugin data directory as
;;;; policy.json and ledger.json. This keeps the same two files beside each
;;;; other under the home: the section names policy.json, and the ledger sits
;;;; next to it. No network, no subprocess, no credential, no other app's
;;;; file: what is written here is what a `recall-vault' folder holds.
;;;;
;;;; policy.json
;;;;   {"format": 1,
;;;;    "policy": {"currency": "NEAR", "per_tx_cap": 1, "daily_cap": 2,
;;;;               "confirm_above": 0.5, "allowlist": ["addr", ...]},
;;;;    "rules": ["...", ...],
;;;;    "spent_today": 0.0,
;;;;    "spent_day": "YYYY-MM-DD"}
;;;; ledger.json
;;;;   {"format": 1, "entries": [ {...}, ... ]}   oldest first, capped

(in-package #:nodecode-recall-vault)

(defparameter +store-format+ 1
  "The format stamp both files carry, so a later rewrite can read an old one.")

;;; --- where they live -------------------------------------------------------------

(defun store-pathname ()
  "The policy file the section names, under the home unless it is absolute."
  (let* ((name (nlk:trimmed (princ-to-string (or (setting :store-file) ""))))
         (name (if (zerop (length name)) "recall-vault/policy.json" name)))
    (if (or (uiop:string-prefix-p "/" name) (uiop:string-prefix-p "~/" name))
        (nlk:expand-home name)
        (nlk:home name))))

(defun ledger-pathname ()
  "The ledger, beside the policy file the section names."
  (merge-pathnames (make-pathname :name "ledger" :type "json")
                   (store-pathname)))

;;; --- reading and writing ----------------------------------------------------------

(defun today ()
  "The UTC day, as the file records it."
  (subseq (nlk:iso-now) 0 10))

(defun read-json-file (path)
  "PATH decoded, or NIL when it is absent or unreadable: a vault is never
refused on the request path for a file it can start again from."
  (let ((object (and (probe-file path)
                     (ignore-errors (nlk:decode-json (nlk:read-text path))))))
    (and (hash-table-p object) object)))

(defun write-json-file (path object)
  "OBJECT to PATH, atomically: a temp file, then a rename. => PATH."
  (ensure-directories-exist path)
  (let ((tmp (merge-pathnames (make-pathname :name (format nil "~a.tmp" (pathname-name path))
                                              :type (pathname-type path))
                               path)))
    (with-open-file (out tmp :direction :output :if-exists :supersede
                              :external-format :utf-8)
      (write-string (nlk:encode-json-object object) out))
    (rename-file tmp path)
    path))

;;; --- the policy ------------------------------------------------------------------

(defun default-state ()
  "The state a vault starts from, before the operator writes anything."
  (list :policy (default-policy)
        :rules (list "Never spend more than the daily cap."
                     "Never pay a brand-new address without asking first.")
        :spent-today 0.0d0
        :spent-day (today)))

(defun json-allowlist (object)
  "OBJECT's allowlist as a list of strings, or the default when it carries none."
  (let ((items (and object (nlk:json-array object "allowlist"))))
    (if (and items (vectorp items))
        (loop for item across items when (stringp item) collect item)
        (copy-list *default-allowlist*))))

(defun policy-from-json (object)
  "OBJECT as the policy plist, every member defaulted."
  (let ((base (default-policy)))
    (when (hash-table-p object)
      (setf (getf base :per-tx-cap) (or (amount-value (nlk:json-value object :number "per_tx_cap"))
                                        (getf base :per-tx-cap))
            (getf base :daily-cap) (or (amount-value (nlk:json-value object :number "daily_cap"))
                                        (getf base :daily-cap))
            (getf base :confirm-above) (or (amount-value (nlk:json-value object :number "confirm_above"))
                                            (getf base :confirm-above)))
      (when (nlk:json-value object :array "allowlist")
        (setf (getf base :allowlist) (json-allowlist object))))
    base))

(defun policy-json (policy)
  "POLICY as the JSON object the policy file keeps."
  (nlk:make-json-object "currency" (or (getf policy :currency) *currency*)
                         "per_tx_cap" (coerce (getf policy :per-tx-cap) 'double-float)
                         "daily_cap" (coerce (getf policy :daily-cap) 'double-float)
                         "confirm_above" (coerce (getf policy :confirm-above) 'double-float)
                         "allowlist" (apply #'vector (getf policy :allowlist))))

(defun load-state ()
  "The state the files hold, spent-today reset when the day has turned.
=> (:policy ... :rules (...) :spent-today N :spent-day \"YYYY-MM-DD\")"
  (let* ((path (store-pathname))
         (object (read-json-file path))
         (state (default-state)))
    (when object
      (let ((policy (nlk:json-value object :object "policy")))
        (when policy (setf (getf state :policy) (policy-from-json policy))))
      (let ((rules (nlk:json-array object "rules")))
        (when (and rules (vectorp rules))
          (setf (getf state :rules)
                (loop for rule across rules when (stringp rule) collect rule))))
      (setf (getf state :spent-today)
            (or (amount-value (nlk:json-value object :number "spent_today")) 0.0d0))
      (let ((day (nlk:json-value object :string "spent_day")))
        (when day (setf (getf state :spent-day) day))))
    (when (not (equal (getf state :spent-day) (today)))
      (setf (getf state :spent-today) 0.0d0
            (getf state :spent-day) (today))
      (save-state state))
    state))

(defun save-state (state)
  "STATE to the policy file. => STATE."
  (write-json-file (store-pathname)
                    (nlk:make-json-object
                     "format" +store-format+
                     "policy" (policy-json (getf state :policy))
                     "rules" (apply #'vector (getf state :rules))
                     "spent_today" (coerce (getf state :spent-today) 'double-float)
                     "spent_day" (getf state :spent-day)))
  state)

;;; --- the ledger ------------------------------------------------------------------

(defun entry-json (entry)
  "ENTRY as the JSON object the ledger keeps."
  (nlk:make-json-object "decision" (getf entry :decision)
                         "rule" (or (getf entry :rule) :null)
                         "reason" (or (getf entry :reason) "")
                         "amount" (coerce (or (getf entry :amount) 0.0d0) 'double-float)
                         "to" (or (getf entry :to) "")
                         "tool" (or (getf entry :tool) :null)
                         "currency" (or (getf entry :currency) *currency*)
                         "at" (or (getf entry :at) (nlk:iso-now))))

(defun entry-from-json (object)
  "OBJECT as the entry plist a listing hands back."
  (list :decision (or (nlk:json-value object :string "decision") "")
        :rule (nlk:json-value object :string "rule")
        :reason (or (nlk:json-value object :string "reason") "")
        :amount (or (amount-value (nlk:json-value object :number "amount")) 0.0d0)
        :to (or (nlk:json-value object :string "to") "")
        :tool (nlk:json-value object :string "tool")
        :currency (or (nlk:json-value object :string "currency") *currency*)
        :at (or (nlk:json-value object :string "at") "")))

(defun load-ledger ()
  "Every entry the ledger holds, oldest first."
  (let* ((object (read-json-file (ledger-pathname)))
         (items (and object (nlk:json-array object "entries"))))
    (if (vectorp items)
        (loop for item across items when (hash-table-p item) collect (entry-from-json item))
        '())))

(defun save-ledger (entries)
  "ENTRIES to the ledger file, the oldest dropped past the section's cap."
  (let* ((cap (or (ignore-errors (setting :max-ledger)) 200))
         (kept (last entries (min cap (length entries)))))
    (write-json-file (ledger-pathname)
                      (nlk:make-json-object
                       "format" +store-format+
                       "entries" (apply #'vector (mapcar #'entry-json kept))))
    kept))

(defun record-entry (entry)
  "Append ENTRY to the ledger. => the entry written."
  (let ((record (append entry (unless (getf entry :at) (list :at (nlk:iso-now))))))
    (save-ledger (append (load-ledger) (list record)))
    record))

(defun record-allowed (amount to-address &optional tool)
  "Book an allowed payment of AMOUNT to TO-ADDRESS: spent today, then the ledger."
  (let* ((state (load-state))
         (spent (+ (coerce (or (getf state :spent-today) 0.0d0) 'double-float)
                   (coerce amount 'double-float))))
    (setf (getf state :spent-today) spent)
    (save-state state)
    (record-entry (list :decision "ALLOWED"
                        :rule nil
                        :reason "Passes all spending rules."
                        :amount amount
                        :to to-address
                        :tool tool
                        :spent-today-after spent))))

(defun remember-rule (text)
  "Keep TEXT as a rule of the operator's. => the number of rules kept."
  (let* ((text (rule-line text))
         (state (load-state)))
    (cond ((zerop (length text)) nil)
          ((member text (getf state :rules) :test #'string-equal)
           (length (getf state :rules)))
          (t (setf (getf state :rules) (append (getf state :rules) (list text)))
             (save-state state)
             (length (getf state :rules))))))

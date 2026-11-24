;;;; store.lisp --- the local ledger: one JSON line a receipt, salt and all.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Under the home, invinoveritas-receipts/invinoveritas-receipts.jsonl holds what opening a receipt
;;;; later needs: the signed event, the receipt, the salt the service returned,
;;;; the choice and the context that was hashed. Nothing else is written, and
;;;; nothing here is sent anywhere: (invinoveritas-receipts:check) reads this file alone.

(in-package #:nodecode-invinoveritas-receipts)

(defun store-path ()
  "Where kept receipts live, under the home: one JSON object a line."
  (nlk:home "invinoveritas-receipts/invinoveritas-receipts.jsonl"))

(defun remember (record)
  "Append RECORD to the ledger and answer it."
  (nlk:append-json-line (store-path) record)
  record)

(defun kept ()
  "Every kept receipt, oldest first."
  (nlk:read-json-lines (store-path)))

(defun text-field (object key)
  "OBJECT's member KEY as a string, or NIL."
  (and (hash-table-p object) (nlk:json-value object :string key)))

(defun newest-kept ()
  "The newest kept receipts, newest first, at most `listed' of them."
  (let* ((all (kept))
         (limit (max 1 (or (setting :listed) 10))))
    (reverse (last all (min limit (length all))))))

(defun kept-record (id)
  "The kept receipt ID names: an event id, its first characters, or NIL (and
`last') for the newest. A receipt never kept is a refusal naming the file."
  (let* ((all (kept))
         (wanted (nlk:trimmed (or id "")))
         (newest (car (last all))))
    (cond
      ((or (zerop (length wanted)) (string-equal wanted "last"))
       (or newest
           (fail "no receipt is kept in ~a yet: ask for one with the issue_receipt tool"
                 (store-path))))
      (t
       (or (loop for record in (reverse all)
                 for kept-id = (text-field record "id")
                 when (and kept-id
                           (<= (length wanted) (length kept-id))
                           (string-equal wanted (subseq kept-id 0 (length wanted))))
                   return record)
           (fail "no kept receipt starts with ~s in ~a" wanted (store-path)))))))

;;;; state.lisp --- the purse's own state: kill switch, quote cache, ledger, grants.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One JSON file under the home, `purse/state.json`, written atomically
;;;; after every change, 0600: the kill switch, the last quotes seen, the
;;;; integer cents each session captured, the one-time approval grants, and
;;;; what the server said about each merchant it listed. The API key is not
;;;; here; it is read from the section at every call.
;;;;
;;;; The plugin kept the same four things in Hermes plugin state; the shape
;;;; is one object under string keys, so a state file from an older rewrite
;;;; still reads.

(in-package #:nodecode-agentpurse)

(defparameter *state* nil
  "The loaded state object while the cell runs, NIL after its stop.")

(defparameter +grant-ttl-seconds+ 600
  "How long an approval grant can be spent after it is proposed.")

(defun state-path ()
  "Where the purse keeps its state: under this home, not in the cell folder."
  (nlk:home "purse/state.json"))

(defun empty-state ()
  (nlk:make-json-object "killed" "off" "quotes" (nlk:make-json-object)
                         "ledger" (nlk:make-json-object)
                         "grants" (nlk:make-json-object)
                         "merchants" (nlk:make-json-object)))

(defun load-state ()
  "The state file as an object, or an empty one."
  (let ((path (state-path)))
    (or (and (probe-file path)
             (let ((text (ignore-errors (uiop:read-file-string path))))
               (let ((object (and text (ignore-errors (nlk:decode-json text)))))
                 (and (hash-table-p object) object))))
        (empty-state))))

(defun save-state ()
  "Write the whole state atomically. Nothing is written without a state."
  (when *state*
    (nlk:write-file-atomically (state-path) (nlk:encode-json-object *state*)
                                :mode #o600 :directory-mode #o700)
    (values)))

(defmacro with-state (&body body)
  "BODY inside the purse's lock, against the loaded state, saved after."
  `(with-agentpurse-lock
     (prog1 (progn ,@body) (save-state))))

(defun state-object (key)
  "The object KEY holds, made and installed when it is absent."
  (let ((value (and *state* (gethash key *state*))))
    (if (hash-table-p value)
        value
        (setf (gethash key *state*) (nlk:make-json-object)))))

(defun state-value (key)
  (and *state* (nlk:json-value *state* :string key)))

(defun killed-p () (equal "on" (state-value "killed")))
(defun set-killed (on) (setf (gethash "killed" *state*) (if on "on" "off")))

;;; --- quotes -------------------------------------------------------------------

(defun remember-quote (quote)
  "Keep QUOTE (a plist :quote-id :merchant :amount-cents :guard :guard-detail
:description) in the cache, newest 40 kept."
  (let* ((quotes (state-object "quotes"))
         (id (getf quote :quote-id)))
    (when (and (stringp id) (plusp (length id)))
      (setf (gethash id quotes)
            (nlk:make-json-object "merchant" (getf quote :merchant)
                                   "amount_cents" (getf quote :amount-cents)
                                   "guard" (getf quote :guard)
                                   "guard_detail" (or (getf quote :guard-detail) "")
                                   "description" (or (getf quote :description) "")
                                   "stored_at" (get-universal-time)))
      (when (> (hash-table-count quotes) 40)
        (dotimes (i (- (hash-table-count quotes) 40))
          (let ((oldest (reduce (lambda (a b)
                                  (if (< (or (nlk:json-value (cdr a) :integer "stored_at") 0)
                                         (or (nlk:json-value (cdr b) :integer "stored_at") 0))
                                      a b))
                                (loop for key being the hash-keys of quotes
                                      using (hash-value value) collect (cons key value)))))
            (remhash (car oldest) quotes))))
      (values))))

(defun cached-quote (quote-id)
  "The quote cache's row for QUOTE-ID as a plist, or NIL."
  (let ((row (and *state* (nlk:json-value (state-object "quotes") :object quote-id))))
    (when row
      (list :quote-id quote-id
            :merchant (nlk:json-value row :string "merchant")
            :amount-cents (nlk:json-value row :integer "amount_cents")
            :guard (or (nlk:json-value row :string "guard") "unknown")
            :guard-detail (or (nlk:json-value row :string "guard_detail") "")
            :description (or (nlk:json-value row :string "description") "")))))

;;; --- merchants ------------------------------------------------------------------

(defun remember-merchants (rows)
  "Keep what the server said about each merchant in ROWS."
  (let ((known (state-object "merchants")))
    (loop for row across (if (vectorp rows) rows #())
          when (hash-table-p row)
            do (let ((name (nlk:json-value row :text "name")))
                 (when name
                   (setf (gethash (string-downcase (nlk:trimmed name)) known)
                         (nlk:make-json-object
                          "name" (nlk:trimmed name)
                          "blocked" (if (nlk:json-value row :any "blocked") "yes" "no")
                          "trusted" (if (nlk:json-value row :any "trusted") "yes" "no"))))))
    (values)))

(defun cached-merchant (name)
  "What the cache says about NAME, or NIL."
  (and name *state*
       (nlk:json-value (state-object "merchants") :object
                       (string-downcase (nlk:trimmed name)))))

(defun merchant-blocked-p (row)
  (and row (equal "yes" (nlk:json-value row :string "blocked"))))

;;; --- the per-session ledger -------------------------------------------------------

(defun spent-cents (session)
  "The integer cents SESSION captured, or NIL when the ledger row is broken."
  (let ((row (and *state* (nlk:json-value (state-object "ledger") :object session))))
    (cond ((null row) 0)
          ((not (hash-table-p row)) nil)
          (t (let ((cents (nlk:json-value row :integer "spent_cents")))
               (and (integerp cents) (not (minusp cents)) cents))))))

(defun record-purchase (session &key amount-cents merchant intent-id order-id)
  "Add a captured payment to SESSION's row once, by INTENT-ID. => cents spent."
  (let* ((ledger (state-object "ledger"))
         (row (or (nlk:json-value ledger :object session)
                  (nlk:make-json-object "spent_cents" 0
                                         "purchases" (coerce '() 'vector))))
         (purchases (let ((found (nlk:json-value row :array "purchases")))
                      (coerce (if (and found (vectorp found)) found #()) 'list))))
    (unless (some (lambda (item) (and (hash-table-p item)
                                      (equal intent-id (nlk:json-value item :string "intent_id"))))
                  purchases)
      (let* ((purchases (append purchases
                                (list (nlk:make-json-object "intent_id" intent-id
                                                            "merchant" (or merchant "")
                                                            "amount_cents" amount-cents
                                                            "order_id" (or order-id "")))))
             (kept (last purchases (min (length purchases) 20)))
             (spent (+ (or (spent-cents session) 0) amount-cents)))
        (setf (gethash session ledger)
              (nlk:make-json-object "spent_cents" spent
                                     "purchases" (coerce kept 'vector)))))
    (spent-cents session)))

;;; --- the one-time grants ----------------------------------------------------------

(defun fresh-p (row now)
  (let ((issued (and (hash-table-p row) (nlk:json-value row :integer "issued_at"))))
    (and issued (not (minusp (- now issued))) (<= (- now issued) +grant-ttl-seconds+))))

(defun prune-grants ()
  "Drop every grant past its ttl."
  (let ((grants (state-object "grants")) (now (get-universal-time)))
    (loop for key being the hash-keys of grants
          when (not (fresh-p (gethash key grants) now)) do (remhash key grants))
    (when (> (hash-table-count grants) 8)
      (let ((oldest (reduce (lambda (a b)
                              (if (< (or (nlk:json-value (cdr a) :integer "issued_at") 0)
                                     (or (nlk:json-value (cdr b) :integer "issued_at") 0))
                                  a b))
                            (loop for key being the hash-keys of grants
                                  using (hash-value value) collect (cons key value)))))
        (remhash (car oldest) grants)))
    grants))

(defun issue-grant (preview session tool)
  "Store the proposed grant for PREVIEW in SESSION: it can spend only once a
person approved it. => its key."
  (let* ((grants (prune-grants))
         (key (nlk:sha256-text (getf preview :fingerprint)))
         (key (subseq key 0 (min 24 (length key)))))
    (setf (gethash key grants)
          (nlk:make-json-object
           "fingerprint" (getf preview :fingerprint)
           "session" session
           "tool" tool
           "amount_cents" (getf preview :amount-cents)
           "merchant" (getf preview :merchant)
           "quote_id" (or (getf preview :quote-id) "")
           "issued_at" (get-universal-time)
           "human" "no"))
    (save-state)
    key))

(defun approved-grant-p (session fingerprint tool)
  "Whether a fresh grant a person approved already waits for this payment."
  (some (lambda (row)
          (let ((object (cdr row)))
            (and (equal fingerprint (nlk:json-value object :string "fingerprint"))
                 (equal tool (nlk:json-value object :string "tool"))
                 (equal "yes" (nlk:json-value object :string "human")))))
        (pending-grants session)))

(defun pending-grants (session)
  "SESSION's fresh grants, newest first, as (KEY . ROW)."
  (let ((grants (state-object "grants")) (now (get-universal-time)) (rows '()))
    (loop for key being the hash-keys of grants using (hash-value row)
          when (and (hash-table-p row)
                    (equal session (nlk:json-value row :string "session"))
                    (fresh-p row now))
            do (push (cons key row) rows))
    (sort rows #'> :key (lambda (row) (or (nlk:json-value (cdr row) :integer "issued_at") 0)))))

(defun mark-human (session)
  "Mark every pending grant of SESSION human-approved. => how many."
  (let ((rows (pending-grants session)))
    (dolist (row rows)
      (setf (gethash "human" (cdr row)) "yes"))
    (when rows (save-state))
    (length rows)))

(defun clear-grants (session)
  (let ((grants (state-object "grants")) (count 0))
    (loop for key being the hash-keys of grants
          when (equal session (nlk:json-value (gethash key grants) :string "session"))
            do (remhash key grants) (incf count))
    (when (plusp count) (save-state))
    count))

(defun take-grant (session fingerprint tool)
  "Consume the one-time grant matching SESSION, FINGERPRINT and TOOL.
=> the row, or (values NIL reason). A grant no person approved never spends."
  (let ((match nil))
    (dolist (row (pending-grants session))
      (let ((object (cdr row)))
        (when (and (equal fingerprint (nlk:json-value object :string "fingerprint"))
                   (equal tool (nlk:json-value object :string "tool")))
          (when (or (null match) (equal "yes" (nlk:json-value object :string "human")))
            (setf match row)))))
    (if (null match)
        (values nil "no person approved this payment, so it was not sent: ask again, and approve \
the prompt or /purse approve it")
        (let ((row (cdr match)))
          (remhash (car match) (state-object "grants"))
          (save-state)
          (cond ((not (equal "yes" (nlk:json-value row :string "human")))
                 (values nil "the approval for this payment was never given by a person, so it was \
not sent"))
                ((not (fresh-p row (get-universal-time)))
                 (values nil "the approval expired before the payment was sent: ask again"))
                (t row))))))

;;;; policy.lisp --- the money rules as pure functions.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; No clock, no file, no image state: the same rules the Hermes plugin's
;;;; policy.py keeps, checked in the same order, so a decision reads the same
;;;; way whoever asks for it.
;;;;
;;;; A policy is a plist:
;;;;   (:currency "NEAR" :per-tx-cap 1.0 :daily-cap 2.0
;;;;    :confirm-above 0.5 :allowlist ("addr" ...))
;;;; and a check answers one of
;;;;   (:decision "ALLOWED"   :rule nil              :reason "...")
;;;;   (:decision "BLOCKED"   :rule "perTxCap"       :reason "...")
;;;;   (:decision "NEEDS_CONFIRMATION" :rule "confirmThreshold" :reason "...")
;;;;
;;;; Check order, unchanged from the original: an unusable amount, an empty
;;;; allowlist (nobody is allowed), the per-transaction cap, the daily cap,
;;;; the allowlist, then the confirm threshold. A missing allowlist falls
;;;; back to the default one; an explicitly empty allowlist allows nobody.

(in-package #:nodecode-recall-vault)

(defparameter *currency* "NEAR"
  "The one currency this version holds rules in.")

(defparameter *default-per-tx-cap* 1.0d0
  "NEAR allowed in one payment until the operator says otherwise.")

(defparameter *default-daily-cap* 2.0d0
  "NEAR allowed in one day until the operator says otherwise.")

(defparameter *default-confirm-above* 0.5d0
  "A payment above this many NEAR is put to the operator before it runs.")

(defparameter *default-allowlist* '("recallwallet-freelancer.testnet")
  "The recipients a fresh vault allows: the demo address the original shipped
with. An allowlist the operator writes replaces it whole, and an empty one
allows nobody.")

(defun default-policy ()
  "A fresh copy of the policy a vault starts life with."
  (list :currency *currency*
        :per-tx-cap *default-per-tx-cap*
        :daily-cap *default-daily-cap*
        :confirm-above *default-confirm-above*
        :allowlist (copy-list *default-allowlist*)))

;;; --- amounts -------------------------------------------------------------------

(defun amount-value (value)
  "VALUE as a finite double-float, or NIL when it is no amount at all."
  (let ((number (cond ((numberp value) value)
                      ((stringp value)
                       (ignore-errors
                         (let ((*read-eval* nil))
                           (multiple-value-bind (parsed end)
                               (read-from-string (nlk:trimmed value) nil nil)
                             (and (numberp parsed)
                                  (= end (length (nlk:trimmed value)))
                                  parsed)))))
                      (t nil))))
    (when (numberp number)
      (coerce number 'double-float))))

(defun money-text (value)
  "VALUE as the shortest decimal that says it: 1, 0.5, 0.25."
  (let* ((text (format nil "~,4f" (coerce (or value 0) 'double-float)))
         (trimmed (string-right-trim "0" text))
         (trimmed (if (char= #\. (char trimmed (1- (length trimmed))))
                      (subseq trimmed 0 (1- (length trimmed)))
                      trimmed)))
    (if (zerop (length trimmed)) "0" trimmed)))

;;; --- the check ------------------------------------------------------------------

(defun check-payment (policy amount to-address spent-today)
  "POLICY, AMOUNT and TO-ADDRESS against SPENT-TODAY. => the decision plist."
  (let* ((amount (amount-value amount))
         (per-tx (or (amount-value (getf policy :per-tx-cap)) *default-per-tx-cap*))
         (daily (or (amount-value (getf policy :daily-cap)) *default-daily-cap*))
         (confirm-above (or (amount-value (getf policy :confirm-above))
                            *default-confirm-above*))
         ;; an empty allowlist is a state of its own: nobody is allowed.
         ;; Only a policy that carries no allowlist member at all falls back
         ;; to the default one.
         (allowlist (if (member :allowlist policy)
                        (getf policy :allowlist)
                        (copy-list *default-allowlist*)))
         (spent (or (amount-value spent-today) 0.0d0))
         (to (nlk:trimmed (princ-to-string (or to-address "")))))
    (cond
      ((null amount)
       (list :decision "BLOCKED" :rule "invalidAmount"
             :reason "Amount is not a number."))
      ((or (sb-ext:float-nan-p amount) (sb-ext:float-infinity-p amount))
       (list :decision "BLOCKED" :rule "invalidAmount"
             :reason "Amount is not a finite number."))
      ((<= amount 0)
       (list :decision "BLOCKED" :rule "invalidAmount"
             :reason "Amount must be greater than zero."))
      ((and (listp allowlist) (null allowlist))
       (list :decision "BLOCKED" :rule "allowlist"
             :reason "Allowlist is empty, so nobody is allowed. Add an address to the allowlist before paying."))
      ((> amount per-tx)
       (list :decision "BLOCKED" :rule "perTxCap"
             :reason (format nil "Amount ~a NEAR exceeds the per-transaction cap of ~a NEAR."
                             (money-text amount) (money-text per-tx))))
      ((> (+ spent amount) daily)
       (list :decision "BLOCKED" :rule "dailyCap"
             :reason (format nil "This would take today spending to ~a NEAR, over the daily cap of ~a NEAR."
                             (money-text (+ spent amount)) (money-text daily))))
      ((not (member to allowlist :test #'string-equal))
       (list :decision "BLOCKED" :rule "allowlist"
             :reason (format nil "Address ~a is not on your allowlist, and new addresses need your approval first."
                             to)))
      ((> amount confirm-above)
       (list :decision "NEEDS_CONFIRMATION" :rule "confirmThreshold"
             :reason (format nil "Amount ~a NEAR is above your ~a NEAR confirmation threshold, so the operator is asked before it runs."
                             (money-text amount) (money-text confirm-above))))
      (t
       (list :decision "ALLOWED" :rule nil
             :reason "Passes all spending rules.")))))

;;; --- the rules a person wrote ---------------------------------------------------------

(defun rule-line (rule)
  "RULE as one line, trimmed: what the model and the ledger show."
  (nlk:trimmed (princ-to-string (or rule ""))))

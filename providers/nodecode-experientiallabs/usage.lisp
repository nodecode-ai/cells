;;;; usage.lisp --- /explabs: the wallet balance and the daily key cap.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of kyoto-aircon's hermes-experientiallabs (see NOTICE), whose
;;;; account_usage.py backs Hermes' /usage view from the gateway's Cost API:
;;;;
;;;;   GET {cost_api_base}/credits  -> {data: {total_credits, total_usage}} in USD
;;;;   GET {cost_api_base}/key      -> the presented key's limit / limit_remaining,
;;;;                                    a daily USD cap that resets at 00:00 UTC
;;;;
;;;; Nodecode has no per-provider usage view, so the same two reads answer a
;;;; slash command instead. Every read fails open, as the original insists
;;;; (account_usage.py:231-236): a command never signals the operator's shell.

(in-package #:nodecode-experientiallabs)

(defun get-json (url key timeout)
  "The JSON URL answers to one bearer GET, or NIL on any failure: a status
the gateway refused, a body that is not JSON, a dial that ran out of time."
  (handler-case
      (multiple-value-bind (body status)
          (nlk:http :get url
                    :headers (list (cons "authorization" (format nil "Bearer ~a" key))
                                   (cons "accept" "application/json"))
                    :timeout timeout)
        (when (and (stringp body) (integerp status) (< status 300))
          (nlk:decode-json body)))
    (error () nil)))

(defun data-of (payload)
  "PAYLOAD's `data' object, or PAYLOAD itself when the body is bare."
  (or (nlk:json-value payload :object "data") payload))

(defun money (value)
  "VALUE, a real, as dollars and cents."
  (format nil "$~,2f" (coerce value 'double-float)))

(defun wallet-line (payload)
  "The wallet window from a /credits body, or NIL when it names no money."
  (let* ((data (data-of payload))
         (total (nlk:json-value data :number "total_credits"))
         (used (nlk:json-value data :number "total_usage")))
    (when (and total used)
      (format nil "wallet: ~a remaining of ~a (~a used)"
              (money (max 0 (- total used))) (money total) (money used)))))

(defun cap-line (payload)
  "The daily cap window from a /key body: the cap and what is left of it, or
that the key carries none. NIL only when the body is unreadable."
  (let* ((data (data-of payload))
         (limit (nlk:json-value data :number "limit"))
         (remaining (nlk:json-value data :number "limit_remaining")))
    (cond ((null data) nil)
          ((or (null limit) (null remaining))
           (if (and limit (not (plusp limit))) "daily key cap: $0.00, a hard cap"
               "daily key cap: none set"))
          (t (format nil "daily key cap: ~a of ~a used, resets at 00:00 UTC"
                     (money (max 0 (- limit remaining))) (money limit))))))

(defun saved-key ()
  "The key the chain resolves for this provider, or NIL when it resolves none
(the `public' placeholder, __init__.py:117's variables, auth.json first)."
  (handler-case
      (let ((credential (nle::resolve-provider-credential +provider+)))
        (unless (eq :public (nle:credential-source credential))
          (nle:credential-key credential)))
    (error () nil)))

(defun usage-text ()
  "What /explabs answers: the two windows, or what to do about a missing key."
  (let ((key (saved-key)))
    (if (null key)
        (format nil "experientiallabs: no key saved; /connect experientiallabs, or set EXPLABS_API_KEY (~a)"
                +key-page+)
        (let* ((timeout (setting :usage-timeout-seconds))
               (base (string-right-trim "/" (setting :cost-api-base)))
               (credits (get-json (concatenate 'string base "/credits") key timeout))
               (key-body (get-json (concatenate 'string base "/key") key timeout))
               (lines (remove nil (list (and credits (wallet-line credits))
                                        (and key-body (cap-line key-body))))))
          (if lines
              (format nil "Experiential Labs~%~{  ~a~^~%~}~%" lines)
              "experientiallabs: the Cost API did not answer; the gateway's own status page has the numbers")))))

(defun run-command (args session-id)
  "/explabs, typed alone or with anything after it, answers the usage report."
  (declare (ignore args session-id))
  (usage-text))

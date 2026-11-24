;;;; client.lisp --- the two calls to invinoveritas, and the key they use.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; POST /decision-receipt signs one decision (authorized with the key, paid
;;;; per call). POST /verify-proof checks a signature and is free and
;;;; unauthenticated. Nothing else is called, and no call carries the command,
;;;; the session name or anything else in plain text: the body holds the
;;;; question, the options, the choice and one salted hash.
;;;;
;;;; A call that fails is a INVINOVERITAS-RECEIPTS-ERROR naming the URL and what came back;
;;;; it never takes a session down, because the tool answers what the operator
;;;; reads, not an exception.

(in-package #:nodecode-invinoveritas-receipts)

(defparameter +user-agent+ "nodecode-invinoveritas-receipts/0.1.0"
  "What the service sees this cell as.")

(defun base-url ()
  "The invinoveritas API base, without its trailing slash."
  (string-right-trim "/" (setting :api-url)))

(defun verify-hint ()
  "Where a kept receipt is checked, in words, for the record and the answer."
  (format nil *verify-note-format* (base-url)))

(defun api-key ()
  "The invinoveritas key this machine holds: the environment variable the
section names, or NIL. The cell reads no other credential."
  (let ((name (setting :api-key-env)))
    (and name (nle::credential-env name))))

(defun short-text (text &optional (limit 200))
  "TEXT cut to LIMIT characters: enough of an answer to say what came back."
  (let ((text (if (stringp text) text (princ-to-string text))))
    (if (> (length text) limit) (subseq text 0 limit) text)))

(defun post-json (url body &key auth)
  "POST BODY, canonical JSON text, to URL. => the decoded answer.
AUTH, when given, rides as the bearer token."
  (let* ((headers (append (list (cons "content-type" "application/json")
                                (cons "user-agent" +user-agent+))
                          (when auth
                            (list (cons "authorization" (format nil "Bearer ~a" auth))))))
         (timeout (setting :timeout-seconds))
         (text (handler-case
                   (multiple-value-bind (answer status)
                       (nlk:http :post url :headers headers :content body :timeout timeout)
                     (unless (<= 200 status 299)
                       (fail "~a answered HTTP ~d~@[: ~a~]" url status (short-text answer)))
                     answer)
                 (invinoveritas-receipts-error (condition) (error condition))
                 (error (condition)
                   (fail "~a could not be reached: ~a" url (short-text condition 160))))))
    (or (ignore-errors (nlk:decode-json text))
        (fail "~a answered something that is not JSON: ~a" url (short-text text)))))

(defun issue-request (body)
  "Ask for a receipt for BODY. => the service's answer, decoded, holding the
signed event, the receipt and the salt."
  (post-json (format nil "~a/decision-receipt" (base-url)) (canonical-text body)
             :auth (api-key)))

(defun verify-request (event)
  "Ask the free endpoint whether EVENT's signature holds. => its answer."
  (post-json (format nil "~a/verify-proof" (base-url))
             (canonical-text (jobject "event" event))))

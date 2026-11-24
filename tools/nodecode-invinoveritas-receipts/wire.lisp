;;;; wire.lisp --- what a receipt commits to, and the bytes it commits with.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A receipt is a signed, salted commitment to one decision, issued by
;;;; invinoveritas POST /decision-receipt. Only the SHA-256 of the decision
;;;; context leaves this machine: what is being decided and the session it was
;;;; decided in are hashed here, beside a 32-byte nonce generated here, and the
;;;; service returns the salt to this machine alone. The salt is kept next to
;;;; the receipt, so the holder can later open it to anyone; nobody else can
;;;; recover the choice from the public receipt.
;;;;
;;;; Canonical JSON is the whole contract of the hash: keys sorted, no space
;;;; between them, UTF-8 as it stands. The service recomputes it when a receipt
;;;; is opened, so this file writes that one dialect and nothing else.

(in-package #:nodecode-invinoveritas-receipts)

(defparameter +question+ "nodecode decision: commit to the choice made here?"
  "The question a receipt answers when the caller names none.")

(defparameter +options+ '("approve" "deny")
  "The choices a receipt offers when the caller names none.")

(defparameter *verify-note-format* "POST the event to ~a/verify-proof (free, no auth)"
  "What a kept receipt says about checking it, over the API base.")

;;; --- the hash ------------------------------------------------------------

(defun hex (bytes)
  "BYTES, an octet vector, as lowercase hex."
  (format nil "~{~2,'0x~}" (coerce bytes 'list)))

(defun new-nonce ()
  "32 bytes from the operating system's entropy source, as hex. The nonce
stays here: without it the hash sent to invinoveritas could be reversed by
guessing a short command, with it nobody but the holder can open the context."
  (hex (nlk:random-bytes 32)))

(defun jobject (&rest pairs)
  "A JSON object, a string-keyed hash table, from PAIRS of key and value."
  (let ((object (make-hash-table :test #'equal)))
    (loop for (key value) on pairs by #'cddr do (setf (gethash key object) value))
    object))

(defun write-json-string (text stream)
  "TEXT as a JSON string literal, UTF-8 as it stands, escaping only what JSON
requires: the quote, the backslash and the control characters."
  (write-char #\" stream)
  (loop for char across text
        for code = (char-code char)
        do (cond ((char= char #\") (write-string "\\\"" stream))
                 ((char= char #\\) (write-string "\\\\" stream))
                 ((char= char #\Newline) (write-string "\\n" stream))
                 ((char= char #\Tab) (write-string "\\t" stream))
                 ((char= char #\Return) (write-string "\\r" stream))
                 ((< code 32) (format stream "\\u~4,'0x" code))
                 (t (write-char char stream))))
  (write-char #\" stream))

(defun canonical-write (value stream)
  "VALUE as canonical JSON: a string, an integer, a hash table (keys sorted),
a vector (an array), or :TRUE, :FALSE and :NULL."
  (typecase value
    (string (write-json-string value stream))
    (integer (format stream "~d" value))
    (hash-table
     (write-char #\{ stream)
     (loop for first = t then nil
           for key in (sort (loop for key being the hash-keys of value collect key)
                            #'string<)
           do (unless first (write-char #\, stream))
              (write-json-string key stream)
              (write-char #\: stream)
              (canonical-write (gethash key value) stream))
     (write-char #\} stream))
    (vector
     (write-char #\[ stream)
     (loop for first = t then nil
           for item across value
           do (unless first (write-char #\, stream))
              (canonical-write item stream))
     (write-char #\] stream))
    (keyword
     (case value
       (:true (write-string "true" stream))
       (:false (write-string "false" stream))
       (:null (write-string "null" stream))
       (t (write-json-string (string-downcase (symbol-name value)) stream))))
    (t (write-json-string (princ-to-string value) stream))))

(defun canonical-text (value)
  "VALUE as canonical JSON in one string: the bytes that get hashed and sent."
  (with-output-to-string (stream)
    (canonical-write value stream)))

;;; --- the decision context ------------------------------------------------

(defun context-object (nonce subject reference session)
  "The context a receipt commits to. It is hashed here and never sent."
  (jobject "nonce" nonce
           "reference" (or reference "")
           "session_key" (or session "")
           "subject" (or subject "")
           "surface" "nodecode"))

(defun digest (text)
  "TEXT's SHA-256 as the 64 lowercase hex characters the service publishes:
NLK:SHA256-TEXT's answer without the tag the organism prefixes its own facts with."
  (let ((hex (nlk:sha256-text text)))
    (if (uiop:string-prefix-p "sha256:" hex) (subseq hex 7) hex)))

(defun context-sha256 (context)
  "The SHA-256 of CONTEXT's canonical JSON, as the service writes it."
  (digest (canonical-text context)))

(defun commitment (salt value)
  "The salted commitment the service publishes: SHA-256 of SALT, a bar, VALUE."
  (digest (format nil "~a|~a" salt value)))

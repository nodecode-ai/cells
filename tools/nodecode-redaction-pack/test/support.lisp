;;;; support.lisp --- the redaction pack's test slice and its helpers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the SAME nodecode.test registry (the core
;;;; DEFTEST, with its hermetic posture) under a REDACTION-PACK-CELL- name
;;;; prefix, so RUN-REDACTION-PACK-TESTS runs exactly that slice. Nothing here
;;;; reaches outside the image or the network: the matcher reads a string, the
;;;; advice runs against a `next' the test writes, and the tool is called the
;;;; way the engine's body calls it.
;;;;
;;;; Every credential is built at run time from a prefix and a deterministic
;;;; fill -- the original's own synthetic-token recipe -- so the repository
;;;; holds no credential-shaped literal.

(in-package #:nodecode.test)

(define-test-slice "redaction-pack" "REDACTION-PACK-CELL-"
  :start nodecode-redaction-pack:start-cell)

(defparameter rp-alnum "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
  "What the formats this slice builds draw their bodies from.")

(defparameter rp-hex "0123456789abcdef"
  "The hexadecimal half of a Cloudflare key or a Shopify token.")

(defparameter rp-urlsafe (concatenate 'string rp-alnum "_-")
  "A base64url body, for a Vault token.")

(defun rp-fill (chars n &optional (seed 0))
  "N characters of CHARS in a fixed, mixed-looking order."
  (let ((out (make-string-output-stream)))
    (dotimes (i n)
      (write-char (char chars (mod (+ (* i 7) (* seed 13) 3) (length chars))) out))
    (get-output-stream-string out)))

(defun rp-vault-token ()
  "A HashiCorp Vault service token: 95 body characters, as gitleaks reads one."
  (concatenate 'string "hvs." (rp-fill rp-urlsafe 95)))

(defun rp-cloudflare-token ()
  "A Cloudflare global API key in the cfk_ format."
  (concatenate 'string "cfk_" (rp-fill rp-alnum 40) (rp-fill rp-hex 8 1)))

(defun rp-shopify-token ()
  "A Shopify access token."
  (concatenate 'string "shpat_" (rp-fill rp-hex 32 2)))

(defun rp-wire-hook ()
  "The cell's advice on the fold, or NIL while it is not started."
  (cell-hook "nodecode-redaction-pack" 'nodecode.evolved::wire-messages-from-events))

(defun rp-message (role content)
  "A folded provider message in the shape the fold answers with."
  (nlk:json-object "role" role "content" content))

(defun rp-tool-call (text &optional style)
  "redact_credentials through the registered tool, the way the engine's body calls it."
  (let ((entry (nle::find-tool "redact_credentials")))
    (funcall (third entry)
             (nodecode-redaction-pack:decode-redact-credentials-request
              (if style
                  (nlk:json-object "text" text "style" style)
                  (nlk:json-object "text" text))))))

(defun rp-slash (args)
  "/redaction-pack ARGS in a session, answering its text."
  (nodecode-redaction-pack::run-slash args "s1"))

(defun rp-wire-answer (messages)
  "The cell's advice run over MESSAGES as the fold would answer them:
(values MASKED CLEAN-P)."
  (funcall (rp-wire-hook)
           (lambda (&rest args) (declare (ignore args)) (values messages :clean))))

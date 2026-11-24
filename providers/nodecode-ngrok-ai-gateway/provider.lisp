;;;; provider.lisp --- what the ngrok AI Gateway is: its address, its key, its verdicts.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from ngrok/hermes-ngrok's model-providers/ngrok (see NOTICE):
;;;; __init__.py's profile (name, aliases, display name, env vars, base URL,
;;;; the live-only catalog) and its GATEWAY_ERROR_RULES, which read the
;;;; gateway's own ERR_NGROK_ codes out of an OpenAI-shaped error body.
;;;;
;;;; The gateway speaks the OpenAI chat wire at https://gateway.ngrok.ai/v1
;;;; with an ngrok access key (`ng-...'), and routes each model id to the
;;;; provider account behind it, so one key serves every model provider the
;;;; operator wired up at ngrok. A model is named `<provider>:<model>' -- the
;;;; gateway's own addressing, sent as it is. The model list is per-account
;;;; gateway configuration, so nothing is bundled: the catalog is the
;;;; gateway's own /models answer.

(in-package #:nodecode-ngrok-ai-gateway)

(defparameter +base+ "https://gateway.ngrok.ai/v1"
  "The hosted gateway: the base the chat lane appends /chat/completions to.")

(defparameter +env+ '("NGROK_AI_GATEWAY_KEY")
  "The environment variables an access key is read from, in order.")

(defparameter +base-env+ "NGROK_AI_GATEWAY_BASE_URL"
  "Where the original module reads a gateway endpoint of the operator's own.")

(defparameter +key-page+ "https://app.ngrok.ai/keys"
  "Where an access key is made.")

(defun base ()
  "The gateway endpoint this box dials: the section's base_url, else
NGROK_AI_GATEWAY_BASE_URL, else the hosted endpoint."
  (or (alexandria:when-let ((value (nlk:trimmed (or (setting :base-url) ""))))
        (and (plusp (length value)) value))
      (nle::credential-env +base-env+)
      +base+))

;;; --- the client it names ---------------------------------------------------------

(defun user-agent ()
  "The client a gateway request names: Nodecode's release, marked as this
cell, the way the original marks its own requests `<product>/<version>
(ngrok-plugin)'. Disclosed, and no other client's name borrowed."
  (format nil "nodecode_cli/~a (ngrok-ai-gateway)" (nle::effective-version)))

(defun client-headers ()
  "The one header every gateway request carries."
  `(("User-Agent" . ,(user-agent))))

;;; --- the gateway's verdicts ------------------------------------------------------
;;;
;;; The gateway's error body is OpenAI-shaped with the ngrok code embedded in
;;; the message (ngrok's AI Gateway error-code reference):
;;;   {"error": {"message": "[ERR_NGROK_27201] Authentication failed: ...",
;;;              "type": "authentication_error", "code": "authentication_error"}}
;;;
;;; Each row is (CODE STATUS LABEL). LABEL is a word of Nodecode's own
;;; failure vocabulary, so the round's failure says what the gateway meant
;;; instead of a bare status: the model is not one the gateway routes
;;; (`not_found'), the key cannot reach it -- an access policy, or no usable
;;; upstream credential (`forbidden'), or the account is out of credit
;;; (`billing'). A code no row names defers to the core's own reading of the
;;; status, which is right for the rest: ERR_NGROK_27201 (401, a bad access
;;; key) is `auth', 27217 (429) is `rate_limit', the 5xx ones are retried.

(defparameter +gateway-errors+
  '(;; the model address does not resolve to anything the gateway routes
    ("err_ngrok_27224" 400 "not_found")   ; Unknown model "X".
    ("err_ngrok_27225" 400 "not_found")   ; Unknown provider "X".
    ("err_ngrok_27229" 400 "not_found")   ; Invalid model address "X".
    ("err_ngrok_27230" 400 "not_found")   ; "X" is not a known model ...
    ;; the access key works but cannot reach this model
    ("err_ngrok_27231" 403 "forbidden")   ; Provider "X" is not allowed by the access policy.
    ("err_ngrok_27232" 403 "forbidden")   ; Model "X" is not allowed by the access policy.
    ("err_ngrok_27202" 502 "forbidden")   ; No keys available for provider "X".
    ("err_ngrok_27211" 401 "forbidden")   ; Upstream provider rejected the API key.
    ("err_ngrok_27233" 400 "forbidden")   ; Model not available with an ngrok-managed key.
    ;; the ngrok account is out of credit
    ("err_ngrok_4026" 403 "billing")      ; Your account has run out of credit.
    ("err_ngrok_27206" 402 "billing"))    ; Credit check failed.
  "The gateway failures this cell names, (CODE STATUS LABEL).")

(defun gateway-code (said)
  "The ERR_NGROK_ code SAID carries, lowercased, or NIL."
  ;; The pattern captures nothing, so SCAN-TO-STRINGS answers the match
  ;; itself; a code past a row's own (ERR_NGROK_272242) is that whole code,
  ;; never the row its prefix names.
  (ppcre:scan-to-strings "err_ngrok_[0-9]+" (string-downcase (or said ""))))

(defun gateway-label (status said)
  "The label the gateway's own code in SAID gives a failure with STATUS, or
NIL when no row names it."
  (let ((code (gateway-code said)))
    (when code
      (loop for (row-code row-status label) in +gateway-errors+
            when (and (equal row-code code) (eql row-status status))
              return label))))

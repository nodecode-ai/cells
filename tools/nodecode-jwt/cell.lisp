;;;; cell.lisp --- jwt_decode, /jwt, and the manual.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of olric3015/hermes-plugins' jwt-ref, a Hermes Agent
;;;; plugin that taught the harness's composer a @jwt: reference: write
;;;; @jwt:<token> in a message and the token's header, its claims and whether
;;;; it is good now are put there instead of the reference. Hermes expands a
;;;; reference as the message goes out; Nodecode has no such seam, and needs
;;;; none: the model calls jwt_decode with the token, and the same description
;;;; comes back as the tool result, before it writes the answer the user reads.
;;;; The decoding itself is decode.lisp's, unchanged in what it answers.

(in-package #:nodecode-jwt)

;;; --- the tool --------------------------------------------------------------------

(nlk:define-contract jwt-arguments
  (:doc "Decode a JSON Web Token and read it: the header's fields, the claims,
when it expires, and what is worth knowing - an alg of none, an empty
signature, a token that points at its own key, a claim name that appears twice.
Call it whenever a token turns up in a log line, a config, an Authorization
header or a pasted string: a token decoded in the head is right about the names
and wrong about the times, and a writer's second copy of a claim stays
invisible. It decodes only - nothing is verified, and no key is needed.")
  ("token" :text :required t
           :doc "the token, whole: header.payload.signature, a 5-part JWE, or an Authorization header's Bearer value"))

(defun current-limits ()
  "The bounds the running cell's section sets, as the description reads them."
  (list :max-token-chars (setting :max-token-chars)
        :max-claims (setting :max-claims)
        :max-value-chars (setting :max-value-chars)
        :skew (setting :skew-seconds)))

(defun describe-or-reason (token)
  "TOKEN's description, or the one line saying why it could not be read: what a
model can act on, rather than a failed call."
  (handler-case (expand-token token :limits (current-limits))
    (jwt-error (failure) (format nil "jwt: ~a" (jwt-error-detail failure)))))

(defun run-decode (arguments)
  "The jwt_decode tool: ARGUMENTS' token described."
  (describe-or-reason (jwt-arguments-token arguments)))

;;; --- /jwt ------------------------------------------------------------------------

(defparameter +usage+
  "jwt: /jwt <token> - the header, the claims, whether the token is good now,
 and what is worth knowing. Nothing is verified: no key, no network, no
 signature check. A Bearer prefix and surrounding quotes are stripped."
  "What /jwt says when it is typed alone.")

(defun run-slash (args session-id)
  "The /jwt command: the token typed after it, described. => the answer's text."
  (declare (ignore session-id))
  (running-settings)
  (let ((token (nlk:trimmed (or args ""))))
    (if (zerop (length token))
        +usage+
        (describe-or-reason token))))

;;; --- the manual ------------------------------------------------------------------

(defparameter +manual+
  "jwt: decodes a JSON Web Token - header.payload.signature, or a 5-part JWE -
and says what is in it: the header's fields and, for each known name, what the
name means (alg as the algorithm it names); the claims, a NumericDate claim
(exp, nbf, iat, auth_time, updated_at) shown as its UTC date and how far off it
is; whether the token is good now (EXPIRED with how long ago, valid for how
much longer, NOT VALID YET when nbf is in the future, a lifetime of exp - iat);
and the notes: a 2-part token, a header with no alg, an alg of none, an empty
signature, a key set or certificate URL the token points at itself, a crit list,
a claim name that appears twice. Nothing is verified - no signature, no key, no
network - so the description says what the token claims, not that a server will
take it. A token with no exp says so. A JWE's payload cannot be read without
the recipient's key, so only its header is shown. A payload that is not a JSON
object is shown as text. The section jwt bounds it: max_token_chars (a longer
token is refused), max_claims (the rest are counted), max_value_chars (one
value's JSON), skew_seconds (the clock difference not worth a warning)."
  "What (help :jwt) answers while the cell runs.")

;;; --- the cell --------------------------------------------------------------------

(nle:define-cell jwt
  (:section ("jwt")
    (:guide "nothing here is required: a token of up to 16,384 characters is decoded, up to 60 claims are listed, and jwt_decode and /jwt answer the same description")
    ("max_token_chars" :integer :default 16384 :min 32
                       :doc "the longest token decoded; a longer one is refused, not cut")
    ("max_claims" :integer :default 60 :min 1
                  :doc "claims listed in the header and in the payload; the rest are counted")
    ("max_value_chars" :integer :default 200 :min 20
                       :doc "characters of one claim's JSON shown before it is cut")
    ("skew_seconds" :integer :default 60 :min 0
                    :doc "seconds of clock difference not worth a nbf or iat warning"))
  (:settings (lambda (values table)
               (declare (ignore table))
               values))
  (:tool "jwt_decode" (nlk:contract 'jwt-arguments) 'run-decode)
  (:command "jwt" 'run-slash
            :description "Decode a JSON Web Token: header, claims, expiry"
            :argument-hint "TOKEN")
  (:help :jwt "jwt: decode a JSON Web Token - /jwt <token>" +manual+))

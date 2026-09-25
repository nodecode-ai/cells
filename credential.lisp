;;;; credential.lisp --- the ChatGPT login, read off the shared auth store.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The ChatGPT backend is not an API key's transport: it wants the OAuth
;;;; access token a `codex login' wrote into auth.json's oauth_tokens section
;;;; as its bearer, it names itself originator codex_cli_rs, it carries the
;;;; account the token belongs to in a chatgpt-account-id header, and it is
;;;; dialled at https://chatgpt.com/backend-api/codex/responses instead of the
;;;; OpenAI API. The store's entry is keyed by provider id — a profile may
;;;; follow a colon (openai:work) — and holds access_token, an id_token, and
;;;; sometimes account_id.
;;;;
;;;; This file is that knowledge and nothing else: one entry, the account id
;;;; behind it (the explicit field, else the https://api.openai.com/auth claim
;;;; of either JWT), the address an openai-family lane's login is served at,
;;;; and the headers the backend wants. The kernel knows none of it: it offers
;;;; the parsed store to the :CREDENTIAL point and applies the attributes the
;;;; answer carries (provider.lisp MAKE-CREDENTIAL, CREDENTIAL-ATTRIBUTE).

(in-package #:nodecode-codex-auth)

(defun oauth-entry (auth provider &aux (tokens (nlk:json-value auth :object "oauth_tokens"))
                                       (matches '()))
  "The oauth_tokens entry of the auth store AUTH that belongs to PROVIDER, or
NIL."
  ;; The exact bare id first; otherwise a scan over profiled entries, where
  ;; more than one match is decisive — CREDENTIAL-ERROR, never a pick (the Zig
  ;; gateway's AmbiguousAuthOAuthToken rule). Expiry is deliberately not
  ;; checked: expires_at is write-only on both sides, and an expired token
  ;; fails at the provider as an ordinary 401.
  (when tokens
    (if (nlk:json-value tokens :text provider "access_token")
        (setf matches (list (gethash provider tokens)))
        (maphash (lambda (key entry &aux (name (or (nlk:json-value entry :string "provider") key)))
                   (when (and (equal provider (subseq name 0 (position #\: name)))
                              (nlk:json-value entry :text "access_token"))
                     (push entry matches)))
                 tokens))
    (when (rest matches)
      (error 'nle:credential-error
             :detail (format nil "multiple OAuth tokens match provider ~s"
                             provider)))
    (first matches)))

(defun jwt-account-id (token)
  "The chatgpt_account_id claim under the https://api.openai.com/auth
namespace of the JWT TOKEN, or NIL."
  ;; Malformed tokens are skipped silently (the auth/root.zig decode posture).
  (let* ((parts (and (stringp token) (uiop:split-string token :separator ".")))
         (text (and (= (length parts) 3) (second parts)))
         ;; base64url, unpadded: cl-base64's URI alphabet, padded with its dots
         (payload (and text
                       (ignore-errors
                         (cl-base64:base64-string-to-string
                          (concatenate 'string text (make-string (mod (- (length text)) 4)
                                                                 :initial-element #\.))
                          :uri t)))))
    (and payload
         (nlk:json-value (ignore-errors (nlk:decode-json payload))
                         :text "https://api.openai.com/auth" "chatgpt_account_id"))))

(defun account-id (entry)
  "The ChatGPT account ENTRY's token belongs to: its explicit account_id, else
the id_token's claim, else the access_token's."
  (or (nlk:json-value entry :text "account_id")
      (jwt-account-id (gethash "id_token" entry))
      (jwt-account-id (gethash "access_token" entry))))

(defun backend-endpoint (configured)
  "The address a saved login is served at, out of the CONFIGURED endpoint:
the ChatGPT backend when CONFIGURED is the OpenAI API's own, else NIL — no
override, so an operator who pointed the provider somewhere else keeps it
(the Options.resolvedUrl rule)."
  (when (and (stringp configured)
             (search "api.openai.com" (string-downcase configured)))
    (setting :endpoint)))

(defun codex-headers (account)
  "The headers the ChatGPT backend requires: the originator it insists on, and
the account the token belongs to when ACCOUNT is known."
  (append '(("originator" . "codex_cli_rs"))
          (when account
            (list (cons "chatgpt-account-id" account)))))

(defun credential (op &aux (provider (getf op :provider))
                           (family (getf op :family))
                           (auth (getf op :auth)))
  "The credential OP's provider resolves from the shared store, or NIL: an
openai-family lane's OAuth token, carrying the ChatGPT backend's address, its
headers and its cache discriminator."
  (nlk:when-let (entry (and provider auth (eq family :openai) (oauth-entry auth provider)))
    (nle:make-credential
     (gethash "access_token" entry) :oauth
     (list :endpoint (backend-endpoint (getf op :endpoint))
           :headers (codex-headers (account-id entry))
           ;; the shard a ChatGPT subscription keeps to itself: the same
           ;; prompt twice is two cache entries, one per transport
           :cache-key "codex_oauth"))))

;;;; cell.lisp --- the cell: one :CREDENTIAL hook.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One contribution: a hook on the kernel's :CREDENTIAL point that answers
;;;; the store's OAuth token for an openai-family lane — carrying the request
;;;; attributes the ChatGPT backend wants — and runs NEXT otherwise. The
;;;; kernel's own ladder is untouched and keeps its order: config, auth.json's
;;;; api_keys, this, the environment, "public"; a store holding no such entry
;;;; costs one hook call and nothing else.
;;;;
;;;; Config, a sibling top-level key next to `websearch' and `cron':
;;;;   "codex-auth": {"enabled": true,
;;;;                  "endpoint": "https://chatgpt.com/backend-api/codex/responses"}
;;;; `endpoint' is the address a saved login is served at — an operator who
;;;; reaches the backend through a relay names theirs. A vetoed section
;;;; ("enabled": false) installs no hook at all, so the store's OAuth tokens
;;;; resolve to nothing and the chain falls through to the environment exactly
;;;; as it did before this folder existed.

(in-package #:nodecode-codex-auth)

(nle:define-cell codex-auth
  (:section ("codex-auth")
    (:guide "endpoint is the ChatGPT backend a saved login is served at")
    ("endpoint" :string :default "https://chatgpt.com/backend-api/codex/responses"
     :doc "the address an OAuth credential for an openai-family lane is served at"))
  (:hook :credential
    (lambda (op next)
      (or (credential op) (funcall next op)))))

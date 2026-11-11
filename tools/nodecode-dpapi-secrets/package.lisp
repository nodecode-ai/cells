;;;; package.lisp --- the cell's one package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The loader finds START-CELL as the exported symbol of the package named
;;;; after the system, and every contribution is filed under the name
;;;; downcased, which is this folder's name. The original plugin had no
;;;; package of its own because it never spoke to the model: it was a Hermes
;;;; secret source, and the orchestrator called it. Here it speaks.

(defpackage #:nodecode-dpapi-secrets
  (:use #:cl)
  (:export #:start-cell
           #:stop-cell
           #:dpapi-secrets-error
           #:dpapi-secrets-error-detail
           ;; the verbs a form uses instead of a tool call
           #:secret #:resolve #:entries
           ;; the tools' bodies
           #:list-entries #:read-secret #:store-secret #:drop-secret
           #:bulk-read #:status
           ;; the store, as the tests and a form read it
           #:store-path #:protection #:store-entries #:store-entry #:read-store #:write-store
           #:dpapi-available-p #:parse-map #:valid-env-name-p
           #:decode-entry #:decode-entry-write))

(in-package #:nodecode-dpapi-secrets)

(nlk:define-peripheral dpapi-secrets :not-running t)

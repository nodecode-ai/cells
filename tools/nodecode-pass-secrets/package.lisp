;;;; package.lisp --- the cell's one package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The loader finds START-CELL as the exported symbol of the package named
;;;; after the system, and every contribution is filed under the same name
;;;; downcased, which is this folder's name.

(defpackage #:nodecode-pass-secrets
  (:use #:cl)
  (:export #:start-cell
           #:stop-cell
           #:pass-secrets-error
           #:pass-secrets-error-detail
           #:list-entries
           #:read-secret
           #:bulk-read
           #:status
           #:secret
           #:show
           #:entries
           #:env-values
           #:valid-env-name-p
           #:safe-name-p
           #:pass-name
           #:leaf-name
           #:run-cli
           #:decode-entry-listing
           #:decode-secret-read))

(in-package #:nodecode-pass-secrets)

;;;; package.lisp --- the cell's one package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The loader finds START-CELL as the exported symbol of the package named
;;;; after the system, and every contribution is filed under the same name
;;;; downcased, which is this folder's name.

(defpackage #:nodecode-wincred
  (:use #:cl)
  (:export #:start-cell
           #:stop-cell
           #:wincred-error
           #:wincred-error-target
           #:wincred-error-code
           #:wincred-error-detail
           #:credential-decode-error
           #:decode-credential
           #:vault-available-p
           #:credential-value
           #:environment-name-p
           #:credential-target
           #:parse-mapping
           #:mapping
           #:apply-mapping
           #:environment-value
           #:set-environment-variable
           #:unset-environment-variable
           #:status-text
           #:status
           #:read-one
           #:apply-tool
           #:run-command))

(in-package #:nodecode-wincred)

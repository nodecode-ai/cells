;;;; package.lisp --- the cell's one package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The loader finds START-CELL as the exported symbol of the package named
;;;; after the system, and every contribution is filed under the same name
;;;; downcased, which is this folder's name.

(defpackage #:nodecode-agentplaybooks
  (:use #:cl)
  (:export #:start-cell
           #:stop-cell
           #:agentplaybooks-error
           #:agentplaybooks-error-detail
           #:apb-doctor
           #:apb-setup
           #:apb-connect
           #:apb-project
           #:decode-doctor-query
           #:decode-setup-query
           #:decode-connect-query
           #:decode-project-query
           #:call
           #:run-cli
           #:words
           #:version-of))

(in-package #:nodecode-agentplaybooks)

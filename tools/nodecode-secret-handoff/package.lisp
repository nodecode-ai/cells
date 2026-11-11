;;;; package.lisp --- the cell's one package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The loader finds START-CELL as the exported symbol of the package named
;;;; after the system, and every contribution is filed under the same name
;;;; downcased, which is this folder's name.

(defpackage #:nodecode-secret-handoff
  (:use #:cl)
  (:export #:start-cell
           #:stop-cell
           #:secret-handoff-error
           #:secret-handoff-error-detail
           #:request-secret
           #:run-command
           #:answer-page
           #:decode-secret-request
           #:page-origin
           #:url-host
           #:origin-matches-service
           #:loopback-origin-p
           #:pick-target
           #:cdp-plan
           #:inject-secret
           #:settle
           #:pending-list
           #:pending-id
           #:pending-service
           #:pending-origin
           #:cancel-all
           #:url-decode
           #:form-fields
           #:jget))

(in-package #:nodecode-secret-handoff)

(nlk:define-peripheral secret-handoff :not-running t)

;;;; package.lisp --- the cell's one package.
;;;;
;;;; SPDX-License-Identifier: MIT

(defpackage #:nodecode-secret-guard
  (:use #:cl)
  (:nicknames #:secret-guard)
  (:export #:start-cell #:stop-cell #:setting
           ;; what a form reaches for
           #:secret #:secret-names #:held #:redact-text
           ;; the store
           #:register-secret #:forget-secret #:load-store #:save-store #:store-path
           #:fingerprint #:held-secret-name #:held-secret-fingerprint #:held-secret-length
           #:held-secret-source #:held-secret-created #:held-secret-value
           ;; the gate
           #:gate-reason #:argument-text #:secret-shaped-p
           ;; the contributions
           #:run-slash #:status-text #:list-text #:verify-text
           #:secret-request-tool #:secret-list-tool #:secret-check-tool
           #:secret-request-request #:secret-list-request #:secret-check-request
           #:decode-secret-request-request #:decode-secret-list-request
           #:decode-secret-check-request))

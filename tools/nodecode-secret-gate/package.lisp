;;;; package.lisp --- the cell's one package.
;;;;
;;;; SPDX-License-Identifier: MIT

(defpackage #:nodecode-secret-gate
  (:use #:cl)
  (:nicknames #:secret-gate)
  (:export #:start-cell #:stop-cell #:setting
           #:scan-text #:scan-line
           #:secret-scan-tool #:secret-gate-check-tool #:secret-gate-receipts-tool
           #:run-slash #:receipts #:status-text
           #:secret-scan-request #:secret-gate-check-request #:secret-gate-receipts-request
           #:decode-secret-scan-request #:decode-secret-gate-check-request
           #:decode-secret-gate-receipts-request))

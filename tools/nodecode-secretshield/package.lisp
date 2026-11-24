;;;; package.lisp --- the cell's one package.
;;;;
;;;; SPDX-License-Identifier: MIT

(defpackage #:nodecode-secretshield
  (:use #:cl)
  (:nicknames #:secretshield)
  (:export #:start-cell #:stop-cell #:setting
           ;; the scanner, callable on its own
           #:scan-text #:redact-text #:scan-arguments #:redact-arguments
           #:finding-kind #:finding-preview #:finding-length #:finding-path
           #:detector-kinds
           ;; the incident ledger
           #:incident-path #:recent-incidents #:incident-text))

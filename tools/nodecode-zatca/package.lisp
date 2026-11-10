;;;; package.lisp --- the one package, and what the other files read.
;;;;
;;;; SPDX-License-Identifier: MIT

(defpackage #:nodecode-zatca
  (:use #:cl)
  (:export #:halalas #:money #:money-sar #:vat-breakdown #:vat-answer #:report-text
           #:check-invoice #:valid-uuid-p #:valid-seller-vat-p #:valid-date-p
           #:valid-time-p #:parse-amount #:words
           #:run-vat #:run-validate #:run-slash #:start-cell #:stop-cell))

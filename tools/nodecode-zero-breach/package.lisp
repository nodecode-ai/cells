;;;; package.lisp --- one package, named after the system, nicknamed for the command.
;;;;
;;;; SPDX-License-Identifier: MIT

(defpackage #:nodecode-zero-breach
  (:use #:cl)
  (:nicknames #:zero-breach)
  (:export #:scan-answer #:phone-answer #:last-answer #:describe-text
           #:gather-scan #:read-report #:report-text #:run-slash
           #:engine-directory #:exit-meaning #:newest-report
           #:*run* #:zero-breach-error))

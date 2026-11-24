;;;; package.lisp --- the cell's one package.
;;;;
;;;; SPDX-License-Identifier: MIT

(defpackage #:nodecode-ja-writing-guard
  (:use #:cl)
  (:export #:start-cell #:stop-cell #:setting
           #:check-text #:format-report #:report-summary #:report-score
           #:report-threshold #:report-findings #:finding-line #:finding-col
           #:finding-label #:finding-hint #:finding-counted
           #:report-japanese #:report-register-issues #:finding-rule
           #:surface-names #:run-slash #:run-check
           #:japanese-p #:mask-text #:last-report))

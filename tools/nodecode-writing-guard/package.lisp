;;;; package.lisp --- the cell's one package.
;;;;
;;;; SPDX-License-Identifier: MIT

(defpackage #:nodecode-writing-guard
  (:use #:cl)
  (:export #:start-cell #:stop-cell #:setting
           #:check-text #:report #:report-language #:report-dashes #:report-dashes-fixed
           #:report-phrases #:report-du #:report-sie #:report-mixed #:report-form
           #:report-expected #:report-wrong-form #:report-switched-from #:report-text
           #:report-findings-p #:footer-line #:fix-dashes #:count-dashes
           #:detect-language #:find-phrases #:address-counts #:mixed-paragraphs
           #:mask #:unmask #:run-slash #:run-check))

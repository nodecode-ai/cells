;;;; package.lisp --- the package of nodecode-charts.
;;;;
;;;; SPDX-License-Identifier: MIT

(defpackage #:nodecode-charts
  (:use #:cl)
  (:export #:start-cell #:stop-cell
           #:chart-error #:chart-error-code #:chart-error-message
           #:chart-error-where #:chart-error-fix
           #:parse-number #:not-a-number #:not-a-number-text
           #:format-value #:format-values #:choose-style #:decimals-for
           #:load-table #:table #:table-columns #:table-rows #:table-labels
           #:build-plan #:plan #:plan-chart-type #:plan-categories #:plan-series
           #:plan-values #:plan-unit #:plan-source #:plan-title #:plan-notes
           #:plan-orientation #:plan-locale #:plan-number-style #:plan-decimals
           #:plan-preset #:plan-width #:plan-height #:plan-category-total
           #:plan-warnings #:plan-inferred #:plan-highlight
           #:render-plan #:drawing #:drawing-size #:drawing-labels #:drawing-path
           #:drawing-bars #:verify-drawing #:alt-text
           #:png-path #:output-dir
           #:run-render #:run-validate #:run-types #:run-slash))

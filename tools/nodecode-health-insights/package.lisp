;;;; package.lisp --- the HEALTH-INSIGHTS package: the reports and the one condition.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Every exported verb answers a STRING a model can read as it stands; the
;;;; refusal is HEALTH-INSIGHTS-ERROR, rendered by the eval snippet as
;;;; "ERROR: HEALTH-INSIGHTS-ERROR: <detail>".
;;;;
;;;; The settings the cell runs on derive from the one `health_insights'
;;;; section declaration in cell.lisp (NLE:DEFINE-CELL), read with
;;;; (SETTING :db) and (SETTING :utc-offset-hours).

(defpackage #:nodecode-health-insights
  (:use #:cl)
  (:export
   ;; the reports, each a string
   #:weekly-report #:concerns-report #:coverage-report #:daily-report #:report-text
   ;; data, so a layer can reword it
   #:*metrics* #:*reports* #:*source* #:*level-words*
   #:*recent-days* #:*baseline-days*
   ;; the one condition
   #:health-insights-error))

(in-package #:nodecode-health-insights)

(nlk:define-peripheral health-insights :not-running t)

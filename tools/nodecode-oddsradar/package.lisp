;;;; package.lisp --- the NODECODE-ODDSRADAR package: the model's verbs and the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system: the loader finds START-CELL as
;;;; the exported symbol of package NODECODE-ODDSRADAR, and every
;;;; contribution is filed under that name downcased, which is this folder's
;;;; name. DEFINE-PERIPHERAL gives the package its condition
;;;; ODDSRADAR-ERROR, FAIL, the settings variable and DEFINE-VERB, so the
;;;; files between this one and the declaration refuse in one voice.

(defpackage #:nodecode-oddsradar
  (:use #:cl)
  (:export #:search-markets #:movers #:resolve-market #:live-price #:sparkline
           #:load-watches #:add-watch #:remove-watch #:check-watches #:oddsradar-error))

(in-package #:nodecode-oddsradar)

(nlk:define-peripheral oddsradar :not-running t)

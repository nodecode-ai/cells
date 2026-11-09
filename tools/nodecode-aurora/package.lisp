;;;; package.lisp --- the cell's package: two verbs and the census behind them.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Named after the system, which is how the folder loader finds START-CELL
;;;; in it, and nicknamed AURORA so an eval form reads (aurora:lookup "...").
;;;; Inside the package, CL:SEARCH keeps its own meaning: this cell scans
;;;; strings for a living.

(defpackage #:nodecode-aurora
  (:use #:cl)
  (:nicknames #:aurora)
  (:export #:start-cell #:stop-cell #:setting
           #:lookup #:check #:report-text
           #:census-info #:ensure-census
           #:aurora-error))

(in-package #:nodecode-aurora)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last. FAIL and AURORA-ERROR, which the folder signals when its
;;; census snapshot cannot be read, and the *AURORA* settings variable, come
;;; from here.
(nlk:define-peripheral aurora :not-running t)

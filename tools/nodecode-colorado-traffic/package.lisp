;;;; package.lisp --- the cell's package, named after its system.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nicknamed COTRIP so an eval form reads (cotrip:events). Every verb
;;;; answers a string; a refusal is COLORADO-TRAFFIC-ERROR, which the eval
;;;; snippet renders as "ERROR: COLORADO-TRAFFIC-ERROR: <detail>".

(defpackage #:nodecode-colorado-traffic
  (:use #:cl)
  (:nicknames #:cotrip)
  (:export #:start-cell #:stop-cell #:events #:colorado-traffic-error))

(in-package #:nodecode-colorado-traffic)

;;; FAIL and COLORADO-TRAFFIC-ERROR, and *COLORADO-TRAFFIC* with
;;; RUNNING-SETTINGS and DEFINE-VERB: the settings come from the
;;; NLE:DEFINE-CELL declaration in cell.lisp, which loads last, and cotrip.lisp
;;; between them reads the section through (SETTING ...).
(nlk:define-peripheral colorado-traffic :not-running t)

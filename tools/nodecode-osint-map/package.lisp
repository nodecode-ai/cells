;;;; package.lisp --- the OSINT-MAP package: the model's five verbs and the one condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed OSINT so an eval form reads
;;;; (osint:situation). SETTING, FAIL and OSINT-MAP-ERROR come from
;;;; NLK:DEFINE-PERIPHERAL; the cell's declaration in cell.lisp loads last.

(defpackage #:nodecode-osint-map
  (:use #:cl)
  (:nicknames #:osint)
  (:export #:start-cell #:stop-cell #:situation #:events #:news #:recon #:country #:osint-map-error))

(in-package #:nodecode-osint-map)

(nlk:define-peripheral osint-map :not-running t)

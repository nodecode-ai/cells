;;;; package.lisp --- the NODECODE-FLEETKEEP package: the review, and its verbs.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed FLEETKEEP so an eval form reads
;;;; (fleetkeep:review ...). The pure half -- PARSE-CSV, WRITE-CSV, REVIEW --
;;;; is exported because it is where the behaviour is decided and what the
;;;; tests read; the tool and the command wrap it.

(defpackage #:nodecode-fleetkeep
  (:use #:cl)
  (:nicknames #:fleetkeep)
  (:export
   ;; the pure half: the CSV, and the review of one
   #:parse-csv #:write-csv #:review #:report-text
   ;; the cell's own verbs
   #:run-review #:run-slash #:start-cell #:stop-cell
   ;; the one condition
   #:fleetkeep-error))

(in-package #:nodecode-fleetkeep)

;;; SETTING, FAIL and RUNNING-SETTINGS come from the NLE:DEFINE-CELL
;;; declaration in cell.lisp, which loads last; fleetkeep.lisp reads the
;;; section through SETTING.
(nlk:define-peripheral fleetkeep :not-running t)

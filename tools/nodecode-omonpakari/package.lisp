;;;; package.lisp --- the OMONPAKARI package: seven tools, one hook, one manual.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed OMONPAKARI so an eval form reads
;;;; (omonpakari:picture-text). The verbs are tools the model calls; a person
;;;; can call the same functions from eval.

(defpackage #:nodecode-omonpakari
  (:use #:cl)
  (:nicknames #:omonpakari)
  (:export #:start-cell
           #:stop-cell
           #:setting
           #:picture-text
           #:pressure-now
           #:log-entry
           #:cycle-status
           #:interactions-check
           #:life-notes
           #:waves
           #:correlate
           #:*store-file*))

(in-package #:nodecode-omonpakari)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last. FAIL and *OMONPAKARI*, the settings variable, come from here.
(nlk:define-peripheral omonpakari :not-running t)

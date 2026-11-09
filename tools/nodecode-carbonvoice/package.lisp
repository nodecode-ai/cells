;;;; package.lisp --- the NODECODE-CARBONVOICE package: four verbs, one condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed CARBONVOICE so an eval form reads
;;;; (carbonvoice:recent :minutes 30).

(defpackage #:nodecode-carbonvoice
  (:use #:cl)
  (:nicknames #:carbonvoice)
  (:export #:start-cell
           #:stop-cell
           #:recent #:read-message #:send #:channel #:whoami #:status
           #:running-settings
           #:carbonvoice-error #:carbonvoice-error-detail))

(in-package #:nodecode-carbonvoice)

;;; CARBONVOICE-ERROR, its detail reader, FAIL, the settings variable, the
;;; not-running refusal and SETTING all come from here; cell.lisp declares the
;;; section that fills them in.
(nlk:define-peripheral carbonvoice :not-running t)

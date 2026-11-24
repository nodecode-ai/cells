;;;; package.lisp --- the NODECODE-PUSHOVER package: one tool, its condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed PUSHOVER so an eval form reads
;;;; (pushover:notify "...").

(defpackage #:nodecode-pushover
  (:use #:cl)
  (:nicknames #:pushover)
  (:export #:start-cell
           #:stop-cell
           #:send-notification #:notify #:status
           #:running-settings
           #:pushover-error #:pushover-error-detail))

(in-package #:nodecode-pushover)

;;; PUSHOVER-ERROR, its detail reader, FAIL, the settings variable, the
;;; not-running refusal and SETTING's companion all come from here; cell.lisp
;;; declares the section that fills them in.
(nlk:define-peripheral pushover :not-running t)

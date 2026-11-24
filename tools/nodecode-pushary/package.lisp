;;;; package.lisp --- the NODECODE-PUSHARY package: six tools, one command.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed PUSHARY so an eval form reads
;;;; (pushary:notify "the build is green").

(defpackage #:nodecode-pushary
  (:use #:cl)
  (:nicknames #:pushary)
  (:export #:start-cell
           #:stop-cell
           #:notify #:ask #:wait-for-answer #:cancel #:remind #:propose-scope
           #:send-notification #:status
           #:running-settings
           #:pushary-error #:pushary-error-detail))

(in-package #:nodecode-pushary)

;;; PUSHARY-ERROR, its detail reader, FAIL, the settings variable, the
;;; not-running refusal and SETTING all come from here; cell.lisp declares the
;;; section that fills them in.
(nlk:define-peripheral pushary :not-running t)

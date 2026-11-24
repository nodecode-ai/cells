;;;; package.lisp --- the NODECODE-CALL-ME package: two tools, one command.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed CALL-ME so an eval form reads
;;;; (call-me:ring "5551234567" "the build is green").

(defpackage #:nodecode-call-me
  (:use #:cl)
  (:nicknames #:call-me)
  (:export #:start-cell
           #:stop-cell
           #:make-call #:send-text #:ring #:text-message
           #:status
           #:running-settings
           #:call-me-error #:call-me-error-detail))

(in-package #:nodecode-call-me)

;;; CALL-ME-ERROR, its detail reader, FAIL, the settings variable, the
;;; not-running refusal and SETTING come from here; cell.lisp declares the
;;; section that fills them in.
(nlk:define-peripheral call-me :not-running t)

(nlk:define-error session-gone (call-me-error) ()
  (:documentation "The service no longer knows the stored session token: the
next call mints another one and the agent's number changes with it."))

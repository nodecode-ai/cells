;;;; package.lisp --- the NODECODE-PETHOST package: the MCP bridge and its tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed PETHOST so an eval form reads
;;;; (pethost:call-tool "GetMachine").

(defpackage #:nodecode-pethost
  (:use #:cl)
  (:nicknames #:pethost)
  (:export #:start-cell
           #:stop-cell
           #:call-tool #:list-tools #:machine
           #:status
           #:running-settings
           #:pethost-error #:pethost-error-detail))

(in-package #:nodecode-pethost)

;;; PETHOST-ERROR, its detail reader, FAIL, the settings variable, the
;;; not-running refusal and SETTING all come from here; cell.lisp declares the
;;; section that fills them in.
(nlk:define-peripheral pethost :not-running t)

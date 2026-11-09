;;;; package.lisp --- the NODECODE-ADWAVE package: the MCP bridge and its tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed ADW so an eval form reads (adw:list-tools).

(defpackage #:nodecode-adwave
  (:use #:cl)
  (:nicknames #:adw)
  (:export #:start-cell
           #:stop-cell
           #:list-tools #:call-tool
           #:running-settings
           #:adwave-error #:adwave-error-detail))

(in-package #:nodecode-adwave)

;;; ADWAVE-ERROR, its detail reader, FAIL, the settings variable, the
;;; not-running refusal and SETTING all come from here; cell.lisp declares
;;; the section that fills them in.
(nlk:define-peripheral adwave :not-running t)

;;;; package.lisp --- the NODECODE-0BULL package: the MCP bridge and its tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed BULL so an eval form reads
;;;; (bull:phones).

(defpackage #:nodecode-0bull
  (:use #:cl)
  (:nicknames #:bull)
  (:export #:start-cell
           #:stop-cell
           #:phones #:accounts #:see #:act #:run #:post #:status
           #:running-settings
           #:0bull-error #:0bull-error-detail))

(in-package #:nodecode-0bull)

;;; 0BULL-ERROR, its detail reader, FAIL, the settings variable, the
;;; not-running refusal and SETTING all come from here; cell.lisp declares
;;; the section that fills them in.
(nlk:define-peripheral 0bull :not-running t)

;;;; package.lisp --- the NODECODE-COSPARK package: the MCP bridge and its tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed COSPARK so an eval form reads
;;;; (cospark:list-tools nil).

(defpackage #:nodecode-cospark
  (:use #:cl)
  (:nicknames #:cospark)
  (:export #:start-cell
           #:stop-cell
           #:docs #:list-tools #:call-tool
           #:running-settings
           #:cospark-error #:cospark-error-detail))

(in-package #:nodecode-cospark)

;;; COSPARK-ERROR, its detail reader, FAIL, *COSPARK*, RUNNING-SETTINGS and
;;; the not-running refusal come from here; cell.lisp declares the section
;;; that fills them in.
(nlk:define-peripheral cospark :not-running t)

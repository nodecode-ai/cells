;;;; package.lisp --- the NODECODE-ASTRO package: the MCP bridge and its tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed ASTRO so an eval form reads
;;;; (astro:chart ...).

(defpackage #:nodecode-astro
  (:use #:cl)
  (:nicknames #:astro)
  (:export #:start-cell
           #:stop-cell
           #:chart #:sky #:kundli #:any-tool
           #:running-settings
           #:astro-error #:astro-error-detail))

(in-package #:nodecode-astro)

;;; ASTRO-ERROR, its detail reader, FAIL, the settings variable, the
;;; not-running refusal and SETTING all come from here; cell.lisp declares
;;; the section that fills them in.
(nlk:define-peripheral astro :not-running t)

;;;; package.lisp --- the WSLC package: the sandbox's verbs and its condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader
;;;; finds START-CELL in it, nicknamed WSLC so an eval form reads
;;;; (wslc:exec "uname -a"). It is meant to be used qualified only.

(defpackage #:nodecode-wslc
  (:use #:cl)
  (:nicknames #:wslc)
  (:export #:exec #:sandboxes #:remove-sandbox #:sandbox-name #:binary
           #:version #:wslc-error))

(in-package #:nodecode-wslc)

;;; FAIL, WSLC-ERROR, *WSLC*, RUNNING-SETTINGS and DEFINE-VERB come from
;;; here; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp,
;;; which loads last, and cli.lisp reads the section through it.
(nlk:define-peripheral wslc :not-running t)

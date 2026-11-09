;;;; package.lisp --- the NODECODE-USAGE-GUARD package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader
;;;; finds START-CELL in it, and nicknamed USAGE-GUARD so an eval form reads
;;;; (usage-guard:report). Every refusal the model sees is
;;;; USAGE-GUARD-ERROR, which the eval snippet renders as "ERROR:
;;;; USAGE-GUARD-ERROR: <detail>"; the detail names a source, never a token.

(defpackage #:nodecode-usage-guard
  (:use #:cl)
  (:nicknames #:usage-guard)
  (:export #:report #:status #:windows))

(in-package #:nodecode-usage-guard)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last; the functions between read the section through it.
(nlk:define-peripheral usage-guard :not-running t)

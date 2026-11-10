;;;; package.lisp --- the CADUCEUS package: one vault, four tools.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed CADUCEUS so an eval form reads
;;;; (caduceus:vault-summary). The verbs are tools the model calls; a person
;;;; can call the same functions from eval.

(defpackage #:nodecode-caduceus
  (:use #:cl)
  (:nicknames #:caduceus)
  (:export #:start-cell
           #:stop-cell
           #:search-vault
           #:read-genome
           #:read-trend
           #:capture
           #:caduceus-command
           #:caduceus-error
           #:caduceus-error-detail))

(in-package #:nodecode-caduceus)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last. FAIL and CADUCEUS-ERROR, which a verb that cannot go on
;;; signals, come from here: every refusal starts as one of these, and the
;;; tools answer it as the call's failure.
(nlk:define-peripheral caduceus :not-running t)

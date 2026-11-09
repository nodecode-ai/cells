;;;; package.lisp --- the HERMES-HERALD package: six tools, their condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed HERMES-HERALD so an eval form reads
;;;; (hermes-herald:dispatch ...). The tools are what the model calls; a person
;;;; can call the same functions from eval.

(defpackage #:nodecode-hermes-herald
  (:use #:cl)
  (:nicknames #:hermes-herald)
  (:export #:start-cell
           #:stop-cell
           #:dispatch #:check #:cancel #:models #:chat #:runs #:report
           #:hermes-herald-error #:hermes-herald-error-detail))

(in-package #:nodecode-hermes-herald)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last. FAIL and HERMES-HERALD-ERROR, which a request that cannot go on
;;; signals, come from here: every refusal starts as one of these, and the
;;; tools answer it as the call's failure rather than as an unhandled error.
(nlk:define-peripheral hermes-herald :not-running t)

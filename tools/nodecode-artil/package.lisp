;;;; package.lisp --- the NODECODE-ARTIL package: the verbs and their condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, nicknamed ARTIL so an eval form reads
;;;; (artil:inbox "email"). It is meant to be used qualified only.

(defpackage #:nodecode-artil
  (:use #:cl)
  (:nicknames #:artil)
  (:export #:inbox #:read-message #:send-message #:message-agent #:team #:status
           #:artil-error))

(in-package #:nodecode-artil)

;;; ARTIL-ERROR is the one condition every refusal signals, FAIL signals it
;;; with a detail; NOT-RUNNING gives this package *ARTIL*, RUNNING-SETTINGS --
;;; the refusal a verb opens with while the cell is not started -- and
;;; DEFINE-VERB. SETTING comes from the NLE:DEFINE-CELL declaration in
;;; cell.lisp, which loads last.
(nlk:define-peripheral artil :not-running t)

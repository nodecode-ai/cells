;;;; package.lisp --- the HASS-STATISTICS package: two tools over one socket.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed HASS-STATISTICS so an eval form reads
;;;; (hass-statistics:list-statistics ...). The two verbs are tools the model
;;;; calls; a person can call the same functions from eval.

(defpackage #:nodecode-hass-statistics
  (:use #:cl)
  (:nicknames #:hass-statistics)
  (:export #:start-cell
           #:stop-cell
           #:list-statistics #:get-statistics
           #:*wire-maker*
           #:hass-statistics-error #:hass-statistics-error-detail))

(in-package #:nodecode-hass-statistics)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last. FAIL and HASS-STATISTICS-ERROR, which an exchange that cannot
;;; go on signals, come from here: every refusal starts as one of these, and
;;; the tools answer it as the call's failure rather than as an unhandled
;;; error.
(nlk:define-peripheral hass-statistics :not-running t)

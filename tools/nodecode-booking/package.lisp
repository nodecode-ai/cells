;;;; package.lisp --- the NODECODE-BOOKING package: one client for Google Calendar v3.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, nicknamed BOOKING so an eval form reads
;;;; (booking:status-line) or (booking:list-availability ...).

(defpackage #:nodecode-booking
  (:use #:cl)
  (:nicknames #:booking)
  (:export #:start-cell
           #:stop-cell
           #:running-settings
           #:booking-error
           #:booking-error-detail
           #:status-line))

(in-package #:nodecode-booking)

;; BOOKING-ERROR, its detail reader, FAIL, the settings variable and the
;; not-running refusal come from here; cell.lisp declares the section that
;; fills them in.
(nlk:define-peripheral booking :not-running t)

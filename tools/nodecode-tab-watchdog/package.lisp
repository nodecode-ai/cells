;;;; package.lisp --- the TAB-WATCHDOG package: the verbs the model and eval call.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed TAB-WATCHDOG so an eval form reads
;;;; (tab-watchdog:report). Meant to be used qualified only.

(defpackage #:nodecode-tab-watchdog
  (:use #:cl)
  (:nicknames #:tab-watchdog)
  (:export #:report #:sweep #:tabs))

(in-package #:nodecode-tab-watchdog)

;;; *TAB-WATCHDOG* (the running settings), RUNNING-SETTINGS, FAIL,
;;; TAB-WATCHDOG-ERROR, SETTING and, from :LOCK, WITH-TAB-WATCHDOG-LOCK.
(nlk:define-peripheral tab-watchdog :not-running t :lock "tab-watchdog")

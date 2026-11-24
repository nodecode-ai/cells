;;;; package.lisp --- the BROWSER-PROFILES package: the verbs the model calls.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed BROWSER-PROFILES so an eval form reads
;;;; (browser-profiles:switch "work"). Meant to be used qualified only.

(defpackage #:nodecode-browser-profiles
  (:use #:cl)
  (:nicknames #:browser-profiles)
  (:export #:available #:switch #:disconnect #:cdp-url #:version
           #:tabs #:open-tab #:activate-tab #:close-tab))

(in-package #:nodecode-browser-profiles)

;;; *BROWSER-PROFILES* (the running settings), RUNNING-SETTINGS, FAIL,
;;; BROWSER-PROFILES-ERROR, DEFINE-VERB and, with :LOCK, *LOCK* and
;;; WITH-BROWSER-PROFILES-LOCK. SETTING comes from the NLE:DEFINE-CELL
;;; declaration in cell.lisp, which loads after this file.
(nlk:define-peripheral browser-profiles :not-running t :lock "browser-profiles")

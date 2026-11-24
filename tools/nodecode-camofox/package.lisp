;;;; package.lisp --- the CAMOFOX package: the condition, the settings, the lock.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, used qualified from the outside; the model never calls it
;;;; directly, it calls the browser_* tools cell.lisp registers. A refusal in
;;;; a tool body signals CAMOFOX-ERROR, which the engine renders to the model
;;;; as "ERROR: <detail>".
;;;;
;;;; NLK:DEFINE-PERIPHERAL gives this package CAMOFOX-ERROR, FAIL, the
;;;; settings variable *CAMOFOX*, RUNNING-SETTINGS (the refusal a verb gives
;;;; before the cell started) and WITH-CAMOFOX-LOCK around the tab table. The
;;;; typed settings derive from the one `camofox' section declaration in
;;;; cell.lisp.

(defpackage #:nodecode-camofox
  (:use #:cl)
  (:export #:camofox-error))

(in-package #:nodecode-camofox)

(nlk:define-peripheral camofox :not-running t :lock "camofox" :filter redact)

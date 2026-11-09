;;;; package.lisp --- the MERMAIL package: the condition, the settings, the session.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, used qualified from the outside; the model never calls it
;;;; directly, it calls the mermail_* tools cell.lisp registers. A refusal in
;;;; a tool body signals MERMAIL-ERROR, which the engine renders to the model
;;;; as "ERROR: <detail>".
;;;;
;;;; NLK:DEFINE-PERIPHERAL gives this package MERMAIL-ERROR, FAIL, the
;;;; settings variable *MERMAIL*, RUNNING-SETTINGS (the refusal a verb gives
;;;; before the cell started) and WITH-MERMAIL-LOCK around the MCP session.
;;;; The typed settings derive from the one `mermail' section declaration in
;;;; cell.lisp.

(defpackage #:nodecode-mermail
  (:use #:cl)
  (:export #:mermail-error))

(in-package #:nodecode-mermail)

(declaim (ftype (function (t) t) redact))

(nlk:define-peripheral mermail :not-running t :lock "mermail" :recursive t :filter redact)

;;;; package.lisp --- the BROWSERLESS package: the condition, the settings, the token filter.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, used qualified from the outside; the model never calls it
;;;; directly, it calls the browserless_* tools cell.lisp registers. A
;;;; refusal in a tool body signals BROWSERLESS-ERROR, which the engine
;;;; renders to the model as "ERROR: <detail>".
;;;;
;;;; NLK:DEFINE-PERIPHERAL gives this package BROWSERLESS-ERROR, FAIL, the
;;;; settings variable *BROWSERLESS*, RUNNING-SETTINGS (the refusal a verb
;;;; gives before the cell started) and the typed settings derived from the
;;;; one `browserless' section declaration in cell.lisp. Its :FILTER takes
;;;; the API token back out of whatever a message or a notice says.

(defpackage #:nodecode-browserless
  (:use #:cl)
  (:export #:browserless-error))

(in-package #:nodecode-browserless)

(nlk:define-peripheral browserless :not-running t :filter redact)

;;;; package.lisp --- the PROMPT-ENHANCE package: the condition and the settings.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, used qualified from the outside; the model never calls it
;;;; directly, it calls the prompt_* tools cell.lisp registers. A refusal in
;;;; a tool body signals PROMPT-ENHANCE-ERROR, which the engine renders to
;;;; the model as "ERROR: <detail>".
;;;;
;;;; NLK:DEFINE-PERIPHERAL gives this package PROMPT-ENHANCE-ERROR, FAIL, the
;;;; settings variable *PROMPT-ENHANCE* and RUNNING-SETTINGS (the refusal a
;;;; verb gives before the cell started). The typed settings derive from the
;;;; one `prompt-enhance' section declaration in cell.lisp.

(defpackage #:nodecode-prompt-enhance
  (:use #:cl)
  (:export #:prompt-enhance-error))

(in-package #:nodecode-prompt-enhance)

(nlk:define-peripheral prompt-enhance :not-running t)

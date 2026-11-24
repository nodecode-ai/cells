;;;; package.lisp --- the cell's package: the condition, the settings, the verbs.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed NVAPP so an EVAL form reads (nvapp:tools). The
;;;; name is the system's, which is how the folder loader finds START-CELL.
;;;; Every verb answers a STRING -- the eval snippet prints the value with
;;;; ~S, so a string reads as text -- and every refusal is NVIDIA-APP-ERROR,
;;;; rendered by the eval snippet as "ERROR: NVIDIA-APP-ERROR: <detail>".
;;;;
;;;; NLK:DEFINE-PERIPHERAL gives this package NVIDIA-APP-ERROR, FAIL, the
;;;; settings variable *NVIDIA-APP*, RUNNING-SETTINGS (the refusal a verb
;;;; gives before the cell started) and the lock. Every failure detail has
;;;; this cell's token taken out of it before the model reads it.

(defpackage #:nodecode-nvidia-app
  (:use #:cl)
  (:nicknames #:nvapp)
  (:export #:nvidia-app-error
           ;; verbs, every one a string
           #:call-tool #:tools #:status
           ;; data, so a layer can reword it
           #:+primer+))

(in-package #:nodecode-nvidia-app)

(nlk:define-peripheral nvidia-app :not-running t :lock "nvidia-app" :filter redact)

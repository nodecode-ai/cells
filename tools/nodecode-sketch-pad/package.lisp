;;;; package.lisp --- the SKETCH-PAD package: the pad's own names.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, so an eval form reads (nodecode-sketch-pad:last-sketch).
;;;; NLK:DEFINE-PERIPHERAL gives it SKETCH-PAD-ERROR, FAIL, *SKETCH-PAD*,
;;;; RUNNING-SETTINGS and the SKETCH-PAD lock; every exported verb answers a
;;;; string and a refusal is that condition, which the tool and the command
;;;; render as "ERROR: <detail>".

(defpackage #:nodecode-sketch-pad
  (:use #:cl)
  (:export #:last-sketch #:last-sketch-text #:save-sketch #:pad-url
           #:pad-app))

(in-package #:nodecode-sketch-pad)

(nlk:define-peripheral sketch-pad :not-running t :lock "sketch-pad")

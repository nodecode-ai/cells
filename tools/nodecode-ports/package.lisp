;;;; package.lisp --- the PORTS package: who holds a local port, on any of the three.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed PORTS so an eval form reads (ports:owners 3000).
;;;; Every verb answers a STRING, and every refusal is PORTS-ERROR, rendered
;;;; by the eval snippet as "ERROR: PORTS-ERROR: <detail>".

(defpackage #:nodecode-ports
  (:use #:cl)
  (:nicknames #:ports)
  (:export
   ;; the question, and the answer as text
   #:owners #:rows
   ;; the seams a layer or a test rebinds
   #:*row-source* #:*platform* #:*proc-root* #:*run* #:*timeout-seconds*
   ;; the one condition
   #:ports-error #:ports-error-detail))

(in-package #:nodecode-ports)

(nlk:define-peripheral ports :not-running t)

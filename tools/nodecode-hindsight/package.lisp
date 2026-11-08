;;;; package.lisp --- the Hindsight package and its one condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed HINDSIGHT so an eval form reads
;;;; (hindsight:recall "what did we decide"): every exported verb answers a
;;;; STRING and every refusal is HINDSIGHT-ERROR, rendered by the tool or
;;;; the eval snippet as "ERROR: HINDSIGHT-ERROR: <detail>".
;;;;
;;;; The settings this cell runs on are NOT declared here: they derive from
;;;; the one `hindsight' section declaration in cell.lisp (NLE:DEFINE-CELL),
;;;; read with (SETTING :bank-id), and *HINDSIGHT* below is the variable
;;;; they land in.

(defpackage #:nodecode-hindsight
  (:use #:cl)
  (:nicknames #:hindsight)
  (:export
   ;; verbs, every one a string; the three the model calls are also tools
   #:remember #:recall #:reflect
   ;; the one condition
   #:hindsight-error))

(in-package #:nodecode-hindsight)

(nlk:define-peripheral hindsight :not-running t)

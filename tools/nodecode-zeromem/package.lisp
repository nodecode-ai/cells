;;;; package.lisp --- the ZEROMEM package: the verbs, the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed ZEROMEM so an eval form reads (zeromem:recall "x").
;;;; Every exported verb answers a STRING and every refusal is ZEROMEM-ERROR,
;;;; rendered by the eval snippet as "ERROR: ZEROMEM-ERROR: <detail>".
;;;;
;;;; The settings this cell runs on are NOT declared here: they derive from
;;;; the one `zeromem' section declaration in cell.lisp (NLE:DEFINE-CELL),
;;;; read with (SETTING :top-k), and *ZEROMEM* below is where they land.

(defpackage #:nodecode-zeromem
  (:use #:cl)
  (:nicknames #:zeromem)
  (:export
   ;; verbs, every one a string
   #:recall #:stats #:forget-session #:import-turns
   ;; the one condition
   #:zeromem-error))

(in-package #:nodecode-zeromem)

;;; The lock is over the store's file and its memory.
(nlk:define-peripheral zeromem :not-running t :lock "zeromem")

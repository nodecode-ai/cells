;;;; package.lisp --- the DYSCALCULIA package: six verbs, the condition, the words.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed DYSCALCULIA so an eval form reads
;;;; (dyscalculia:get-prevalence :age 11). Every verb answers a string the
;;;; model reads and every refusal is DYSCALCULIA-ERROR, rendered by the tool
;;;; dispatch and by the eval snippet as "ERROR: DYSCALCULIA-ERROR: <detail>".
;;;;
;;;; The settings this cell runs on derive from the one `dyscalculia' section
;;;; declared in cell.lisp (NLE:DEFINE-CELL), read with (SETTING :TIMEOUT-SECONDS).

(defpackage #:nodecode-dyscalculia
  (:use #:cl)
  (:nicknames #:dyscalculia)
  (:export
   ;; the six verbs of the plugin's hosted server, every one a string
   #:get-prevalence #:list-studies #:get-reference
   #:list-chart-data #:explain-screening-test #:get-citation
   ;; the operator's own door
   #:run-slash
   ;; data, so a layer can reword it
   #:*primer*
   ;; the one condition
   #:dyscalculia-error))

(in-package #:nodecode-dyscalculia)

;;; NLK:DEFINE-PERIPHERAL gives this package DYSCALCULIA-ERROR, FAIL, SETTING,
;;; RUNNING-SETTINGS, DEFINE-VERB and the *DYSCALCULIA* the settings live in.
(nlk:define-peripheral dyscalculia :not-running t)

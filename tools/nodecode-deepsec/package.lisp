;;;; package.lisp --- the DEEPSEC package: the verbs, the rules, the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed DEEPSEC so an eval form reads (deepsec:scan ...).
;;;; Every exported verb answers a STRING and every refusal is DEEPSEC-ERROR,
;;;; rendered by the eval snippet as "ERROR: DEEPSEC-ERROR: <detail>".
;;;;
;;;; The settings this cell runs on derive from the one `deepsec' section
;;;; declaration in cell.lisp (NLE:DEFINE-CELL), read with (SETTING :MAX-FINDINGS).

(defpackage #:nodecode-deepsec
  (:use #:cl)
  (:nicknames #:deepsec)
  (:export
   ;; verbs, every one a string
   #:scan #:agent-audit #:supply-chain #:report
   ;; data, so a layer can reword it or add a rule
   #:*rules* #:*agent-rules* #:*popular-names* #:+primer+
   ;; the one condition
   #:deepsec-error))

(in-package #:nodecode-deepsec)

(nlk:define-peripheral deepsec :not-running t :lock "deepsec")

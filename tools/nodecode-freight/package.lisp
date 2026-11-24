;;;; package.lisp --- the FREIGHT package: the case library's verbs, the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed FREIGHT so an eval form reads
;;;; (freight:differences "november"). Every exported verb answers a STRING
;;;; and every refusal is FREIGHT-ERROR, rendered by the eval snippet as
;;;; "ERROR: FREIGHT-ERROR: <detail>".
;;;;
;;;; The settings this cell runs on derive from the one `freight' section
;;;; declaration in cell.lisp (NLE:DEFINE-CELL), read with (SETTING :PAGE-SIZE).

(defpackage #:nodecode-freight
  (:use #:cl)
  (:nicknames #:freight)
  (:export
   ;; the case library, every verb a string
   #:open-case #:load-source #:reconcile #:summary #:differences #:export-review #:cases
   ;; data, so a layer can reword it
   #:+kinds+ #:+charges+ #:*framing* #:+primer+
   ;; the one condition
   #:freight-error))

(in-package #:nodecode-freight)

(nlk:define-peripheral freight :not-running t :lock "freight" :recursive t)

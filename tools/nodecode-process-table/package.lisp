;;;; package.lisp --- the PROCESS-TABLE package: one tool, one command, the words.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed PROCESS-TABLE so an eval form reads
;;;; (process-table:rows ...). The tools and the command answer a STRING and
;;;; every refusal is PROCESS-TABLE-ERROR, which the tool renders as
;;;; {"ok": false, "error": "..."} rather than failing the call.
;;;;
;;;; The settings this cell runs on derive from the one `process-table'
;;;; section declaration in cell.lisp (NLE:DEFINE-CELL), read with
;;;; (SETTING :LIMIT) and its siblings.

(defpackage #:nodecode-process-table
  (:use #:cl)
  (:nicknames #:process-table)
  (:export
   ;; the loader's two
   #:start-cell #:stop-cell
   ;; verbs, every one a string
   #:report #:describe-table #:read-rows
   ;; data, so a layer can reword it
   #:*system-prefixes*
   ;; the one condition
   #:process-table-error))

(in-package #:nodecode-process-table)

(nlk:define-peripheral process-table :not-running t)

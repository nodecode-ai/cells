;;;; package.lisp --- the OPENSPEC package: the verbs, the words, the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed OPSX so an eval form reads (opsx:status ...).
;;;; Every exported verb answers a string (JSON, as the original plugin did)
;;;; and every refusal is OPENSPEC-ERROR, which the tools render as
;;;; {"ok": false, "error": "<detail>"} so the model can repair the call.
;;;;
;;;; The settings this cell runs on derive from the one `openspec' section
;;;; declaration in cell.lisp (NLE:DEFINE-CELL): (setting :workdir).

(defpackage #:nodecode-openspec
  (:use #:cl)
  (:nicknames #:opsx)
  (:export
   ;; verbs, every one a string
   #:status #:changes #:tasks #:task-set #:validate #:spec-diff #:run-slash
   #:instructions #:context #:cli
   ;; data, so a layer can reword it
   #:+primer+
   ;; the one condition
   #:openspec-error #:openspec-error-detail))

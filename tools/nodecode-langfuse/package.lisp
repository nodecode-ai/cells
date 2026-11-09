;;;; package.lisp --- the Langfuse package: the settings variable and the lock.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed LANGFUSE so an eval form reads (langfuse:status).
;;;; The settings this cell runs on are not declared here: they derive from
;;;; the one `langfuse' section declaration in cell.lisp (NLE:DEFINE-CELL),
;;;; read with (SETTING :capture), and *LANGFUSE* below is the variable they
;;;; land in.

(defpackage #:nodecode-langfuse
  (:use #:cl)
  (:nicknames #:langfuse)
  (:export
   ;; what a test or an eval form drives
   #:*post* #:status #:send-all #:show-batch #:test-trace #:clear-held
   #:record-frame #:observe-frame #:observe-tool #:run-slash
   #:capture-string #:redact-string #:basic-auth #:ingestion-url
   #:+capture-modes+))

(in-package #:nodecode-langfuse)

(nlk:define-peripheral langfuse :not-running t :lock "langfuse")

;;;; package.lisp --- the FILE-LINEAGE package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Named after the system, as every cell is; nicknamed FILE-LINEAGE so an eval
;;;; form reads (file-lineage:explain-text ...). The verbs answer strings: the
;;;; answer's text.

(defpackage #:nodecode-file-lineage
  (:use #:cl)
  (:nicknames #:file-lineage)
  (:export
   ;; the graph, as data
   #:artifacts #:artifact #:explain-text #:verify-text #:mermaid-text
   #:receipt-json #:stats-text #:find-artifact #:store-path))

(in-package #:nodecode-file-lineage)

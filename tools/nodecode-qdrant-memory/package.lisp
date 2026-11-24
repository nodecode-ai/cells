;;;; package.lisp --- the QDRANT-MEMORY package: four tools, the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed QDRANT-MEMORY so an eval form reads
;;;; (qdrant-memory:search "where do I live"). The four tools are the same
;;;; names the original plugin's MCP server exposes; a person can call the same
;;;; functions from eval.

(defpackage #:nodecode-qdrant-memory
  (:use #:cl)
  (:nicknames #:qdrant-memory)
  ;; SEARCH and DELETE are COMMON-LISP's own; the cell's are its verbs.
  (:shadow #:search #:delete)
  (:export #:start-cell
           #:stop-cell
           ;; the verbs, every one a string
           #:search #:add #:update #:delete #:status
           ;; the one condition
           #:qdrant-memory-error #:qdrant-memory-error-detail))

(in-package #:nodecode-qdrant-memory)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last. FAIL and QDRANT-MEMORY-ERROR, which a request that cannot go on
;;; signals, come from here: every refusal starts as one of these, and the
;;; tools answer it as the call's failure rather than as an unhandled error.
(nlk:define-peripheral qdrant-memory :not-running t)

;;;; package.lisp --- the NODECODE-UMT package: three sources, eleven verbs.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed UMT so an eval form reads
;;;; (umt:search-stories "common lisp"): the lookups the Universal MCP
;;;; Toolkit ships as three zero-config MCP servers, as Lisp verbs. Every verb
;;;; answers a STRING a model or the operator reads, and every refusal is
;;;; UMT-ERROR, rendered as "ERROR: UMT-ERROR: <detail>".

(defpackage #:nodecode-umt
  (:use #:cl)
  (:nicknames #:umt)
  (:export
   ;; Hacker News
   #:top-stories #:new-stories #:best-stories #:search-stories #:item-thread
   ;; arXiv
   #:search-papers #:paper #:recent-papers
   ;; npm registry
   #:search-packages #:package-metadata #:package-versions
   ;; the one condition
   #:umt-error))

(in-package #:nodecode-umt)

;; FAIL and UMT-ERROR, RUNNING-SETTINGS (the settings, or the refusal a verb
;; reads while the cell is not running) and the verb macro DEFINE-VERB whose
;; body opens with it. The values themselves come from the NLE:DEFINE-CELL
;; declaration in cell.lisp, read with (SETTING :timeout).
(nlk:define-peripheral umt :not-running t)

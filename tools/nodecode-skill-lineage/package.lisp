;;;; package.lisp --- the SKILL-LINEAGE package: hashes in, hashes out.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Nicknamed LINEAGE so an eval form reads (lineage:record ...). Every verb
;;;; answers a string: the answer's text, or a refusal rendered as
;;;; "ERROR: SKILL-LINEAGE-ERROR: <detail>".
;;;;
;;;; TRACE is shadowed because this cell's trace is the provenance walk, not
;;;; the debugger's.

(defpackage #:nodecode-skill-lineage
  (:use #:cl)
  (:nicknames #:lineage)
  (:shadow #:trace)
  (:export
   ;; verbs, every one a string
   #:record #:trace #:descendants #:report #:verify #:head #:status
   ;; the one condition
   #:skill-lineage-error))

(in-package #:nodecode-skill-lineage)

(nlk:define-peripheral skill-lineage :not-running t :lock "lineage")

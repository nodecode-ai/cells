;;;; package.lisp --- the package, its one condition, the store's names.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of the holographic memory provider for Hermes Agent: the same
;;;; verbs over the same kind of local store, kept as one file of Lisp data
;;;; rather than a SQLite database. See NOTICE.

(defpackage #:nodecode-holographic
  (:nicknames #:holographic)
  (:use #:cl)
  (:export #:start-cell
           #:remember #:recall #:forget
           #:facts-text #:status-line
           #:holographic-error #:holographic-error-detail))

(in-package #:nodecode-holographic)

;; HOLOGRAPHIC-ERROR and its detail reader come from (NLK:DEFINE-PERIPHERAL
;; holographic), in cell.lisp: the cell's one refusal, which the tool answers
;; as ERROR: HOLOGRAPHIC-ERROR: ... and the gateway prints on the folder's line.

(defun refuse (format-string &rest arguments)
  "Signal HOLOGRAPHIC-ERROR with FORMAT-STRING applied to ARGUMENTS."
  (apply #'fail format-string arguments))

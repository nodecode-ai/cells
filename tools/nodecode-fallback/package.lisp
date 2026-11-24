;;;; package.lisp --- the cell's one package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The loader finds START-CELL as the exported symbol of the package named
;;;; after the system, and every contribution is filed under the same name
;;;; downcased, which is this folder's name. It is meant to be used qualified
;;;; from eval: (fallback:resolve ...), (fallback:apply-chain ...).

(defpackage #:nodecode-fallback
  (:use #:cl)
  (:nicknames #:fallback)
  (:export #:start-cell
           #:stop-cell
           #:fallback-errors
           #:fallback-error
           #:fallback-error-detail
           #:resolve
           #:apply-chain
           #:status
           #:providers
           #:*candidates*
           #:*catalog*
           #:*chain-reader*
           #:*chain-writer*))

(in-package #:nodecode-fallback)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last; facts.lisp and the verbs read the section through it. FAIL and
;;; FALLBACK-ERROR, which a verb that cannot answer signals, come from here.
(nlk:define-peripheral fallback :not-running t)

(declaim (ftype (function (keyword) t) setting))

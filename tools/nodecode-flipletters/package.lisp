;;;; package.lisp --- the package NLK:DEFINE-PERIPHERAL fills in.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Named after the system, which is the folder's name: the loader finds
;;;; START-CELL as this package's exported symbol. Only the condition is
;;;; exported, so a test names a refusal without the internals.

(defpackage #:nodecode-flipletters
  (:use #:cl)
  (:export #:flipletters-error #:flipletters-error-detail))

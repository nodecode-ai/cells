;;;; package.lisp --- the one package, named after the folder.
;;;;
;;;; SPDX-License-Identifier: MIT

(defpackage #:nodecode-epistemic
  (:use #:cl)
  (:export #:start-cell #:stop-cell
           #:case-request #:probe-request
           #:case-request-operation #:case-request-case-id #:probe-request-operation))

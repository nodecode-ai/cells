;;;; package.lisp --- the one package, and what the other files read.
;;;;
;;;; SPDX-License-Identifier: MIT

(defpackage #:nodecode-b64
  (:use #:cl)
  (:export #:b64-error #:b64-error-detail #:fail
           #:decoded #:decoded-label #:decoded-chars #:decoded-data #:decoded-notes
           #:clean-value #:decode-value #:describe-value #:expand-value
           #:kind-of #:text-of #:utf-8-octets #:base64-string
           #:note-push #:make-notes #:limits
           #:start-cell #:stop-cell #:run-decode #:run-slash))

;;;; package.lisp --- the relay's one package and its condition.
;;;;
;;;; SPDX-License-Identifier: MIT

(defpackage #:nodecode-relay
  (:use #:cl)
  (:export #:start-cell #:stop-cell #:relay
           #:relay-error #:relay-error-text
           #:relay-response #:start-listener #:stop-listener
           #:reaches-page-p))

(in-package #:nodecode-relay)

(nlk:define-peripheral relay :not-running t)

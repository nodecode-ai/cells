;;;; package.lisp --- the verbs a model reaches through eval, and the error.
;;;;
;;;; SPDX-License-Identifier: MIT

(defpackage #:nodecode-nerve
  (:nicknames #:nerve)
  (:use #:cl)
  (:export #:decide #:rank #:verify #:assess #:stats
           #:nerve-error #:nerve-error-text))

;;;; package.lisp --- the cell's package, its condition and what it exports.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of the bumblebee-guard plugin for Hermes Agent
;;;; (github.com/kahnanX/bumblebee-guard). See NOTICE.

(defpackage #:nodecode-bumblebee
  (:use #:cl)
  (:nicknames #:bumblebee)
  (:export #:scan #:status #:diff #:bumblebee-error #:bumblebee-error-detail))

(in-package #:nodecode-bumblebee)

(nlk:define-peripheral bumblebee :not-running t)

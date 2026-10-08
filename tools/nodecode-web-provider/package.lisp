;;;; package.lisp --- the NODECODE-WEB-PROVIDER package: the model's verb and its condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed ENGINES so an eval form reads
;;;; (engines:search "..."): not WEB, which the shipped websearch cell's
;;;; package answers to. It is meant to be used qualified only: SEARCH shadows
;;;; its CL namesake (the model's verb); inside this package, string searching
;;;; spells CL:SEARCH.

(defpackage #:nodecode-web-provider
  (:use #:cl)
  (:nicknames #:engines)
  (:shadow #:search)
  (:export #:search #:web-provider-error))

(in-package #:nodecode-web-provider)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last; engines.lisp reads the section through it. FAIL and
;;; WEB-PROVIDER-ERROR, which a search that cannot answer signals, and
;;; DEFINE-VERB, whose functions refuse while the cell is not running, come
;;; from here too. The SearXNG token never reaches the model.
(nlk:define-peripheral web-provider :not-running t :filter redact)

(declaim (ftype (function () list) secrets))

(defun redact (text)
  "TEXT with every SearXNG credential replaced by [redacted]."
  (uiop:frob-substrings text (ignore-errors (secrets)) "[redacted]"))

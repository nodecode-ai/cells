;;;; package.lisp --- the cell's package, and the refusal its verbs answer with.
;;;;
;;;; SPDX-License-Identifier: MIT

(defpackage #:nodecode-rea
  (:use #:cl)
  (:nicknames #:rea)
  (:export #:tools #:call #:schema #:status #:reconnect))

(in-package #:nodecode-rea)

;; FAIL, REA-ERROR, *REA*, RUNNING-SETTINGS and DEFINE-VERB; the last names the
;; condition a verb signals while the cell is not started.
(nlk:define-peripheral rea :not-running t :lock "rea")

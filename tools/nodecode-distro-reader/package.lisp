;;;; package.lisp --- the NODECODE-DISTRO-READER package: the cell's tools and its condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system, which is how the folder loader
;;;; finds START-CELL in it; DISTRO-READER is a nickname, the folder's own
;;;; name. SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp,
;;;; which loads last; wire.lisp and tools.lisp read the section through it.
;;;; FAIL and DISTRO-READER-ERROR come from NLK:DEFINE-PERIPHERAL: what a
;;;; tool signals when its call cannot be made.

(defpackage #:nodecode-distro-reader
  (:use #:cl)
  (:nicknames #:distro-reader)
  (:export #:distro-reader-error
           #:distro-reader-error-detail
           #:latest #:find-stories #:story #:series #:render-story #:book
           #:payment-info #:tip #:server-tools #:call-tool #:endpoint))

(in-package #:nodecode-distro-reader)

(nlk:define-peripheral distro-reader :not-running t)

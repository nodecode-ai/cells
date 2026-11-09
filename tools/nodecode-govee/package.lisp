;;;; package.lisp --- the NODECODE-GOVEE package: the cell's tool and its condition.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; The package is named after the system, which is how the folder loader
;;;; finds START-CELL in it; GOVEE is a nickname. SETTING comes from the
;;;; NLE:DEFINE-CELL declaration in cell.lisp, which loads last; wire.lisp and
;;;; lights.lisp read the section through it. FAIL and GOVEE-ERROR come from
;;;; NLK:DEFINE-PERIPHERAL: what a tool signals when its call cannot be made.

(defpackage #:nodecode-govee
  (:use #:cl)
  (:nicknames #:govee)
  (:export #:govee-error #:govee-error-detail))

(in-package #:nodecode-govee)

(nlk:define-peripheral govee :not-running t)

(defmacro answering (&body body)
  "BODY's answer; a refusal becomes that text, marked as the call's failure,
never an unhandled error up the stack."
  `(handler-case (progn ,@body)
     (govee-error (condition)
       (nle:failure (format nil "ERROR: ~a" (govee-error-detail condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: govee: ~a"
                            (nlk:one-line (princ-to-string condition)))))))

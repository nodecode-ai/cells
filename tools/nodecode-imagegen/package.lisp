;;;; package.lisp --- the IMAGEGEN package: one tool, one command, their condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed IMAGEGEN so an eval form reads
;;;; (imagegen:generate-image "a red fox"). The tool is what the model calls;
;;;; a person can call the same function from eval, or type /image.

(defpackage #:nodecode-imagegen
  (:use #:cl)
  (:nicknames #:imagegen)
  (:export #:start-cell
           #:stop-cell
           #:generate-image
           #:image-command
           #:imagegen-error
           #:imagegen-error-detail))

(in-package #:nodecode-imagegen)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last. FAIL and IMAGEGEN-ERROR, which a request that cannot go on
;;; signals, come from here: every refusal starts as one of these, and the
;;; tool answers it as the call's failure rather than as an unhandled error.
(nlk:define-peripheral imagegen :not-running t)
